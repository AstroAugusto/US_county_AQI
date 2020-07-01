## From website: https://urbaninstitute.github.io/urbnmapr/


#devtools::install_github("UrbanInstitute/urbnmapr", force = TRUE)
library(tidyverse)
library(urbnmapr)
#library(conflicted)
library(dplyr)
library(ggplot2)
library(purrr)
library(data.table)
library(tm)


#####################################READ CSV FILES #################################
 temp            <- list.files(pattern = "*county_20*") ## temp[1] = "daily_aqi_by_county_2000.csv"
 list_of_dfs     <- lapply(temp, read.csv)
 desired_columns <- c("State.Name", "county.Name", "AQI")
 ldf             <- lapply(list_of_dfs, "[", , desired_columns)
 
 ## Use "rbindlist" from package "data.table" to merge all dfs in ldf into one big df
 ## Kind of like "rbind" from base. The difference is that rbindlist returns a data table object
 bigdf <- as.data.frame(rbindlist(ldf, use.names = TRUE))
 
 ## Calculate mean AQI for each county in each state
 big_agg <- aggregate(bigdf$AQI, by = bigdf[c("State.Name", "county.Name")], FUN = mean)
 
 ## Rename mean AQI column
 names(big_agg)[names(big_agg) == 'x'] <- "mean.AQI" 
 
 ## Since the resulting df comes out with counties column 
 ## alphabetical order, instead make states in alph. order
 big_agg <- big_agg[order(big_agg$State.Name),] 
 
 ## Convert df to tibble with first two columns as character instead of factors
 big_agg %>% map_if(is.factor, as.character) %>% as_tibble -> big_agg_tib
 
 ## Sort counties alphabetically in each state
 big_agg_tib <- arrange(big_agg_tib, State.Name, county.Name )
 
 ## Get rid of rows that have Canada, Country of Mexico, and other unwanted regions
 big_agg_tib <- subset(big_agg_tib, State.Name != "Canada" & State.Name != "Country of Mexico" &
                         State.Name != "Puerto Rico" & State.Name != "Virgin Islands" &
                         county.Name != "BAJA CALIFORNIA NORTE" & county.Name != "CHIHUAHUA STATE" &
                         county.Name != "SONORA")
 
 ## Change column names to match those of household_data below
 colnames(big_agg_tib)    <- c("state_name", "county_name", "mean.AQI")
 
 ## Remove blank spaces around county names (the same is done for household_counties below)
 big_agg_tib$county_name  <- str_remove_all(big_agg_tib$county_name, " ")

# states_sf <- get_urbn_map("states", sf = TRUE)
# ggplot(states_sf, aes()) + geom_sf(fill = "grey", color = "#ffffff")

# counties_sf <- get_urbn_map("counties", sf = TRUE)
# ggplot(counties_sf, aes()) + geom_sf(fill = "grey", color = "#ffffff")

# spatial_data <- left_join(statedata,
#                           get_urbn_map(map = "states", sf = TRUE),
#                           by = "state_name")
# 
# ggplot() +
#   geom_sf(spatial_data,
#           mapping = aes(fill = horate, geometry = geometry),
#           color = "#ffffff", size = 0.25) +
#   labs(fill = "Homeownership rate")

########################################PROCESS URBAN INSTITUTE DATA ##########################
state         <- "Iowa"
counties_sf   <- get_urbn_map(map = "counties", sf = TRUE)
county_groups <- countydata %>% 
  mutate(cat_var = paste0("Group ",
                          sample(1:4, nrow(countydata), replace = TRUE)))

household_data <- left_join(county_groups, counties_sf, by = "county_fips")

##-----------------------Extract county_name column---------------------------------
##State-wide
#house_counties <- household_data[household_data$state_name == state,]$county_name

##Country-wide
house_counties <- household_data$county_name
##----------------------------------------------------------------------------------
#house_counties <- removeWords(house_counties, c("County", "Parish", "Census Area", "City and Borough",
#                                                "Borough", "Municipality", " "))

house_counties <- str_remove_all(house_counties, "County") ## Remove word "County" from each county name
house_counties <- str_remove_all(house_counties, "Parish")
house_counties <- str_remove_all(house_counties, "Census Area")
house_counties <- str_remove_all(house_counties, "City and Borough")
house_counties <- str_remove_all(house_counties, "Borough")
house_counties <- str_remove_all(house_counties, "Municipality")
house_counties <- str_remove_all(house_counties, "City")
house_counties <- str_remove_all(house_counties, " ")

##---- Repopulate counties column in household_data with the word "County" omitted----
## State-wide
#household_data[household_data$state_name == state,]$county_name <- house_counties

## Country-wide
household_data$county_name <- house_counties
##----------------------------------------------------------------------------------

##Count number of rows in household_data
nr <- nrow(household_data)

##Create vector of NAs to later store AQIs according to state and counties
AQI   <- replicate(nr, NA)


##------ The following is based on http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/ ------
household_data$source   <- "Source1" 
big_agg_tib$source      <- "Source2"

small_household_data <- household_data[,c("source","state_name", "county_name")]
hugedf <- rbind(small_household_data, big_agg_tib[,c("source", "state_name", "county_name")])

source("/Users/augusto_carballido/Desktop/Jobs/Project/dupsBetweenGroups.r")
dupRows     <- dupsBetweenGroups(hugedf, "source")
hugedf_dups <- cbind(hugedf, dup = dupRows)

new_household_data <- subset(hugedf_dups, source == "Source1", select = -source)
new_big_agg_tib    <- subset(hugedf_dups, source == "Source2", select = -source)

##-----------------------------------------------------------------------------------------------------------

###### MODIFY THIS TO INCLUDE ALL AVAILABLE AQI COUNTIES, NOT JUST STATE ONES ###############
##-------In which rows of household_data can we find the counties available from big_agg_tibb?-----------------
## State-wide
# avail_agg_counties <- big_agg_tib[big_agg_tib$State.Name == state,]$county.Name
# eff_counties       <- sort(house_counties[which(house_counties %in% avail_agg_counties)])
# row_index          <- which(household_data$state_name == state & household_data$county_name %in% eff_counties)

## Country-wide
##avail_agg_counties <- big_agg_tib$county.Name
##eff_counties       <- sort(house_counties[house_counties %in% avail_agg_counties])
##row_index          <- which(house_counties %in% avail_agg_counties)
row_index          <- which(new_household_data$dup == TRUE)

##---- Put available AQI values in AQI[row_index] vector ("pull" function makes the mean.AQI into a vector, otherwise mean.AQI remains a list)----
## State-wide
#AQI[row_index] <- pull(big_agg_tib[big_agg_tib$State.Name == state, "mean.AQI"]) 

## Country-wide
AQI[row_index] <- big_agg_tib$mean.AQI
##----------------------------------------------------------------------------------------------------------------------------------------------
AQI_cut <- cut(AQI, breaks = c(0, 50, 100, 500), include.lowest = T)

## Add AQI and AQI_cut vectors as new column to household_data
household_data <- household_data %>% add_column(AQI)
household_data <- household_data %>% add_column(AQI_cut)

household_data %>%
  #filter(state_name == state) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = AQI_cut, geometry = geometry),
          color = "gray20", size = 0.05) +
  coord_sf(datum = NA) +
#  scale_fill_continuous(low = "green", high = "orange", guide = guide_colorbar(nbin=10))
  scale_fill_manual(values = c("green", "yellow", "orange"), guide = guide_legend(reverse = TRUE)) +
  labs(fill = "AQI")

  #ggsave("US_county_AQI.png", device = "png")

