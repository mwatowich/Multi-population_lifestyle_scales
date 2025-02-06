### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate the household style of life index. Higher = more urban. 
## Based on Gildner et al. 2020, "Market integration and soil-transmitted helminth infection among the Shuar of Amazonian Ecuador" 


# Load libraries 
library(dplyr)


## Calculate H-Sol 

# Turkana 
data_turkana <- data_turkana %>% 
  mutate(number_of_rooms_in_the_household_num = case_when(number_of_rooms_in_the_household>1~1, 
                                                          number_of_rooms_in_the_household<=1~0,
                                                          .default = NA), 
         presence_of_a_finished_floor_num = case_when(presence_of_a_finished_floor=="Yes"~1,
                                                      presence_of_a_finished_floor=="No"~0,
                                                      .default = NA), 
         presence_of_an_iron_concrete_or_slate_roof_num = case_when(presence_of_an_iron_concrete_or_slate_roof=="Yes"~1,
                                                                    presence_of_an_iron_concrete_or_slate_roof=="No"~0,
                                                                    .default = NA), 
         presence_of_electricity_num = case_when(presence_of_electricity=="Yes"~1,
                                                 presence_of_electricity=="No"~0,
                                                 .default = NA), 
         presence_of_flush_toilet_num = case_when(presence_of_flush_toilet=="Yes"~1,
                                                  presence_of_flush_toilet=="No"~0,
                                                  .default = NA), 
         does_the_household_have_indoor_tap_water_num = case_when(does_the_household_have_indoor_tap_water=="Yes"~1,
                                                                  does_the_household_have_indoor_tap_water=="No"~0,
                                                                  .default = NA)) %>% 
  rowwise() %>%
  mutate(h_sol = (sum(c(number_of_rooms_in_the_household_num, presence_of_a_finished_floor_num,
                        presence_of_an_iron_concrete_or_slate_roof_num,
                        presence_of_electricity_num, presence_of_flush_toilet_num,
                        does_the_household_have_indoor_tap_water_num), na.rm = TRUE))/
           (sum(!is.na(c(number_of_rooms_in_the_household_num, presence_of_a_finished_floor_num,
                         presence_of_an_iron_concrete_or_slate_roof_num,
                         presence_of_electricity_num, presence_of_flush_toilet_num,
                         does_the_household_have_indoor_tap_water_num))))) %>%
  ungroup()
data_turkana[data_turkana$h_sol == "NaN",]$h_sol <- NA


# Orang Asli 
data_orangAsli$water_source___gov_tap <- 0
data_orangAsli[which(data_orangAsli$water_source___filter==0 & 
                     data_orangAsli$water_source___river_lake==0 & 
                     data_orangAsli$water_source___well==0),]$water_source___gov_tap <- 1

data_orangAsli <- data_orangAsli %>% 
  mutate(highest_where_poop = case_when(
    where_poop___toilet == 1 ~ 1,
    where_poop___latrine == 1 ~ 0.5,
    where_poop___field == 1 ~ 0,
    where_poop___forest == 1 ~ 0,
    where_poop___river_lake == 1 ~ 0,
    .default = NA
  ),
  house_type_score = case_when(
    house_type == "concrete" ~ 1,
    house_type == "wood" ~ 1,
    house_type == "traditional" ~ 0,
    .default = NA
  ),
  water_source_score = case_when(
    water_source___filter == 1 ~ 1,
    water_source___well == 1 ~ 1,
    water_source___gov_tap == 1 ~ 1,
    water_source___river_lake == 1 ~ 0,
    .default = NA
  ))

data_orangAsli <- data_orangAsli %>% 
  rowwise() %>%
  mutate(h_sol = (sum(c(house_type_score, house_type_score,
                        water_source_score, highest_where_poop,
                        electricity_resid), na.rm = TRUE))/
           (sum(!is.na(c(house_type_score, house_type_score,
                         water_source_score, highest_where_poop,
                         electricity_resid))))) %>%
  ungroup()
# Note that house type score is multiplied by 2. Done because Turkana H-SOL includes 2 housing variables 
