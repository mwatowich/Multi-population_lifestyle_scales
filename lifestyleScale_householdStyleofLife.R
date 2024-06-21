### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate the household style of life index. Higher = more urban. 
## Based on Gildner et al. 2020, "Market integration and soil-transmitted helminth infection among the Shuar of Amazonian Ecuador" 


# Load libraries 
library(dplyr)


## Calculate H-Sol 

# Turkana 
data_turkana$h_sol<-ifelse(data_turkana$number_of_rooms_in_the_household>1, 1, 0) + 
  ifelse(data_turkana$presence_of_a_finished_floor=="Yes",1,0) + 
  ifelse(data_turkana$presence_of_an_iron_concrete_or_slate_roof=="Yes",1,0) + 
  ifelse(data_turkana$presence_of_electricity=="Yes",1,0) + 
  ifelse(data_turkana$presence_of_flush_toilet=="Yes",1,0) + 
  ifelse(data_turkana$does_the_household_have_indoor_tap_water=="Yes",1,0)

# Orang Asli 
data_orangAsli %>% 
  mutate(highest_where_poop = case_when(
    where_poop___toilet == 1 ~ 1,
    where_poop___latrine == 1 ~ 0.5,
    where_poop___field == 1 ~ 0,
    where_poop___forest == 1 ~ 0,
    where_poop___river_lake == 1 ~ 0,
    TRUE ~ NA_real_
  ),
  house_type_score = case_when(
    house_type == "concrete" ~ 1,
    house_type == "wood" ~ 1,
    house_type == "traditional" ~ 0,
    TRUE ~ NA_real_
  ),
  water_source_score = case_when(
    water_source___filter == 1 ~ 1,
    water_source___well == 1 ~ 1,
    water_source___gov_tap == 1 ~ 1,
    water_source___river_lake == 1 ~ 0,
    TRUE ~ NA_real_
  ))

data_orangAsli$h_sol <- as.numeric(data_orangAsli$house_type_score)*2 + 
  as.numeric(data_orangAsli$water_source_score) + 
  as.numeric(data_orangAsli$highest_where_poop) + 
  as.numeric(data_orangAsli$electricity_resid)
# Note that house type score is multiplied by 2. Done because Turkana H-SOL includes 2 housing variables 
