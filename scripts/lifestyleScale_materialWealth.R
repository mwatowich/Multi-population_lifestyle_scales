### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate the household style of life index. Higher = higher wealth 
## Goal: create a scale to measure material wealth. Specifically aims to capture material wealth in 'Western' or industrialized contexts. Material wealth estimated via housing materials, possessions. 


# Load libraries 
library(dplyr)


## Calculate material wealth score

# Turkana
data_turkana <- data_turkana %>% 
  mutate(presence_of_a_finished_floor_score = ifelse(presence_of_a_finished_floor=="Yes",1,0), 
         presence_of_an_iron_concrete_or_slate_roof_score = ifelse(presence_of_an_iron_concrete_or_slate_roof=="Yes",1,0), 
         presence_of_electricity_score = ifelse(presence_of_electricity=="Yes",1,0), 
         presence_of_television_set_score = ifelse(presence_of_television_set=="Yes",1,0), 
         presence_of_mobile_phone_score = ifelse(presence_of_mobile_phone=="Yes",1,0), 
         presence_of_flush_toilet_score = ifelse(presence_of_flush_toilet=="Yes",1,0), 
         does_the_household_cook_with_gas_score = ifelse(does_the_household_cook_with_gas=="Yes",1,0), 
         does_the_household_have_indoor_tap_water_score = ifelse(does_the_household_have_indoor_tap_water=="Yes",1,0), 
         does_the_household_drink_treated_or_boiled_water_score = ifelse(does_the_household_drink_treated_or_boiled_water=="Yes",1,0), 
         number_of_rooms_in_the_household_score = ifelse(number_of_rooms_in_the_household>1, 1, 0), 
         ppl_per_room_score = ifelse((number_of_household_members/number_of_rooms_in_the_household)<=2,1,0)) %>% 
  rowwise() %>%
  mutate(material_wealth = (sum(c(presence_of_a_finished_floor_score, 
                                  presence_of_an_iron_concrete_or_slate_roof_score, 
                                  presence_of_electricity_score, 
                                  presence_of_television_set_score, 
                                  presence_of_mobile_phone_score, 
                                  presence_of_flush_toilet_score, 
                                  does_the_household_cook_with_gas_score, 
                                  does_the_household_have_indoor_tap_water_score, 
                                  does_the_household_drink_treated_or_boiled_water_score, 
                                  number_of_rooms_in_the_household_score, 
                                  ppl_per_room_score), na.rm = TRUE))/
           (sum(!is.na(c(presence_of_a_finished_floor_score, 
                         presence_of_an_iron_concrete_or_slate_roof_score, 
                         presence_of_electricity_score, 
                         presence_of_television_set_score, 
                         presence_of_mobile_phone_score, 
                         presence_of_flush_toilet_score, 
                         does_the_household_cook_with_gas_score, 
                         does_the_household_have_indoor_tap_water_score, 
                         does_the_household_drink_treated_or_boiled_water_score, 
                         number_of_rooms_in_the_household_score, 
                         ppl_per_room_score))))) %>%
  ungroup()
data_turkana[is.na(data_turkana$material_wealth),]$material_wealth <- NA


# Orang Asli 
data_orangAsli <- data_orangAsli %>% 
  rowwise() %>% 
  mutate(material_wealth = sum(c(house_type_score, water_source_score, electricity_resid, 
                                 hh_item___moto, hh_item___tv, hh_item___car, hh_item___smart_phone, 
                                 hh_item___flip_phone, hh_item___chainsaw, hh_item___weedwacker, 
                                 hh_item___generator, hh_item___gas_stove), na.rm = TRUE)/
           sum(!is.na(c(house_type_score, water_source_score, electricity_resid, 
                        hh_item___moto, hh_item___tv, hh_item___car, hh_item___smart_phone, 
                        hh_item___flip_phone, hh_item___chainsaw, hh_item___weedwacker, 
                        hh_item___generator, hh_item___gas_stove)))) %>%
  ungroup()
