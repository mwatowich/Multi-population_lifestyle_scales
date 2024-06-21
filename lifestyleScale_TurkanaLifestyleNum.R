### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate numeric lifestyle categorization in Turkana. Higher = more urban. 
## Based on Lea et al. 2020, "Urbanization and market integration have strong, nonlinear effects on cardiometabolic health in the Turkana" 

# Load libraries 
library(dplyr)


## Make lifestyle category
data_turkana <- data_turkana %>% 
  mutate(location_simple = case_when(latitude>1.4 ~ "rural",
                                     latitude<1.4 ~ "urban",
                                     .default = NA), 
         
         lifestyle = case_when(
           # Assign Pastoralist
           main_subsistence_activity == "Animal Keeping" & how_often_do_you_drink_milk == "Everyday" & 
             location_simple == "rural" & sampling_location != "Lodwar" ~ "Pastoralist",
           # Assign Urban 
           location_simple != "rural" | sampling_location == "Lodwar" ~ "Urban",
           # Assign Peri-urban
           how_often_do_you_drink_milk != "Everyday" & location_simple == "rural" & 
             sampling_location != "Lodwar" ~ "Peri-urban",
           .default = NA),
         
         lifestyle_numeric = case_when(lifestyle == "Pastoralist" ~ 0, 
                                       lifestyle == "Peri-urban" ~ 1,
                                       lifestyle == "Urban" ~ 2))
