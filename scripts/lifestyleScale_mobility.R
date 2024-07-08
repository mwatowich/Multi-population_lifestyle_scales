### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate the mobility scale. Higher = more market access
# Goal: create a scale to measure access to markets and urban centers 


# Load libraries 
library(dplyr)


## Calculate mobility score

# Orang Asli 
data_orangAsli <- data_orangAsli %>% 
  mutate(town_2wk_new = case_when(town_2wk > 5 ~ 1, 
                                  town_2wk > 0 & town_2wk <= 5, 0.5, 
                                  town_2wk == 0, 0, 
                                  .default = NA
                                  ),
         kl_times_numeric = case_when(kl_times > 5 ~ 1, 
                                      kl_times > 0 & kl_times <= 5 ~ 0.5, 
                                      kl_times == 0 ~ 0, .default = NA)) %>% 
  rowwise() %>%
  mutate(mobility = (sum(c(town_2wk_new, kl_times_numeric, hh_item___moto, hh_item___car), na.rm = TRUE))/
           (sum(!is.na(c(town_2wk_new, kl_times_numeric, hh_item___moto, hh_item___car))))) %>%
  ungroup()
