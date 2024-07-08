### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate the subsistence-type scale. Higher = more market integrated
# Goal: create a scale to measure market-integration (wage-labor, occupation) using subsistence strategy 


# Load libraries 
library(dplyr)


## Calculate subsistence scale 

# Turkana 
data_turkana <- data_turkana %>%
  mutate(subsistence = case_when(
    main_subsistence_activity %in% c('Animal Keeping', 'Herding', 'Gathering', 'Farming', 
                                     'Fishing', 'Other Subsistence', 'Hunting and Gathering', 
                                     'Hunting and Gathering, Animal Keeping') ~ 0,
    main_subsistence_activity %in% c('Formal Employment') ~ 1,
    is.na(main_subsistence_activity) ~ NA_real_,
    TRUE ~ 0.5
  ))


# Orang Asli
data_orangAsli <- data_orangAsli %>% 
  mutate(wage_rank_recode = case_when(
    wage_rank == "1" ~ 3/3, # 1 = most. Recode to 3/3: higher wage rank = more urban
    wage_rank == "2" ~ 2/3,
    wage_rank == "3" ~ 1/3,
    wage_rank == "0" ~ 0/3,
    TRUE ~ NA_real_),
    crop_rank_recode = case_when(
      crop_rank == "1" ~ 3/3, # 1 = most. Recode to 3/3: higher crop rank = more urban 
      crop_rank == "2" ~ 2/3,
      crop_rank == "3" ~ 1/3,
      crop_rank == "0" ~ 0/3,
      TRUE ~ NA_real_),
    forage_rank_recode = case_when(
      forage_rank == "1" ~ 0/3, # 1 = most reliance on foraging. Recode to 0/3: higher forage rank = least urban
      forage_rank == "2" ~ 1/3,
      forage_rank == "3" ~ 2/3,
      forage_rank == "0" ~ 3/3,
      TRUE ~ NA_real_)) %>% 
  rowwise() %>%
  mutate(subsistence = (sum(c(wage_rank_recode, crop_rank_recode, forage_rank_recode, 
                              1-hunted_1wk/7, 1-fished_1wk/7, 1-gather_1wk/7, wage_past_month), na.rm = TRUE))/
           (sum(!is.na(c(wage_rank_recode, crop_rank_recode, forage_rank_recode, 
                         hunted_1wk, fished_1wk, gather_1wk, wage_past_month))))) %>%
  ungroup()

data_orangAsli[is.na(data_orangAsli$subsistence),]$subsistence <- NA
