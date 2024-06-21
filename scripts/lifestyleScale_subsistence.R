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
                                     'Fishing', 'Other Subsistence', 'Hunting and Gathering') ~ 1,
    main_subsistence_activity %in% c('Formal Employment') ~ 3,
    is.na(main_subsistence_activity) ~ NA_real_,
    TRUE ~ 2
  ))


# Orang Asli

# Recode rank columns
# currently rank is: 1=most, 2=second most, 3=least, 0=never
# recode ranks to: 0=never, 1=least, 2=second most, 3=most
data_orangAsli <- data_orangAsli %>% 
  mutate(wage_rank_recode = case_when(
    wage_rank == "1" ~ 3,
    wage_rank == "2" ~ 2,
    wage_rank == "3" ~ 1,
    wage_rank == "0" ~ 0,
    TRUE ~ NA_real_
  ),
  forage_rank_recode = case_when(
    forage_rank == "1" ~ 3,
    forage_rank == "2" ~ 2,
    forage_rank == "3" ~ 1,
    forage_rank == "0" ~ 0,
    TRUE ~ NA_real_
  ),
  crop_rank_recode = case_when(
    crop_rank == "1" ~ 3,
    crop_rank == "2" ~ 2,
    crop_rank == "3" ~ 1,
    crop_rank == "0" ~ 0,
    TRUE ~ NA_real_
  ),
  subsistence = ((case_when(
    # wage: higher rank = more urban/wage labor 
    wage_rank_recode == 3 ~ 3/3, 
    wage_rank_recode == 2 ~ 2/3,
    wage_rank_recode == 1 ~ 1/3,
    wage_rank_recode == 0 ~ 0/3,
    TRUE ~ NA_real_
  )) +
    (case_when(
      # crop: higher rank = more urban 
      crop_rank_recode == 3 ~ 3/3, 
      crop_rank_recode == 2 ~ 2/3,
      crop_rank_recode == 1 ~ 1/3,
      crop_rank_recode == 0 ~ 0/3,
      TRUE ~ NA_real_
    )) + 
    (case_when(
      # forage: higher rank = least urban, thus code as 0 (lower = less urban)
      forage_rank_recode == 3 ~ 0/3, 
      forage_rank_recode == 2 ~ 1/3,
      forage_rank_recode == 1 ~ 2/3,
      forage_rank_recode == 0 ~ 3/3,
      TRUE ~ NA_real_
    ))) +
    # frequency of hunting, fishing, or gathering in past week 
    (hunted_1wk/7) + 
    (fished_1wk/7) + 
    (gather_1wk/7) + 
    # Did you participate in wage labor in past month? Yes (1) / no (0) 
    wage_past_month
  )

