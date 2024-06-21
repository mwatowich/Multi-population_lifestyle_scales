### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate the traditional diet score. Higher = more traditional 
## Goal: create a scale to measure the extent of consumption of foods traditionally eaten by a subsistence-level society. Many items are specific to the Turkana of Kenya and Orang Asli of Malaysia and specific items will need to be adapted for other societies.
# Note that in the manuscript Watowich et al. (in prep), we reverse the coding of these items such that a higher score is indicative of a less traditional diet. 

# Load libraries 
library(dplyr)


## Calculate score of traditional diet 

# Turkana 
data_turkana <- data_turkana %>% 
  mutate(blood_score = case_when(
    blood_consumption == "Never" ~ 0/4,
    blood_consumption == "Rarely" ~ 1/4,
    blood_consumption %in% c("1-2 times per week", "2 times per week") ~ 2/4,
    blood_consumption == ">2 times per week" ~ 3/4,
    blood_consumption %in% c("Everyday") ~ 4/4,
    TRUE ~ NA_real_
  ),
  milk_score = case_when(
    how_often_do_you_drink_milk == "Never" ~ 0/4,
    how_often_do_you_drink_milk == "Rarely" ~ 1/4,
    how_often_do_you_drink_milk == "1-2 times per week" ~ 2/4,
    how_often_do_you_drink_milk == "More than 2 times per week" ~ 3/4,
    how_often_do_you_drink_milk %in% c("Everyday", "More than 2 times per day") ~ 4/4,
    TRUE ~ NA_real_
  ),
  milk_ferm_score = case_when(
    is_the_milk_fresh_or_fermented %in% c("Fermented", "Both") ~ 1/1,
    is_the_milk_fresh_or_fermented == "Fresh" ~ 0/1,
    TRUE ~ NA_real_
  )) %>% 
  rowwise() %>%
  mutate(traditional_diet_score = (sum(c(blood_score, milk_score, milk_ferm_score), na.rm = TRUE))/
           (sum(!is.na(c(blood_score, milk_score, milk_ferm_score))))) %>%
  ungroup()
data_turkana[is.na(data_turkana$traditional_diet_score),]$traditional_diet_score <- NA


# Orang Asli
data_orangAsli <- data_orangAsli %>% 
  mutate(meat_wild_scaled = case_when(
    meat_wild == 3 ~ 3/3, # 3 = more wild meat in diet 
    meat_wild == 2 ~ 2/3,
    meat_wild == 1 ~ 1/3,
    meat_wild == 0 ~ 0/3,
    TRUE ~ NA_real_
  ),
  fish_wild_scaled = case_when(
    fish_wild == 3 ~ 3/3,
    fish_wild == 2 ~ 2/3,
    fish_wild == 1 ~ 1/3,
    fish_wild == 0 ~ 0/3,
    TRUE ~ NA_real_
  ), 
  manioc_scaled = case_when(
    manioc == 3 ~ 3/3,
    manioc == 2 ~ 2/3,
    manioc == 1 ~ 1/3,
    manioc == 0 ~ 0/3,
    TRUE ~ NA_real_
  ), 
  rice_scaled = case_when(
    rice == 3 ~ 3/3,
    rice == 2 ~ 2/3,
    rice == 1 ~ 1/3,
    rice == 0 ~ 0/3,
    TRUE ~ NA_real_
  )) %>% 
  rowwise() %>%
  mutate(traditional_diet_score = (sum(c(meat_wild_scaled, fish_wild_scaled, manioc_scaled, rice_scaled), na.rm = TRUE))/
           (sum(!is.na(c(meat_wild_scaled, fish_wild_scaled, manioc_scaled, rice_scaled))))) %>%
  ungroup()
