### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate the household style of life index. Higher = higher consumption of market-derived foods 
## Goal: create a scale to measure the extent of market-derived foods consumed. Specifically, sugar, salt, and processed cooking oil 

# Load libraries 
library(dplyr)


## Calculate market-derived diet score 

# Turkana 
data_turkana <- data_turkana %>% 
  mutate(salt_score = case_when(
    does_your_household_use_salt == "Never" ~ 0/4,
    does_your_household_use_salt == "Rarely" ~ 1/4,
    does_your_household_use_salt %in% c("1-2 times per week") ~ 2/4,
    does_your_household_use_salt %in% c("More than 2 times per week") ~ 3/4,
    does_your_household_use_salt %in% c("Everyday") ~ 4/4,
    TRUE ~ NA_real_
  ),
  sugar_score = case_when(
    does_your_household_use_sugar == "Never" ~ 0/4,
    does_your_household_use_sugar == "Rarely" ~ 1/4,
    does_your_household_use_sugar %in% c("1-2 times per week") ~ 2/4,
    does_your_household_use_sugar %in% c("More than 2 times per week") ~ 3/4,
    does_your_household_use_sugar %in% c("Everyday") ~ 4/4,
    TRUE ~ NA_real_
  ),
  oil_score = case_when(
    does_your_household_use_cooking_oil == "Never" ~ 0/4,
    does_your_household_use_cooking_oil == "Rarely" ~ 1/4,
    does_your_household_use_cooking_oil %in% c("1-2 times per week") ~ 2/4,
    does_your_household_use_cooking_oil %in% c("More than 2 times per week") ~ 3/4,
    does_your_household_use_cooking_oil %in% c("Everyday") ~ 4/4,
    TRUE ~ NA_real_
  )) %>% 
  rowwise() %>%
  mutate(market_diet_index = (sum(c(salt_score, sugar_score, oil_score), na.rm = TRUE)) / 
           (sum(!is.na(c(salt_score, sugar_score, oil_score))))) %>% 
  ungroup()

data_turkana[is.na(data_turkana$market_diet_index),]$market_diet_index <- NA


# Orang Asli
data_orangAsli <- data_orangAsli %>% 
  mutate(salt_score = case_when(
    salt == 3 ~ 3/3,
    salt == 2 ~ 2/3,
    salt == 1 ~ 1/3,
    salt == 0 ~ 0/3,
    TRUE ~ NA_real_
  ), 
  sugar_score = case_when(
    sugar == 3 ~ 3/3,
    sugar == 2 ~ 2/3,
    sugar == 1 ~ 1/3,
    sugar == 0 ~ 0/3,
    TRUE ~ NA_real_
  ), 
  oil_score = case_when(
    oil == 3 ~ 3/3,
    oil == 2 ~ 2/3,
    oil == 1 ~ 1/3,
    oil == 0 ~ 0/3,
    TRUE ~ NA_real_
  )) %>% 
  rowwise() %>%
  mutate(market_diet_index = (sum(c(salt_score, sugar_score, oil_score), na.rm = TRUE)) /
           (sum(!is.na(c(salt_score, sugar_score, oil_score))))) %>%
  ungroup()
