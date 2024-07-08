### Multi-population continuous lifestyle scales from Watowich et al. 2024, in prep

## Code to generate the acculturation score. Higher = more acculturated  
# Goal: create a scale to measure acculturation to the majority culture in each country. Many items are specific to the Turkana of Kenya and Orang Asli of Malaysia, but the items (highest education attainment, languages spoken, religion) can be adapted to other contexts. 


# Load libraries 
library(dplyr)


## Calculate acculturation scale 

# Turkana 
data_turkana <- data_turkana %>% 
  mutate(highest_education_level_numeric = case_when(
    grepl("None", highest_education_level) ~ 0,
    grepl("Primary|primary", highest_education_level) ~ 1/3,
    grepl("Secondary|secondary", highest_education_level) ~ 2/3,
    grepl("Tertiary|tertiary", highest_education_level) ~ 3/3,
    .default = NA
  ), 
  # Languages the participant speaks 
  language_score = case_when(
    grepl("English", languages_you_speak) ~ 2/2,
    grepl("Swahili", languages_you_speak) ~ 1/2,
    grepl("Turkana", languages_you_speak) ~ 0,
    .default = NA
  )) %>% 
  rowwise() %>% 
  # Calculate score on variables with complete data 
  mutate(acculturation_score = (sum(c(highest_education_level_num, language_score), na.rm = TRUE))/
           (sum(!is.na(c(highest_education_level_num, language_score))))) %>%
  ungroup()

data_turkana[is.na(data_turkana$acculturation_score),]$acculturation_score <- NA


# Orang Asli 
data_orangAsli <- data_orangAsli %>% 
  mutate(highest_education_stage_num = case_when(
    highest_education_stage == 3 ~ 3/3,
    highest_education_stage == 2 ~ 2/3,
    highest_education_stage == 1 ~ 2/3,
    highest_education_stage == 0 ~ 0/3,
    .default = NA
  ), 
  # Type of religion participant primarily practices 
  religion_num = case_when(
    religion___islam == 1 | religion___bahai == 1 | religion___budd == 1 | religion___christ == 1 | religion___hindu == 1 ~ 1,
    religion___oa == 1 ~ 0,
    .default = NA
  ), 
  # If given the choice between these, which meat would you choose? 
  choose_meat_score = case_when(
    choose_meat == "beef" ~ 1,
    choose_meat == "monkey" ~ 0,
    .default = NA
  )) %>% 
  rowwise() %>%
  mutate(acculturation_score = (sum(c(highest_education_stage_num, religion_num, mysia_pres_yn, language/3, choose_meat_score), na.rm = TRUE))/
           (sum(!is.na(c(highest_education_stage_num, religion_num, mysia_pres_yn, language, choose_meat_score))))) %>%
  ungroup()
