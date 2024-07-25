### Sensitivity analyses of top 3 lifestyle scales 

library(dplyr)
library(ggplot2)

# Code shows Turkana data, same methods used for Orang Asli data 
# Run script 01_lifestyleScales_healthOutcomes.R first to generate objects with health outcomes and lifestyle scales

## Sensitivity analyses of H-SOL scale -------------------

# Make combinations of items in h_sol 
hsol_items <- c("number_of_rooms_in_the_household_score",
                "presence_of_a_finished_floor_score", 
                "presence_of_an_iron_concrete_or_slate_roof_score", 
                "presence_of_electricity_score", 
                "presence_of_flush_toilet_score", 
                "does_the_household_have_indoor_tap_water_score")
hsol_comb <- combn(x=c(hsol_items), m = 5)

# Make h_sol sums of each combination of 5 items 
h_sol_sensitivity_scores <- do.call(cbind, lapply(1:6, function(x) {
  # make hsol score for 5 items in each combination 
  TMP <- turkana[,c(hsol_comb[,x])]
  TMP$hsol_tmp <- TMP[[1]] + TMP[[2]] + TMP[[3]] + TMP[[4]] + TMP[[5]]
  # name it for which is missing
  colnames(TMP)[6] <- paste0("hsol_no_", c(hsol_items)[!hsol_items %in% hsol_comb[,x]])
  return(TMP[,6])
}))

# Add original h_sol and cardiometabolic outcomes 
h_sol_sensitivity_scores <- data.frame(h_sol_sensitivity_scores, 
                                       turkana[,c("h_sol",turkana_outcomes,"age","gender")])

# Model each sensitivity analysis vs full H_SOL 
h_sol_sensitivity_out <- do.call(rbind, lapply(paste0("hsol_no_",hsol_items), function(x){
  do.call(rbind, lapply(turkana_outcomes[!turkana_outcomes %in% c("obesity_overweight","hypertension")], function(y){
    # get same dataset 
    tmp <- h_sol_sensitivity_scores[which(complete.cases(h_sol_sensitivity_scores$h_sol) & 
                                            complete.cases(h_sol_sensitivity_scores[[x]])),]
    # model comparison 
    r2_main <- summary(lm(tmp[[y]] ~ age + gender + h_sol, tmp))$r.squared
    r2_new <- summary(lm(tmp[[y]] ~ age + gender + tmp[[x]], tmp))$r.squared
    return(data.frame(r2_main = r2_main, 
                      r2_new = r2_new,
                      outcome = y, 
                      leave_out = x, 
                      metric = "H-SOL", 
                      r2_diff = r2_main-r2_new,
                      r2_diff_percent = ((r2_main-r2_new)/r2_main)))
  }))
}))


## Sensitivity analyses of material wealth scale -------------------
# Make combinations of items in material wealth scale 
material_wealth_items <- c("presence_of_a_finished_floor_score",
                           "presence_of_an_iron_concrete_or_slate_roof_score", 
                           "presence_of_electricity_score",
                           "presence_of_television_set_score",  
                           "presence_of_mobile_phone_score", 
                           "presence_of_flush_toilet_score", 
                           "does_the_household_cook_with_gas_score",
                           "does_the_household_have_indoor_tap_water_score",
                           "does_the_household_drink_treated_or_boiled_water_score",
                           "number_of_rooms_in_the_household_score",
                           "ppl_per_room_score")
material_wealth_comb <- combn(x=c(material_wealth_items), m = length(material_wealth_items)-1)

# Make sums of each combination of items 
material_wealth_sensitivity_scores <- do.call(cbind, lapply(1:ncol(material_wealth_comb), function(x) {
  # Make material wealth score 
  TMP <- turkana[,c(material_wealth_comb[,x])]
  TMP <- TMP %>% 
    rowwise() %>% 
    mutate(material_wealth = (sum(c_across(everything()), na.rm = TRUE))/
             (sum(!is.na(c_across(everything()))))) %>%
    ungroup()
  TMP[is.na(TMP$material_wealth),]$material_wealth <- NA
  colnames(TMP)[11] <- paste0("MW_no_", c(material_wealth_items)[!material_wealth_items %in% material_wealth_comb[,x]])
  return(TMP[,11])
}))

# Add original material wealth and cardiometabolic outcomes 
material_wealth_sensitivity_scores <- data.frame(material_wealth_sensitivity_scores, 
                                                 turkana[,c("material_wealth",turkana_outcomes,"age","gender")])

# Model each sensitivity analysis vs full material wealth 
material_wealth_sensitivity_out <- do.call(rbind, lapply(paste0("MW_no_",material_wealth_items), function(x){
  do.call(rbind, lapply(turkana_outcomes[!turkana_outcomes %in% c("obesity_overweight","hypertension")], function(y){
    tmp <- material_wealth_sensitivity_scores[which(complete.cases(material_wealth_sensitivity_scores$material_wealth) & 
                                                      complete.cases(material_wealth_sensitivity_scores[[x]])),]
    r2_main <- summary(lm(tmp[[y]] ~ age + gender + material_wealth, tmp))$r.squared
    r2_new <- summary(lm(tmp[[y]] ~ age + gender + tmp[[x]], tmp))$r.squared
    return(data.frame(r2_main = r2_main, 
                      r2_new = r2_new,
                      outcome = y, 
                      leave_out = x, 
                      metric = "Material wealth", 
                      r2_diff = r2_main-r2_new,
                      r2_diff_percent = ((r2_main-r2_new)/r2_main)))
  })) 
})) 


## Sensitivity analyses of location-based urbanicity scale ------------
# Make urb_score combinations 
urban5$MW_no_pop_density <- #urban5$pop_cat + 
  (10-(10*urban5$prop_not_wage)) + 
  (5*urban5$prop_electricity) + 
  (5*urban5$prop_toilet) + (5*urban5$prop_tv) + 
  (5*urban5$prop_phone) + 
  10*urban5$prop_40plus_ed + 
  10*urban5$prop_40less_ed

urban5$MW_no_prop_wage <- urban5$pop_cat + 
  #(10-(10*urban5$prop_not_wage)) + 
  (5*urban5$prop_electricity) + 
  (5*urban5$prop_toilet) + (5*urban5$prop_tv) + 
  (5*urban5$prop_phone) + 
  10*urban5$prop_40plus_ed + 
  10*urban5$prop_40less_ed

urban5$MW_no_prop_electricity <- urban5$pop_cat + 
  (10-(10*urban5$prop_not_wage)) + 
  #(5*urban5$prop_electricity) + 
  (5*urban5$prop_toilet) + (5*urban5$prop_tv) + 
  (5*urban5$prop_phone) + 
  10*urban5$prop_40plus_ed + 
  10*urban5$prop_40less_ed

urban5$MW_no_prop_flush_toilet <- urban5$pop_cat + 
  (10-(10*urban5$prop_not_wage)) + 
  (5*urban5$prop_electricity) + 
  #(5*urban5$prop_toilet) + 
  (5*urban5$prop_tv) + 
  (5*urban5$prop_phone) + 
  10*urban5$prop_40plus_ed + 
  10*urban5$prop_40less_ed

urban5$MW_no_prop_TV <- urban5$pop_cat + 
  (10-(10*urban5$prop_not_wage)) + 
  (5*urban5$prop_electricity) + 
  (5*urban5$prop_toilet) + #(5*urban5$prop_tv) + 
  (5*urban5$prop_phone) + 
  10*urban5$prop_40plus_ed + 
  10*urban5$prop_40less_ed

urban5$MW_no_prop_mobile_phone <- urban5$pop_cat + 
  (10-(10*urban5$prop_not_wage)) + 
  (5*urban5$prop_electricity) + 
  (5*urban5$prop_toilet) + (5*urban5$prop_tv) + 
  #(5*urban5$prop_phone) + 
  10*urban5$prop_40plus_ed + 
  10*urban5$prop_40less_ed

urban5$MW_no_prop_40plus_ed <- urban5$pop_cat + 
  (10-(10*urban5$prop_not_wage)) + 
  (5*urban5$prop_electricity) + 
  (5*urban5$prop_toilet) + (5*urban5$prop_tv) + 
  (5*urban5$prop_phone) + 
  #10*urban5$prop_40plus_ed + 
  10*urban5$prop_40less_ed

urban5$MW_no_prop_40less_ed <- urban5$pop_cat + 
  (10-(10*urban5$prop_not_wage)) + 
  (5*urban5$prop_electricity) + 
  (5*urban5$prop_toilet) + (5*urban5$prop_tv) + 
  (5*urban5$prop_phone) + 
  10*urban5$prop_40plus_ed
#10*urban5$prop_40less_ed

# Bind new columns 
turkana <- left_join(turkana, 
                     urban5[,c("Sampling_location",colnames(urban5)[grep("_no_",colnames(urban5))])], 
                     by = c("sampling_location" = "Sampling_location"))

# Model each sensitivity analysis vs full urbanicity score metric 
urb_score_sensitivity_out <- do.call(rbind, lapply(colnames(turkana)[grep("no_",colnames(turkana))], function(x){
  do.call(rbind, lapply(turkana_outcomes[!turkana_outcomes %in% c("obesity_overweight","hypertension")], function(y){
    # get same dataset
    tmp <- turkana[which(complete.cases(turkana$urb_score) & complete.cases(turkana[[x]])),]
    # model comparison
    r2_main <- summary(lm(scale(tmp[[y]]) ~ age + gender + scale(urb_score), tmp))$r.squared
    r2_new <- summary(lm(scale(tmp[[y]]) ~ age + gender + scale(tmp[[x]]), tmp))$r.squared
    return(data.frame(r2_main = r2_main,
                      r2_new = r2_new,
                      outcome = y,
                      leave_out = x,
                      metric = "Urbanicity score",
                      r2_diff = r2_main-r2_new,
                      r2_diff_percent = ((r2_main-r2_new)/r2_main)))
  }))
}))
#
