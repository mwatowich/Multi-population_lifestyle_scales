### Piecewise regression 

library(dplyr)
library(ggplot2)

# Run script 01_lifestyleScales_healthOutcomes.R first to generate objects with health outcomes and lifestyle scales


## Model scales as linear vs polynomial. For those better predicted by a polynomial trend, perform piecewise analysis
lin_vs_poly <- as.data.frame(do.call(rbind, lapply(turkana_outcomes_cont, function(x){
  do.call(rbind, lapply(turkana_metrics[!turkana_metrics %in% c("lifestyle_num","subsistence")], function(y) {
    df <- turkana[which(complete.cases(turkana$age) & 
                          complete.cases(turkana$gender) & 
                          complete.cases(turkana[[x]]) & 
                          complete.cases(turkana[[y]])),]
    
    # Run linear additive model
    out_lin <- lm(scale(df[[x]]) ~ age + gender + scale(df[[y]]), df)
    out_lin <- data.frame(AIC = AIC(out_lin), 
                          n = nrow(df[complete.cases(df[[x]]),]), 
                          metric = y, 
                          outcome = x, 
                          type = "linear")
    
    # Run quadratic additive model
    out_poly <- lm(scale(df[[x]]) ~ age + gender + scale(poly(df[[y]],2)), df)
    out_poly <- data.frame(AIC = AIC(out_poly),
                           n = nrow(df[complete.cases(df[[x]]),]), 
                           metric = y, 
                           outcome = x, 
                           type = "polynomial",
                           pval_metricQuad = summary(out_poly)$coefficients[5,4])
    
    # Combine 
    out <- dplyr::bind_rows(out_poly, out_lin)
    out$linear_AIC <- out[out$type == "linear",]$AIC
    return(out)
  }))
})))

# Get list of scores/outcomes that are better as quadratic 
priority_order <- c("linear","quadratic")
lin_vs_poly_best <- lin_vs_poly %>% 
  group_by(outcome,metric) %>% 
  filter(!(type == "quadratic" & pval_metricQuad>0.05)) %>%
  filter(abs(AIC - min(AIC)) < 2) %>%
  arrange(match(type, priority_order)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>% 
  as.data.frame()

# Combos to run peicewise analysis for 
quad_mods <- lin_vs_poly_best[which(lin_vs_poly_best$type=="polynomial"),]
combos <- quad_mods[,c("metric","outcome")]

## Piecewise regression 
# For metrics that are not 0-1, make on this scale because a break point estimate is needed 
turkana_pw <- turkana
turkana_pw$urb_score <- turkana_pw$urb_score/max(turkana_pw$urb_score,na.rm = T)

# Model 
pw_out <- do.call(rbind, lapply(1:nrow(combos), function(x) { 
  metric <- combos$metric[x]
  outcome <- combos$outcome[x]
  tmp <- turkana_pw[complete.cases(turkana_pw[c(metric,outcome,"age","gender")]),
                    c(metric,outcome,"age","gender")]
  colnames(tmp)[1:2] <- c("metric","outcome")
  fit <- lm(outcome ~ metric + age + gender, tmp)
  segmented.fit <- segmented(fit, seg.Z = ~metric, psi=0.5)
  out <- data.frame(metric = metric, 
                    outcome = outcome,
                    psi_est = segmented.fit$psi[2],
                    n = nrow(tmp))
  return(out)
}))
pw_out <- pw_out %>% 
  left_join(.,turkana_outcomes_df,by="outcome") %>% 
  left_join(.,turkana_metrics_df,by="metric") %>% 
  mutate(biomarker_type = case_when(
    outcome %in% c("waist_circumference_mean","weight_kgs",
                   "bmi_2","body_fat_percentage","waist_hip") ~ "Body composition",
    outcome %in% c("total_cholesterol","ldl_hdl","non_hdl",
                   "ldl","hdl","glucose_level","triglycerides") ~ "Blood biomarker",
    outcome %in% c("blood_pressure_systolic","blood_pressure_diastolic") ~ "Blood pressure", .default = NA))

# Are certain types of biomarkers (body composition) affected at different point than other biomarker types? 
summary(lm(psi_est ~ biomarker_type, 
           pw_out %>% mutate(biomarker_type=relevel(factor(biomarker_type), ref = "Body composition")))) 

# Does urbanicity score/material wealth affect outcomes sooner in transition than other scales? 
summary(lm(psi_est ~ metric, 
           pw_out %>% mutate(metric=relevel(factor(metric), ref = "urb_score")))) 

