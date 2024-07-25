### Set health variables of interest and metrics per population
# Turkana 
turkana_outcomes_df<-data.frame(outcome = c("waist_circumference_mean","weight_kgs",
                                            "bmi_2","body_fat_percentage","waist_hip",
                                            "total_cholesterol","ldl_hdl","non_hdl","ldl","hdl",
                                            "glucose_level","triglycerides",
                                            "blood_pressure_systolic","blood_pressure_diastolic",
                                            "obesity_overweight","hypertension"),
                                outcome_title = c("Waist circumference (cm)","Weight (kgs)","BMI",
                                                  "Body fat %","Waist:Hip ratio","Total cholesterol", 
                                                  "LDL:HDL cholesterol ratio","Non-HDL cholesterol",
                                                  "LDL cholesterol","HDL cholesterol",
                                                  "Glucose level (mg/dL)","Triglycerides (mg dL)", 
                                                  "Blood pressure (mm Hg systolic)",
                                                  "Blood pressure (mm Hg diastolic)",
                                                  "Obese/overweight","Hypertension"))

# Orang Asli 
oa_outcomes_df<-data.frame(outcome = c("waist_circum","weight","bmi","body_fat","waist_hip",
                                       "total_chol","ldl_hdl","non_hdl","ldl","hdl","glucose", 
                                       "triglyc","sys_bp2","dias_bp2","obesity_overweight","hypertension"), 
                           outcome_title = c("Waist circumference (cm)","Weight (kgs)","BMI",
                                             "Body fat %","Waist:Hip ratio","Total cholesterol", 
                                             "LDL:HDL cholesterol ratio","Non-HDL cholesterol",
                                             "LDL cholesterol","HDL cholesterol","Glucose level (mg/dL)",
                                             "Triglycerides (mg dL)",
                                             "Blood pressure (mm Hg systolic)",
                                             "Blood pressure (mm Hg diastolic)",
                                             "Obese/overweight","Hypertension"))
turkana_outcomes <- turkana_outcomes_df$outcome
oa_outcomes <- oa_outcomes_df$outcome


## Lifestyle scales
turkana_metrics_df <- data.frame(metric = c("lifestyle_num", "h_sol", "urb_score",
                                            "subsistence", "acculturation_score", "material_wealth",
                                            "traditional_diet_score", "market_diet_index"), 
                                 title = c("Lifestyle category", "Household style of life",
                                           "Urbanicity score","Subsistence score",
                                           "Acculturation score","Material wealth score",
                                           "Traditional diet score","Market diet score"))
oa_metrics_df <- data.frame(metric=c("h_sol","urb_score","subsistence","mobility", 
                                     "acculturation_score", "material_wealth",
                                     "traditional_diet_score", "market_diet_index"),
                            title=c("Household style of life","Urbanicity score",
                                    "Subsistence score","Mobility score","Acculturation score",
                                    "Material wealth score","Traditional diet score","Market diet score"))
turkana_metrics <- turkana_metrics_df$metric
oa_metrics <- oa_metrics_df$metric
