### Factor analysis of lifestyle variables 

library(factoextra) 
library(psych) 
library(dplyr) 
library(ggplot2) 

# Code shows Turkana data, same methods used for Orang Asli data 
# Run script 01_lifestyleScales_healthOutcomes.R first to generate objects with health outcomes and lifestyle scales


## Factor analysis of Turkana lifestyle variables
# The item 'turkana' refers to a matrix of data in which rows are participants and columns are attributes (e.g., age, sex, and answers to interview questions)
turkana_FA <- as.data.frame(left_join(turkana[,c("unique_id","Y_latitude","Standardized_name",
                                                 "ppl_per_room_score","number_of_rooms_in_the_household",
                                                 "presence_of_a_finished_floor_score",
                                                 "presence_of_an_iron_concrete_or_slate_roof_score",
                                                 "presence_of_electricity_score", "presence_of_flush_toilet_score", 
                                                 "does_the_household_have_indoor_tap_water_score",
                                                 "does_the_household_drink_treated_or_boiled_water_score",
                                                 "does_the_household_cook_with_gas_score", 
                                                 "presence_of_television_set_score", "presence_of_mobile_phone_score", 
                                                 "subsistence","highest_education_level_num", "language_score",
                                                 "milk_score", "milk_ferm_score","blood_score",
                                                 "salt_score", "sugar_score", "oil_score")], 
                                      distinct(density[,c("Standardized_name","ken_general_2020")]), # 'density' contains estimated population density from Gridded World Population for each sampling location 
                                      by = "Standardized_name")) %>% 
  dplyr::select(!c("Y_latitude", "Standardized_name")) %>% 
  dplyr::rename("population density" = "ken_general_2020") 
rownames(turkana_FA)=turkana_FA$unique_id
turkana_FA$unique_id <- NULL
turkana_FA <- turkana_FA[rowSums(is.na(turkana_FA)) != ncol(turkana_FA),]

# Check number of NAs - may need to remove columns with high numbers of NAs 
hist(rowSums(is.na(turkana_FA)))
as.data.frame(colSums(is.na(turkana_FA))) %>% 
  tibble::rownames_to_column(var = "measure") %>% 
  ggplot(aes(y = measure, x = `colSums(is.na(turkana_FA))`)) + 
  geom_col() + 
  theme_classic(base_size=14)

# Remove columns with high missingness 
turkana_FA_2 <- as.data.frame(turkana_FA[,!colnames(turkana_FA) %in% c("milk_ferm_score","language_score","blood_score")])

# Run PCA, check % variance and scree plot
turkana_FA_pca <- prcomp(cor(na.omit(turkana_FA_2)))
summary(turkana_FA_pca)
fviz_eig(turkana_FA_pca)

# Run factor analysis and specify 2 factors - based on scree plot 
factor_model = fa(turkana_FA_2, 2, rotate = "varimax")
summary(factor_model)
factor_model[["Structure"]]

# Pull out loadings for each factor
factor_model_loadings <- as.data.frame.table(factor_model$loadings)
colnames(factor_model_loadings) <- c("variable", "factor", "loading") 
cor.test(factor_model_loadings[which(factor_model_loadings$factor == "MR1"),]$loading,
         factor_model_loadings[which(factor_model_loadings$factor == "MR2"),]$loading)
plot(factor_model_loadings[which(factor_model_loadings$factor == "MR1"),]$loading,
     factor_model_loadings[which(factor_model_loadings$factor == "MR2"),]$loading)

# Extract scores 
factor_model_scores <- as.data.frame(factor_model$scores)
factor_model_scores$unique_id <- rownames(factor_model_scores)
turkana_FA_3 <- left_join(turkana_FA_2 %>% tibble::rownames_to_column(var="unique_id"), factor_model_scores, c("unique_id"))

# Model health ~ factors vs. health ~ urb scores 
turkana_FA_4 <- left_join(factor_model_scores, turkana, c("unique_id"))

# Continuous models 
turkana_outcomes_cont <- turkana_outcomes[!turkana_outcomes %in% c("obesity_overweight","hypertension")] 
turkana_fa_mods <- do.call(rbind, lapply(turkana_outcomes_cont, function(x) {
  do.call(rbind, lapply(c("MR1","MR2","urb_score","material_wealth","h_sol"), function(y) {
    
    # Subset for complete cases across all measures
    tmp <- turkana_FA_4
    tmp$outcome <- tmp[[x]]
    tmp <- as.data.frame(na.omit(tmp[,c("age","gender","outcome",
                                        "MR1","MR2","urb_score","material_wealth","h_sol")]))
    tmp$metric <- tmp[[y]]
    
    # Model 
    out <- lm(scale(outcome) ~ scale(metric) + age + gender, tmp)
    out_1 <- cbind(as.data.frame(summary(out)$coefficients[-1,-3]))
    colnames(out_1) <- c("beta", "se", "pval")
    out_1 <- out_1 %>%
      tibble::rownames_to_column(var = "covariate") %>% 
      tidyr::pivot_wider(names_from = covariate, values_from = c(beta, se, pval))
    colnames(out_1) <- gsub("scale\\(metric\\)", "factor", colnames(out_1))
    
    # Other columns 
    out_1$outcome = x
    out_1$factor = y
    out_1$AIC = AIC(out)
    out_1$n = nrow(tmp)
    return(out_1)
  }))
}))

# Binomial models 
turkana_outcomes_binom <- c("obesity_overweight","hypertension")
turkana_fa_mods_binom <- do.call(rbind, lapply(turkana_outcomes_binom, function(x) {
  do.call(rbind, lapply(c("MR1","MR2","urb_score","material_wealth","h_sol"), function(y) {
    
    # Subset for complete cases across all measures
    tmp <- turkana_FA_4
    tmp$outcome <- tmp[[x]]
    tmp <- as.data.frame(na.omit(tmp[,c("age","gender","outcome",
                                        "MR1","MR2","urb_score","material_wealth","h_sol")]))
    tmp$metric <- tmp[[y]]
    
    # Model 
    out <- glm(outcome ~ age + gender + scale(metric), data = tmp, family = "binomial")
    out_1 <- cbind(as.data.frame(summary(out)$coefficients[-1,-3]))
    colnames(out_1) <- c("beta", "se", "pval")
    out_1 <- out_1 %>%
      tibble::rownames_to_column(var = "covariate") %>% 
      tidyr::pivot_wider(names_from = covariate, values_from = c(beta, se, pval))
    colnames(out_1) <- gsub("scale\\(metric\\)", "factor", colnames(out_1))
    
    # Other columns 
    out_1$outcome = x
    out_1$factor = y
    out_1$AIC = AIC(out)
    out_1$n = nrow(tmp)
    return(out_1)
  }))
}))

# p adjust and then split again 
tmp_t_fa <- dplyr::bind_rows(turkana_fa_mods, turkana_fa_mods_binom)
tmp_t_fa$padj <- p.adjust(tmp_t_fa$pval_factor, method = "BH")
out_cont_fa <- tmp_t_fa[!(tmp_t_fa$outcome %in% c("obesity_overweight","hypertension")),]
out_binom_fa <- tmp_t_fa[tmp_t_fa$outcome %in% c("obesity_overweight","hypertension"),]

# Generates table S6 
# write.table(rbind(out_cont_fa,
#                   out_binom_fa) %>%
#               filter(factor %in% c("MR1","MR2")) %>%
#               left_join(., turkana_outcomes_df, by = "outcome") %>%
#               dplyr::select(!c("outcome")) %>%
#               dplyr::select(c(outcome_title, factor, AIC, n, beta_factor, se_factor, pval_factor, padj)) %>%
#               dplyr::rename(Outcome = outcome_title,
#                             Factor = factor,
#                             `Factor effect size` = beta_factor,
#                             `Factor SE` = se_factor,
#                             `Factor pvalue` = pval_factor,
#                             `Factor padj` = padj),
#             file = "S6_Turkana_Factor_models.txt",
#             quote = F,sep = "\t",row.names = F)

# Plot effect sizes 
out_cont_fa%>%mutate(type="cont") %>% 
  dplyr::select(c("outcome","factor","beta_factor","padj","se_factor","n")) %>% 
  left_join(., turkana_outcomes_df, by = c("outcome")) %>% 
  filter(factor %in% c("MR1","MR2","urb_score")) %>% 
  mutate(outcome_title = factor(outcome_title, levels = turkana_outcomes_df$outcome_title),
         sig = ifelse(padj<0.05, "p<0.05","n.s."),
         factor = case_when(
           factor == "MR1" ~ "Factor 1 (non-diet)",
           factor == "MR2" ~ "Factor 2 (diet)",
           factor == "urb_score" ~ "Urbanicity score"), 
         factor = factor(factor, levels=c("Urbanicity score","Factor 1 (non-diet)", "Factor 2 (diet)"))
  ) %>% 
  ggplot(aes(y = outcome_title, x = beta_factor, color = factor, shape=sig)) + 
  geom_vline(xintercept = 0, lty=2, color = "grey70") + 
  geom_pointrange(aes(xmin=beta_factor-(1.96*se_factor), 
                      xmax=beta_factor+(1.96*se_factor)),
                  position = position_dodge(width = 0.2), size = 0.8) + 
  scale_color_manual(values = park_palette("GeneralGrant")[c(8,3,2)]) + 
  scale_shape_manual(values=c(1, 19), guide = "none") + 
  theme_classic(base_size = 26) + 
  theme(axis.title.y = element_blank()) + 
  geom_text(aes(x = 0.6, label=n), color = "black", size = 6) + 
  scale_y_discrete(limits=rev) + 
  scale_x_continuous(limits = c(-0.27,0.6)) + 
  labs(x="Effect size",y="", fill="", color="") 

# Plot mean AIC between factors and urb scores 
turkana_FA_AIC <- rbind(turkana_fa_mods, turkana_fa_mods_binom) %>% 
  mutate(outcome_type = ifelse(outcome %in% 
                                 c("waist_circumference_mean", "body_fat_percentage",
                                   "weight_kgs","bmi_2","waist_hip",
                                   "obesity_overweight"), "Body composition", 
                               ifelse(outcome %in% 
                                        c("blood_pressure_systolic","blood_pressure_diastolic",
                                          "hypertension"), "Blood pressure",
                                      ifelse(outcome %in% 
                                               c("total_cholesterol","hdl","ldl","ldl_hdl","non_hdl",
                                                 "triglycerides","glucose_level","hba1c"), "Blood biomarkers", NA))), 
         factor = case_when(
           factor == "MR1" ~ "Factor 1 (non-diet)",
           factor == "MR2" ~ "Factor 2 (diet)",
           factor == "urb_score" ~ "Urbanicity scale",
           factor == "h_sol" ~ "H-SOL",
           factor == "material_wealth" ~ "Material wealth"), 
         factor = factor(factor, levels=c("Urbanicity scale","Material wealth","H-SOL",
                                          "Factor 1 (non-diet)", "Factor 2 (diet)"))) %>% 
  group_by(outcome) %>% 
  mutate(lowest_AIC = as.numeric(min(AIC)),
         AIC_diff = AIC-lowest_AIC)
turk_fa_AIC_plot <- turkana_FA_AIC %>% 
  group_by(factor) %>% 
  summarise(mean = mean(AIC_diff)) %>% 
  mutate(outcome_type = "Cumulative") %>% 
  ggplot(aes(x = mean, y = reorder(factor, mean))) + 
  geom_col(fill = park_palette("GeneralGrant")[7]) + 
  scale_y_discrete(limits=rev) + 
  theme_classic(base_size = 16) + 
  theme(axis.title.y = element_blank()) + 
  labs(x = "Mean AIC difference", y = "")

# Correlation b/w factor 1, factor 2, and other scores 
turkana_metrics_df_tmp <- rbind(c("MR1","Factor 1"), c("MR2","Factor 2"),
                                turkana_metrics_df[-1,],turkana_metrics_df[1,])
colnames(turkana_FA_4[,turkana_metrics_df_tmp$metric]) == turkana_metrics_df_tmp$metric
turkana_metric_corr <- turkana_FA_4[,turkana_metrics_df_tmp$metric]
colnames(turkana_metric_corr) = c(turkana_metrics_df_tmp$title)
corrplot::corrplot(cor(turkana_metric_corr, use = "pairwise.complete.obs"), 
                   method = "ellipse", diag = F, type = "lower",
                   tl.col = "black",  addCoef.col = 'black', tl.srt = 45) 
