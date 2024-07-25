### Modeling the effect of lifestyle scales on cardiometabolic phenotypes 

library(dplyr)

# Code shows Turkana data, same methods used for Orang Asli data 
# Run script 01_lifestyleScales_healthOutcomes.R first to generate objects with health outcomes and lifestyle scales


# Continuous models 
turkana_outcomes_cont <- turkana_outcomes[!turkana_outcomes %in% c("obesity_overweight","hypertension")]
out_cont <- do.call(rbind, lapply(1:length(turkana_outcomes_cont), function(i) {
  do.call(rbind, lapply(1:length(turkana_metrics), function(x) {
    
    # Subset for complete cases across all measures
    tmp <- turkana
    tmp$outcome <- tmp[[turkana_outcomes_cont[i]]]
    tmp <- as.data.frame(na.omit(tmp[,c("age","gender","outcome",turkana_metrics)]))
    tmp$metric <- tmp[[turkana_metrics[x]]]
    
    # Model
    out <- lm(scale(tmp$outcome) ~ tmp$age + tmp$gender + scale(tmp$metric))
    out_simple <- lm(scale(tmp$outcome) ~ tmp$age + tmp$gender)
    out_1 <- data.frame(cbind(t(summary(out)$coefficients[4,c(1,2,4)]), 
                              AIC = AIC(out), 
                              partial_r2_metric = summary(out)$r.squared - summary(out_simple)$r.squared, 
                              n = dim(tmp)[1]))
    out_1$metric <- turkana_metrics[x]
    out_1$outcome <- turkana_outcomes_cont[i]
    colnames(out_1)[1:3] <- c("beta", "se", "pval")
    return(out_1)
  }))
}))

# Binomial models 
turkana_outcomes_binom <- c("obesity_overweight","hypertension")
out_binom <- do.call(rbind, lapply(1:length(turkana_outcomes_binom), function(i) {
  do.call(rbind, lapply(1:length(turkana_metrics), function(x) {
    
    # Subset for complete cases across all measures
    tmp <- turkana
    tmp$outcome <- tmp[[turkana_outcomes_binom[i]]]
    tmp <- as.data.frame(na.omit(tmp[,c("age","gender","outcome",turkana_metrics)]))
    tmp$metric <- tmp[[turkana_metrics[x]]]
    
    # Model
    out <- glm(outcome ~ age + gender + scale(metric), data = tmp, family = "binomial")
    out_1 <- data.frame(cbind(t(summary(out)$coefficients[4,c(1,2,4)]), 
                              AIC = AIC(out), 
                              n = dim(tmp)[1]))
    out_1$metric <- turkana_metrics[x]
    out_1$outcome <- turkana_outcomes_binom[i]
    colnames(out_1)[1:3] <- c("beta", "se", "pval")
    return(out_1)
  }))
}))

# p adjust and then split again 
tmp_t <- dplyr::bind_rows(out_cont, out_binom)
tmp_t$padj <- p.adjust(tmp_t$pval, method = "BH")
out_cont_2 <- tmp_t[!(tmp_t$outcome %in% c("obesity_overweight","hypertension")),]
out_binom_2 <- tmp_t[tmp_t$outcome %in% c("obesity_overweight","hypertension"),]

# Generate table S4 
# rbind(out_cont_2,
#       out_binom_2) %>%
#   left_join(., turkana_outcomes_df, by = "outcome") %>%
#   left_join(., turkana_metrics_df, by = "metric") %>%
#   dplyr::select(!c("outcome","metric")) %>%
#   dplyr::select(c("outcome_title","title",everything())) %>%
#   dplyr::rename(`Lifestyle scale beta` = beta,
#                 `Lifestyle scale SE` = se,
#                 `Lifestyle scale pval` = pval,
#                 `Partial r2` = partial_r2_metric,
#                 `Lifestyle scale padj` = padj,
#                 Outcome = outcome_title,
#                 `Lifestyle scale` = title)

# Can read in table S4 here to recapitulate plots (column names will need to be changed per lines 70-76)

# Plot R2 for all scales
out_cont %>% 
  left_join(., turkana_outcomes_df, by = "outcome") %>% 
  left_join(., turkana_metrics_df, by = "metric") %>% 
  mutate(outcome_title = factor(outcome_title,levels=c(turkana_outcomes_df$outcome_title))) %>% 
  mutate(title = factor(title,levels=c(turkana_metrics_df$title[-1],"Lifestyle category"))) %>% 
  ggplot(aes(x = partial_r2_metric, y = outcome_title)) + 
  geom_violin() + 
  geom_jitter(aes(color = title), size = 3, shape = 1, stroke=2, height = 0.2) + 
  theme_classic(base_size = 20) + 
  theme(axis.title.y = element_blank()) + 
  scale_color_brewer(palette = "Dark2") + 
  scale_y_discrete(limits=rev) + 
  labs(x=bquote('Partial r'^2), y="", color = "")

# Plot betas 
left_join(left_join(out_cont_2, turkana_outcomes_df, by = "outcome"), 
                          turkana_metrics_df, by = "metric") %>% 
  mutate(sig = ifelse(padj<0.05, "p<0.05","n.s."),
         outcome_title = factor(outcome_title, levels = rev(turkana_outcomes_df$outcome_title))) %>% 
  ggplot(aes(x = beta, y = outcome_title, color = sig, label=n)) + 
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") + 
  geom_pointrange(aes(xmin=beta-(1.96*se), 
                      xmax=beta+(1.96*se)),
                  position = position_dodge(width = 0.4)) + 
  facet_wrap(.~title, nrow=1) + 
  theme_classic(base_size = 20) + 
  scale_y_discrete() + 
  scale_x_continuous(breaks = c(0,0.25,0.5)) + 
  theme(strip.text = element_text(size=15),
        axis.text.x = element_text(size=14),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) + 
  labs(fill = "",color = "")

# Mean AIC per metric type 
turkana_out_AIC <- rbind(out_cont[,!colnames(out_cont) %in% c("partial_r2_metric")], 
                         out_binom) %>% 
  left_join(., turkana_metrics_df, by = "metric") %>% 
  mutate(outcome_type = ifelse(outcome %in% 
                                 c("waist_circumference_mean", "body_fat_percentage",
                                   "weight_kgs","bmi_2","waist_hip",
                                   "obesity_overweight"), "Body composition", 
                               ifelse(outcome %in% 
                                        c("blood_pressure_systolic","blood_pressure_diastolic",
                                          "hypertension"), "Blood pressure",
                                      ifelse(outcome %in% 
                                               c("total_cholesterol","hdl","ldl","ldl_hdl","non_hdl",
                                                 "triglycerides","glucose_level"), "Blood biomarkers", NA)))) %>% 
  group_by(outcome) %>%  
  mutate(lowest_AIC = as.numeric(min(AIC)),
         AIC_diff = AIC-lowest_AIC) 

rbind(turkana_out_AIC %>% 
        group_by(title) %>% 
        summarise(mean = mean(AIC_diff)) %>% 
        mutate(outcome_type = "Cumulative"), 
      turkana_out_AIC %>% 
        group_by(title, outcome_type) %>% 
        summarise(mean = mean(AIC_diff))) %>% 
  mutate(outcome_type = factor(outcome_type, levels = c("Cumulative","Body composition","Blood biomarkers","Blood pressure"))) %>% 
  ggplot(aes(x = mean, y = reorder(title, mean))) + 
  geom_col() + 
  scale_y_discrete(limits=rev) + 
  theme_classic(base_size = 18) + 
  facet_wrap(.~outcome_type, scales = 'free_x', nrow = 1) + 
  theme(axis.title.y = element_blank()) + 
  labs(x = "Mean AIC difference") + 
  theme(axis.title.y = element_blank())
