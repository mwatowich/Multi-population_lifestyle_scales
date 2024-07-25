### Model sex differences in lifestyle effects 

library(dplyr)
library(ggplot2)

# Code shows Turkana data, same methods used for Orang Asli data 
# Run script 01_lifestyleScales_healthOutcomes.R first to generate objects with health outcomes and lifestyle scales

## 1) Test interactive effect (sex*lifestyle scale) 
# Continuous models 
turkana_outcomes_cont <- turkana_outcomes[!turkana_outcomes %in% c("obesity_overweight","hypertension")]
out_sex_cont <- do.call(rbind, lapply(1:length(turkana_outcomes_cont), function(i) {
  do.call(rbind, lapply(1:length(turkana_metrics), function(x) {
    
    # Subset for complete cases across all measures
    tmp <- turkana
    tmp$outcome <- tmp[[turkana_outcomes_cont[i]]]
    tmp <- as.data.frame(na.omit(tmp[,c("age","sex","outcome",turkana_metrics)]))
    tmp$metric <- tmp[[turkana_metrics[x]]]
    
    # Model
    out <- lm(scale(tmp$outcome) ~ age + sex*scale(tmp$metric), tmp)
    out_1 <- data.frame(cbind(t(summary(out)$coefficients[5,c(1,2,4)]), 
                              AIC = AIC(out), 
                              n = dim(tmp)[1]))
    out_1$metric <- turkana_metrics[x]
    out_1$outcome <- turkana_outcomes_cont[i]
    colnames(out_1)[1:3] <- c("beta", "se", "pval")
    return(out_1)
  }))
}))

# Binomial models 
turkana_outcomes_binom <- c("obesity_overweight","hypertension")
out_sex_binom <- do.call(rbind, lapply(1:length(turkana_outcomes_binom), function(i) {
  do.call(rbind, lapply(1:length(turkana_metrics), function(x) {
    
    # Subset for complete cases across all measures
    tmp <- turkana
    tmp$outcome <- tmp[[turkana_outcomes_binom[i]]]
    tmp <- as.data.frame(na.omit(tmp[,c("age","sex","outcome",turkana_metrics)]))
    tmp$metric <- tmp[[turkana_metrics[x]]]
    
    # Model
    out <- glm(outcome ~ age + sex*metric, data = tmp, family = "binomial")
    out_1 <- data.frame(cbind(t(summary(out)$coefficients[5,c(1,2,4)]), 
                              AIC = AIC(out), 
                              n = dim(tmp)[1]))
    out_1$metric <- turkana_metrics[x]
    out_1$outcome <- turkana_outcomes_binom[i]
    colnames(out_1)[1:3] <- c("beta", "se", "pval")
    return(out_1)
  }))
}))

# p adjust and then split again 
tmp_t_g <- dplyr::bind_rows(out_sex_cont, out_sex_binom)
tmp_t_g$padj <- p.adjust(tmp_t_g$pval, method = "BH")
out_sex_cont_2 <- tmp_t_g[!(tmp_t_g$outcome %in% c("obesity_overweight","hypertension")),]
out_sex_binom_2 <- tmp_t_g[tmp_t_g$outcome %in% c("obesity_overweight","hypertension"),]

# Generate Table S8 (S9 for Orang Asli)
# rbind(out_sex_cont_2,
#       out_sex_binom_2) %>%
#   left_join(., turkana_outcomes_df, by = "outcome") %>%
#   left_join(., turkana_metrics_df, by = "metric") %>%
#   dplyr::select(!c("metric","outcome")) %>%
#   dplyr::select(c(outcome_title,title,n,AIC,everything())) %>%
#   dplyr::rename(`Sex x lifestyle beta` = beta,
#                 `Sex x lifestyle SE` = se,
#                 `Sex x lifestyle pval` = pval,
#                 `Sex x lifestyle padj` = padj)

## 2) Model females/males separately to determine lifestyle scales that best predict cardiometabolic health in females vs males and test correlation 
out_splitsex_cont <- do.call(rbind, lapply(1:length(turkana_outcomes_cont), function(i) {
  do.call(rbind, lapply(c("F","M"), function(y) {
    do.call(rbind, lapply(1:length(turkana_metrics), function(x) {
      
      # Subset for complete cases across all measures
      tmp <- turkana[which(turkana$sex == y),]
      tmp$outcome <- tmp[[turkana_outcomes_cont[i]]]
      tmp <- as.data.frame(na.omit(tmp[,c("age","outcome",turkana_metrics)]))
      tmp$metric <- tmp[[turkana_metrics[x]]]
      
      # Model
      out <- lm(scale(tmp$outcome) ~ age + scale(tmp$metric), tmp)
      out_1 <- data.frame(cbind(t(summary(out)$coefficients[3,c(1,2,4)]), 
                                AIC = AIC(out), 
                                n = dim(tmp)[1]))
      out_1$metric <- turkana_metrics[x]
      out_1$outcome <- turkana_outcomes_cont[i]
      out_1$sex <- y
      colnames(out_1)[1:3] <- c("beta", "se", "pval")
      return(out_1)
    }))
  }))
}))
out_splitsex_binom <- do.call(rbind, lapply(1:length(turkana_outcomes_binom), function(i) {
  do.call(rbind, lapply(c("F","M"), function(y) {
    do.call(rbind, lapply(1:length(turkana_metrics), function(x) {
      
      # Subset for complete cases across all measures
      tmp <- turkana[which(turkana$sex==y),]
      tmp$outcome <- tmp[[turkana_outcomes_binom[i]]]
      tmp <- as.data.frame(na.omit(tmp[,c("age","outcome",turkana_metrics)]))
      tmp$metric <- tmp[[turkana_metrics[x]]]
      
      # Model
      out <- glm(outcome ~ age + metric, data = tmp, family = "binomial")
      out_1 <- data.frame(cbind(t(summary(out)$coefficients[3,c(1,2,4)]), 
                                AIC = AIC(out), 
                                n = dim(tmp)[1]))
      out_1$metric <- turkana_metrics[x]
      out_1$outcome <- turkana_outcomes_binom[i]
      out_1$sex <- y
      colnames(out_1)[1:3] <- c("beta", "se", "pval")
      return(out_1)
    }))
  }))
}))
out_splitsex_cont$sex <- ifelse(out_splitsex_cont$sex=="F","Female","Male")
out_splitsex_binom$sex <- ifelse(out_splitsex_binom$sex=="F","Female","Male")
# female models together 
fem_all <- dplyr::bind_rows(out_splitsex_cont[out_splitsex_cont$sex=="Female",],
                            out_splitsex_binom[out_splitsex_binom$sex=="Female",])
fem_all$padj <- p.adjust(fem_all$pval, method = "BH")
# male models together 
male_all <- dplyr::bind_rows(out_splitsex_cont[out_splitsex_cont$sex=="Male",],
                             out_splitsex_binom[out_splitsex_binom$sex=="Male",])
male_all$padj <- p.adjust(male_all$pval, method = "BH")
# bind 
out_splitsex_cont_2 <- rbind(fem_all[!(fem_all$outcome %in% c("obesity_overweight","hypertension")),],
                                male_all[!(male_all$outcome %in% c("obesity_overweight","hypertension")),])
out_splitsex_binom_2 <- rbind(fem_all[fem_all$outcome %in% c("obesity_overweight","hypertension"),],
                                 male_all[male_all$outcome %in% c("obesity_overweight","hypertension"),])

# Generate Table S10 (S11 for Orang Asli)
# rbind(out_splitsex_cont_2,
#       out_splitsex_binom_2) %>%
#   left_join(., turkana_outcomes_df, by = "outcome") %>%
#   left_join(., turkana_metrics_df, by = "metric") %>%
#   dplyr::select(!c("metric","outcome")) %>%
#   dplyr::select(c(outcome_title,title,sex,n,AIC,everything())) %>%
#   dplyr::rename(`Lifestyle beta` = beta,
#                 `Lifestyle SE` = se,
#                 `Lifestyle pval` = pval,
#                 `Lifestyle padj` = padj,
#                 Sex = sex,
#                 Outcome = outcome_title,
#                 `Lifestyle scale` = title)

# Test correlation in AIC rank between sexes and plot
ranked_turkana_sex <- out_splitsex_cont %>% 
  group_by(outcome,sex) %>% 
  mutate(lowest_AIC = as.numeric(min(AIC)),
         AIC_diff = AIC-lowest_AIC) %>% 
  group_by(metric,sex) %>% 
  summarise(mean = mean(AIC_diff)) %>% 
  group_by(sex) %>% 
  mutate(rank=rank(mean)) 

# Correlation 
cor.test(ranked_turkana_sex$mean[ranked_turkana_sex$sex == "Female"], 
         ranked_turkana_sex$mean[ranked_turkana_sex$sex == "Male"],
         method = "spearman")
