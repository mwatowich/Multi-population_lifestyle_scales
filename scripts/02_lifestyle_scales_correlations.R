### Correlation between lifestyle scales

library(dplyr)

# Code shows Turkana data, same methods used for Orang Asli data 
# Run script 01_lifestyleScales_healthOutcomes.R first to generate objects with health outcomes and lifestyle scales


### Correlation between scales 
turkana_metrics_df_tmp <- rbind(turkana_metrics_df[-1,],turkana_metrics_df[1,])
turkana_metric_corr <- turkana[,turkana_metrics_df_tmp$metric]
colnames(turkana_metric_corr) = c(turkana_metrics_df_tmp$title)
corrplot::corrplot(cor(turkana_metric_corr,
                       use = "pairwise.complete.obs"), 
                   method = "ellipse", diag = F, type = "upper",
                   tl.col = "black",  addCoef.col = 'black', tl.srt = 45) 


### Is diet less correlated than other scales? 
# Model each variable as a function of each other to get r2 
turkana_metric_corrs <- cbind(as.data.frame(t(combn(turkana_metrics,m = 2))), r = NA)
for (i in 1:nrow(turkana_metric_corrs)) {
  x<-turkana_metric_corrs[i,1]
  y<-turkana_metric_corrs[i,2]
  turkana_metric_corrs[i,3]<-cor.test(turkana[[y]], turkana[[x]])$estimate
}
colnames(turkana_metric_corrs) <- c("metric_1","metric_2","r")

# Label diet scales 
turkana_metric_corrs <- turkana_metric_corrs %>% 
  mutate(diet = ifelse(
    metric_1 %in% c("market_diet_index","traditional_diet_score") |
      metric_2 %in% c("market_diet_index","traditional_diet_score"), "tests diet score", 
    ifelse(!metric_1 %in% c("market_diet_index","traditional_diet_score") & 
             !metric_2 %in% c("market_diet_index","traditional_diet_score"), "no diet score", NA))) %>% 
  mutate(trad_diet = ifelse(
    metric_1 == c("traditional_diet_score") |
      metric_2 == c("traditional_diet_score"), "tests traditional diet","no traditional diet score"))

# Test if r2 is less correlated for diet scores than other scores
t.test(turkana_metric_corrs[turkana_metric_corrs$diet=="tests diet score",]$r, 
       turkana_metric_corrs[turkana_metric_corrs$diet=="no diet score",]$r)

