########### Within-Block/Network vs. Between Block/Network Correlation #########

# Packages
library(tidyverse)

# Working directory
#setwd("~/FC-Network-Replicability-Effects")

# Load hierarchical clustering assignments
load("Analysis/Exploratory Analysis/hclust_cuts.RData")

# Reduce to 28 cluster options since filtering does not have pronounced effect
clust <- cut_avg[!grepl("nofilt",cut_avg$combination),]

# Extract list of names of all 28 preprocessing combinations
combos <- clust$combination

# Organize as df
preproc_combo_df <- t(data.frame(strsplit(combos, split = "_")))[,c(1:2,5)] # cols of interest
rownames(preproc_combo_df) <- NULL
colnames(preproc_combo_df) <- c("pipeline","filter","atlas")
preproc_combo_df <- data.frame(preproc_combo_df)
preproc_combo_df$atlas <- gsub("dosenbach160","dos160", preproc_combo_df$atlas)


########### Within Network ###########################
# Load regressed data
load("Data/processed_data_network_regressed.RData")

within_net_avg <- numeric()
within_net_se <- numeric()

for (i in 1:nrow(preproc_combo_df)){
  # Filter and select relevant columns
  df <- df_regressed %>% filter(pipeline == preproc_combo_df$pipeline[i] &
                                  filter == preproc_combo_df$filter[i] &
                                  atlas == preproc_combo_df$atlas[i]) %>% 
    select(-c(pipeline,filter,atlas,site,ID))
  
  # Undo Fisher's z-transform and go back to correlation values
  df <- tanh(df)
  
  # Take mean and se of correlations
  within_net_avg[i] <- mean(as.matrix(df))
  within_net_se[i] <- var(c(as.matrix(df)))/(nrow(df) * ncol(df))
}
######################################################

########### Between Network ###########################
# Load regressed data
load("Data/processed_data_between_network_regressed.RData")

between_net_avg <- numeric()
between_net_se <- numeric()

for (i in 1:nrow(preproc_combo_df)){
  # Filter and select relevant columns
  df <- df_regressed %>% filter(pipeline == preproc_combo_df$pipeline[i] &
                                  filter == preproc_combo_df$filter[i] &
                                  atlas == preproc_combo_df$atlas[i]) %>% 
    select(-c(pipeline,filter,atlas,site,ID))
  
  # Undo Fisher's z-transform and go back to correlation values
  df <- tanh(df)
  
  # Take mean and var of correlations
  between_net_avg[i] <- mean(as.matrix(df))
  between_net_se[i] <- var(c(as.matrix(df)))/(nrow(df) * ncol(df))
}
######################################################

# Make df of results
results <- data.frame(preproc_combo_df,within_net_avg,between_net_avg,
                      within_net_se,between_net_se)

# Difference column
results$difference <- round(results$within_net_avg - results$between_net_avg, digits = 4)

# Standard error column
results$se <- round(sqrt(results$within_net_se + results$between_net_se), digits = 4)

# Reduce down into just differences by pipeline and atlas
pipeline_atlas_diff <- results[,c("pipeline","atlas","difference")]

output <- pipeline_atlas_diff %>% 
    pivot_wider(names_from = pipeline, values_from = difference)

# Export table of mean differences (optional)
#write.csv(output, file = "Analysis/Functional Network Block Structure/within_vs_between_network_corr.csv", row.names = F)

# Reduce down into just se by pipeline and atlas
pipeline_atlas_se <- results[,c("pipeline","atlas","se")]

output <- pipeline_atlas_se %>% 
  pivot_wider(names_from = pipeline, values_from = se)

# Export table of standard errors (optional)
#write.csv(output, file = "Analysis/Functional Network Block Structure/within_vs_between_network_corr_se.csv", row.names = F)
