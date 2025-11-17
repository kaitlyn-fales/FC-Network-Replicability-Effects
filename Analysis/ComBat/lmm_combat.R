######### Linear mixed effect modeling of preprocessing pipelines ############

# Packages
library(stringr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(readxl)
library(reshape2)

######### LMM after ComBat harmonization #########################
# Full LMM with explanatory variables of pipeline, filter, and atlas
# and a random intercept for each unique ID.

# Set wd and load data
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")
load("all_combos_dataframe_combat.RData")

df <- dat.combat

edges <- colnames(df)[-c(1:5)]
effects <- c("pipeline","filter","atlas","pipeline:filter","pipeline:atlas","filter:atlas",
             "pipeline:filter:atlas")

# Empty matrix to store anova results
aov_results <- matrix(NA, nrow = 7, ncol = 435)
colnames(aov_results) <- edges
rownames(aov_results) <- effects

# Prepare ID df to merge with regressed random effects df later to and preserve ordering
df_regressed <- df[,1:5]

# Function to extract random effects and prepare them to be in merged df
extract_random_effect <- function(mdl){
  ran_effect <- ranef(mdl)
  
  # Prepping ID
  ran_effect$ID = cbind(rownames(ran_effect$ID), 
                          data.frame(ran_effect$ID, row.names=NULL))
  colnames(ran_effect$ID) <- c("ID","ID_effect")
  ran_effect$ID[,1] <- factor(ran_effect$ID[,1])
  ran_effect$ID[,2] <- as.numeric(ran_effect$ID[,2])
  
  return(ran_effect)
}

for (i in 1:ncol(aov_results)){
  # Reduced dataset for particular edge
  dat <- df[,c("pipeline","filter","atlas","site","ID",edges[i])]
  
  # Rename edge to response to be general
  colnames(dat)[6] <- "response"
  
  # Run edgewise lmer model - only need ID random effect
  mdl <- lmer(response ~ pipeline * filter * atlas + (1|ID),
              data = dat, contrasts = list(pipeline = "contr.sum", 
                                           filter = "contr.sum",
                                           atlas = "contr.sum"))
  # Run anova from model 
  aov_results[,i] <- anova(mdl, type = 3)$`Sum Sq`
  
  # Extract random effect from model for use later
  ran_effect <- extract_random_effect(mdl)
  
  # Merge dataframes by site and by ID so everything matches up
  dat <- merge(dat,ran_effect$ID,by="ID")
  
  # Create new variable that is the regressed out version
  dat$response_regress <- dat$response-dat$ID_effect
  
  # Reduce df down after regressing out random effect
  dat_ranef <- dat[,c("pipeline","filter","atlas","site","ID","response_regress")] 
  colnames(dat_ranef) <- c(colnames(dat_ranef)[1:(length(colnames(dat_ranef))-1)],edges[i])
  
  # Merge dat with df_id to create df with regressed effect and preserve unique record
  df_regressed <- merge(df_regressed, dat_ranef, by = c("pipeline","filter","atlas","site","ID"))
}

# Export regressed df
save(df_regressed, file = "all_combos_dataframe_combat_regressed.RData")

# Change to dataframe
aov_results <- as.data.frame(aov_results)

# Denote between networks
edges_base <- unique(sub("([^_]+_[^_]+)_.*", "\\1", edges))
edges_base <- unique(ifelse(grepl("[0-9]$", edges_base),
                     sub("^(.*?)_.*", "\\1", edges_base),
                     edges_base))

# Create matrix to store results
network_aov_results <- matrix(NA, nrow = 7, ncol = 28)
colnames(network_aov_results) <- edges_base
rownames(network_aov_results) <- effects

# Within-network, need to separate to take mean over correct range
for (i in 1:7){
  network_aov_results[,i] <- aov_results[,c(1:54)] %>% 
    select(starts_with(edges_base[i])) %>% 
    rowMeans()
}

# Between network
for (i in 8:ncol(network_aov_results)){
  network_aov_results[,i] <- aov_results[,-c(1:54)] %>% 
    select(starts_with(edges_base[i])) %>% 
    rowMeans()
}

# Change to dataframe
network_aov_results <- as.data.frame(network_aov_results)

# Add in between networks mean column for comparison
network_aov_results$between_network_mean <- rowMeans(aov_results[,-c(1:54)])

# Export results
write.csv(network_aov_results, file = "Results/lmm_results_combat.csv",
          row.names = T)



