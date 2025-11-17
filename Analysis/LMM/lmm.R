######### Linear mixed effect modeling of processing pipelines ############

# Packages
library(stringr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(readxl)
library(reshape2)

######### Full linear mixed model WITHIN each network #########################

# Set wd and load data
#setwd("~/FC-Network-Replicability-Effects")
load("Data/processed_data_network.RData")

# Prepare empty dataframe to store results 
edges <- colnames(df)[-c(1:5)]
effects <- c("pipeline","filter","atlas","pipeline:filter","pipeline:atlas","filter:atlas",
             "pipeline:filter:atlas")

# Empty matrix to store anova results
aov_results <- matrix(NA, nrow = 7, ncol = 54)
colnames(aov_results) <- edges
rownames(aov_results) <- effects

# Prepare ID df to merge with regressed random effects df later to and preserve ordering
df_regressed <- df[,1:5]

# Prepare ID df to merge with baseline effects df later to and preserve ordering
df_baseline <- df[,1:5]

# Function to extract random effects and prepare them to be in merged df
extract_random_effect <- function(mdl){
  ran_effect <- ranef(mdl)
  
  # Prepping ID:Site
  ran_effect$`ID:site` <- cbind(rownames(ran_effect$`ID:site`), 
                                data.frame(ran_effect$`ID:site`, 
                                           row.names=NULL))
  ran_effect$`ID:site` <- cbind(data.frame(t(data.frame(
    strsplit(ran_effect$`ID:site`$`rownames(ran_effect$\`ID:site\`)`, split = ":"))),
    ran_effect$`ID:site`$X.Intercept.,row.names = NULL))[,c(1,3)]
  colnames(ran_effect$`ID:site`) <- c("ID","ID_site_effect")
  ran_effect$`ID:site`[,1] <- factor(ran_effect$`ID:site`[,1])
  ran_effect$`ID:site`[,2] <- as.numeric(ran_effect$`ID:site`[,2])
  
  # Prepping site
  ran_effect$site = cbind(rownames(ran_effect$site), 
                          data.frame(ran_effect$site, row.names=NULL))
  colnames(ran_effect$site) <- c("site","site_effect")
  ran_effect$site[,1] <- factor(ran_effect$site[,1])
  ran_effect$site[,2] <- as.numeric(ran_effect$site[,2])
  
  return(ran_effect)
}

for (i in 1:ncol(aov_results)){
  # Reduced dataset for particular edge
  dat <- df[,c("pipeline","filter","atlas","site","ID",edges[i])]
  
  # Rename edge to response to be general
  colnames(dat)[6] <- "response"
  
  # Run edgewise lmer model
  mdl <- lmer(response ~ pipeline * filter * atlas + (1|site/ID),
              data = dat, contrasts = list(pipeline = "contr.sum", 
                                           filter = "contr.sum",
                                           atlas = "contr.sum"))
  # Run anova from model 
  aov_results[,i] <- anova(mdl, type = 3)$`Sum Sq`
  
  # Extract intercept from model for use later
  intercept <- as.numeric(fixef(mdl)[1])
  
  # Extract residuals
  resid <- as.numeric(residuals(mdl))
  
  # Add intercept and residuals as columns in dat
  dat <- cbind(dat,intercept,resid)
  
  # Extract random effect from model for use later
  ran_effect <- extract_random_effect(mdl)
  
  # Merge dataframes by site and by ID so everything matches up
  dat <- merge(dat,ran_effect$site,by="site")
  dat <- merge(dat,ran_effect$`ID:site`,by="ID")
  
  # Create new variable that is the regressed out version
  dat$response_regress <- dat$response-dat$site_effect-dat$ID_site_effect
  
  # Create new variable that is the baseline version
  dat$baseline <- dat$intercept + dat$site_effect + dat$ID_site_effect + dat$resid
  
  # Reduce df down after regressing out random effect
  dat_ranef <- dat[,c("pipeline","filter","atlas","site","ID","response_regress")] 
  colnames(dat_ranef) <- c(colnames(dat_ranef)[1:(length(colnames(dat_ranef))-1)],edges[i])
  
  # Reduce df down after building baseline effect
  dat_base <- dat[,c("pipeline","filter","atlas","site","ID","baseline")] 
  colnames(dat_base) <- c(colnames(dat_base)[1:(length(colnames(dat_base))-1)],edges[i])
  
  # Merge dat with df_id to create df with regressed effect and preserve unique record
  df_regressed <- merge(df_regressed, dat_ranef, by = c("pipeline","filter","atlas","site","ID"))
  df_baseline <- merge(df_baseline, dat_base, by = c("pipeline","filter","atlas","site","ID"))
}

# Export baseline df
save(df_baseline, file = "Data/processed_data_network_baseline.RData")

# Export regressed df
save(df_regressed, file = "Data/processed_data_network_regressed.RData")

# Change to dataframe
aov_results <- as.data.frame(aov_results)

# Denote networks
networks <- c("DefaultMode","SensoriMotor","Visual","Salience",
              "DorsalAttention","FrontoParietal","Language")

# Create matrix to store results
network_aov_results <- matrix(NA, nrow = 7, ncol = 7)
colnames(network_aov_results) <- networks
rownames(network_aov_results) <- effects

for (i in 1:ncol(network_aov_results)){
  network_aov_results[,i] <- aov_results %>% 
    select(starts_with(networks[i])) %>% 
    rowMeans()
}

# Change to dataframe
network_aov_results <- as.data.frame(network_aov_results)

################################################################################

# Clear environment
rm(list = ls())

######### Full linear mixed model BETWEEN each network #########################

# Set wd and load data
#setwd("~/FC-Network-Replicability-Effects")
load("Data/processed_data_between_network.RData")

edges <- colnames(df)[-c(1:5)]
effects <- c("pipeline","filter","atlas","pipeline:filter","pipeline:atlas","filter:atlas",
             "pipeline:filter:atlas")

# Empty matrix to store anova results
aov_results <- matrix(NA, nrow = 7, ncol = 381)
colnames(aov_results) <- edges
rownames(aov_results) <- effects

# Prepare ID df to merge with regressed random effects df later to and preserve ordering
df_regressed <- df[,1:5]

# Prepare ID df to merge with baseline effects df later to and preserve ordering
df_baseline <- df[,1:5]

# Function to extract random effects and prepare them to be in merged df
extract_random_effect <- function(mdl){
  ran_effect <- ranef(mdl)
  
  # Prepping ID:Site
  ran_effect$`ID:site` <- cbind(rownames(ran_effect$`ID:site`), 
                                data.frame(ran_effect$`ID:site`, 
                                           row.names=NULL))
  ran_effect$`ID:site` <- cbind(data.frame(t(data.frame(
    strsplit(ran_effect$`ID:site`$`rownames(ran_effect$\`ID:site\`)`, split = ":"))),
    ran_effect$`ID:site`$X.Intercept.,row.names = NULL))[,c(1,3)]
  colnames(ran_effect$`ID:site`) <- c("ID","ID_site_effect")
  ran_effect$`ID:site`[,1] <- factor(ran_effect$`ID:site`[,1])
  ran_effect$`ID:site`[,2] <- as.numeric(ran_effect$`ID:site`[,2])
  
  # Prepping site
  ran_effect$site = cbind(rownames(ran_effect$site), 
                          data.frame(ran_effect$site, row.names=NULL))
  colnames(ran_effect$site) <- c("site","site_effect")
  ran_effect$site[,1] <- factor(ran_effect$site[,1])
  ran_effect$site[,2] <- as.numeric(ran_effect$site[,2])
  
  return(ran_effect)
}

for (i in 1:ncol(aov_results)){
  # Reduced dataset for particular edge
  dat <- df[,c("pipeline","filter","atlas","site","ID",edges[i])]
  
  # Rename edge to response to be general
  colnames(dat)[6] <- "response"
  
  # Run edgewise lmer model
  mdl <- lmer(response ~ pipeline * filter * atlas + (1|site/ID),
              data = dat, contrasts = list(pipeline = "contr.sum", 
                                           filter = "contr.sum",
                                           atlas = "contr.sum"))
  # Run anova from model 
  aov_results[,i] <- anova(mdl, type = 3)$`Sum Sq`
  
  # Extract intercept from model for use later
  intercept <- as.numeric(fixef(mdl)[1])
  
  # Extract residuals
  resid <- as.numeric(residuals(mdl))
  
  # Add intercept and residuals as columns in dat
  dat <- cbind(dat,intercept,resid)
  
  # Extract random effect from model for use later
  ran_effect <- extract_random_effect(mdl)
  
  # Merge dataframes by site and by ID so everything matches up
  dat <- merge(dat,ran_effect$site,by="site")
  dat <- merge(dat,ran_effect$`ID:site`,by="ID")
  
  # Create new variable that is the regressed out version
  dat$response_regress <- dat$response-dat$site_effect-dat$ID_site_effect
  
  # Create new variable that is the baseline version
  dat$baseline <- dat$intercept + dat$site_effect + dat$ID_site_effect + dat$resid
  
  # Reduce df down after regressing out random effect
  dat_ranef <- dat[,c("pipeline","filter","atlas","site","ID","response_regress")] 
  colnames(dat_ranef) <- c(colnames(dat_ranef)[1:(length(colnames(dat_ranef))-1)],edges[i])
  
  # Reduce df down after building baseline effect
  dat_base <- dat[,c("pipeline","filter","atlas","site","ID","baseline")] 
  colnames(dat_base) <- c(colnames(dat_base)[1:(length(colnames(dat_base))-1)],edges[i])
  
  # Merge dat with df_id to create df with regressed effect and preserve unique record
  df_regressed <- merge(df_regressed, dat_ranef, by = c("pipeline","filter","atlas","site","ID"))
  df_baseline <- merge(df_baseline, dat_base, by = c("pipeline","filter","atlas","site","ID"))
}

# Export baseline df
save(df_baseline, file = "Data/processed_data_between_network_baseline.RData")

# Export regressed df
save(df_regressed, file = "Data/processed_data_between_network_regressed.RData")

# Change to dataframe
aov_results <- as.data.frame(aov_results)

# Denote between networks
edges_base <- unique(sub("([^_]+_[^_]+)_.*", "\\1", edges))

# Create matrix to store results
network_aov_results <- matrix(NA, nrow = 7, ncol = 21)
colnames(network_aov_results) <- edges_base
rownames(network_aov_results) <- effects

for (i in 1:ncol(network_aov_results)){
  network_aov_results[,i] <- aov_results %>% 
    select(starts_with(edges_base[i])) %>% 
    rowMeans()
}

# Change to dataframe
network_aov_results <- as.data.frame(network_aov_results)

# Add in overall mean column for comparison
network_aov_results$mean <- rowMeans(aov_results)

################################################################################

# Clear environment
rm(list = ls())

######### Full linear mixed model with no NIAK pipeline #########################

# Set wd and load data
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Load in df
load("all_combos_dataframe_network.RData")
within_network <- df

load("all_combos_dataframe_between_network.RData")
between_network <- df

# Merge df
df <- merge(within_network, between_network, by = c("pipeline","filter","atlas","site","ID"))

rm(list = setdiff(ls(), "df"))

# Filter out all instances of NIAK pipeline
df <- df %>% filter(pipeline != "niak")

df$pipeline <- factor(df$pipeline, levels = c("cpac","dparsf","ccs"))

edges <- colnames(df)[-c(1:5)]
effects <- c("pipeline","filter","atlas","pipeline:filter","pipeline:atlas","filter:atlas",
             "pipeline:filter:atlas")

# Empty matrix to store anova results
aov_results <- matrix(NA, nrow = 7, ncol = 435)
colnames(aov_results) <- edges
rownames(aov_results) <- effects

for (i in 1:ncol(aov_results)){
  # Reduced dataset for particular edge
  dat <- df[,c("pipeline","filter","atlas","site","ID",edges[i])]
  
  # Rename edge to response to be general
  colnames(dat)[6] <- "response"
  
  # Run edgewise lmer model
  mdl <- lmer(response ~ pipeline * filter * atlas + (1|site/ID),
              data = dat, contrasts = list(pipeline = "contr.sum", 
                                           filter = "contr.sum",
                                           atlas = "contr.sum"))
  # Run anova from model 
  aov_results[,i] <- anova(mdl, type = 3)$`Sum Sq`
}

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
write.csv(network_aov_results, file = "Results/lmm_remove_niak_results.csv",
          row.names = T)

################################################################################

# Clear environment
rm(list = ls())

######### LMM with additive effects ONLY #########################
# Additive LMM with explanatory variables of pipeline, filter, and atlas
# and a random intercept for each unique ID.

# Set wd and load data
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Load in df
load("all_combos_dataframe_network.RData")
within_network <- df

load("all_combos_dataframe_between_network.RData")
between_network <- df

# Merge df
df <- merge(within_network, between_network, by = c("pipeline","filter","atlas","site","ID"))

rm(list = setdiff(ls(), "df"))

edges <- colnames(df)[-c(1:5)]
effects <- c("pipeline","filter","atlas")

# Empty matrix to store anova results
aov_results <- matrix(NA, nrow = 3, ncol = 435)
colnames(aov_results) <- edges
rownames(aov_results) <- effects

for (i in 1:ncol(aov_results)){
  # Reduced dataset for particular edge
  dat <- df[,c("pipeline","filter","atlas","site","ID",edges[i])]
  
  # Rename edge to response to be general
  colnames(dat)[6] <- "response"
  
  # Run edgewise lmer model - only need ID random effect
  mdl <- lmer(response ~ pipeline + filter + atlas + (1|ID),
              data = dat, contrasts = list(pipeline = "contr.sum", 
                                           filter = "contr.sum",
                                           atlas = "contr.sum"))
  # Run anova from model 
  aov_results[,i] <- anova(mdl, type = 3)$`Sum Sq`
}

# Change to dataframe
aov_results <- as.data.frame(aov_results)

# Denote between networks
edges_base <- unique(sub("([^_]+_[^_]+)_.*", "\\1", edges))
edges_base <- unique(ifelse(grepl("[0-9]$", edges_base),
                            sub("^(.*?)_.*", "\\1", edges_base),
                            edges_base))

# Create matrix to store results
network_aov_results <- matrix(NA, nrow = 3, ncol = 28)
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
write.csv(network_aov_results, file = "Results/lmm_additive_results.csv",
          row.names = T)

################################################################################

# Clear environment
rm(list = ls())

######### Full linear mixed model plus scanner brand #########################

# Set wd and load data
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Load in df
load("all_combos_dataframe_network.RData")
within_network <- df

load("all_combos_dataframe_between_network.RData")
between_network <- df

# Merge df
df <- merge(within_network, between_network, by = c("pipeline","filter","atlas","site","ID"))

rm(list = setdiff(ls(), "df"))

# Load in scanning parameters
scan_param <- read_excel("Functional Scan Parameters/scan_param_summary.xlsx")

scan_param <- scan_param[,1:2]
colnames(scan_param) <- c("site","scanner")

# Merge df and make scanner a factor
df <- merge(df,scan_param,by = "site")
df$scanner <- factor(df$scanner)

edges <- colnames(df)[-c(1:5,441)]
effects <- c("pipeline","filter","atlas","scanner","pipeline:filter","pipeline:atlas","filter:atlas",
             "pipeline:filter:atlas")

# Empty matrix to store anova results
aov_results <- matrix(NA, nrow = 8, ncol = 435)
colnames(aov_results) <- edges
rownames(aov_results) <- effects

for (i in 1:ncol(aov_results)){
  # Reduced dataset for particular edge
  dat <- df[,c("pipeline","filter","atlas","site","ID","scanner",edges[i])]
  
  # Rename edge to response to be general
  colnames(dat)[7] <- "response"
  
  # Run edgewise lmer model
  mdl <- lmer(response ~ pipeline * filter * atlas + scanner + (1|site/ID),
              data = dat, contrasts = list(pipeline = "contr.sum", 
                                           filter = "contr.sum",
                                           atlas = "contr.sum",
                                           scanner = "contr.sum"))
  # Run anova from model 
  aov_results[,i] <- anova(mdl, type = 3)$`Sum Sq`
}

# Change to dataframe
aov_results <- as.data.frame(aov_results)

# Denote between networks
edges_base <- unique(sub("([^_]+_[^_]+)_.*", "\\1", edges))
edges_base <- unique(ifelse(grepl("[0-9]$", edges_base),
                            sub("^(.*?)_.*", "\\1", edges_base),
                            edges_base))

# Create matrix to store results
network_aov_results <- matrix(NA, nrow = 8, ncol = 28)
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
write.csv(network_aov_results, file = "Results/lmm_brand_results.csv",
          row.names = T)

################################################################################