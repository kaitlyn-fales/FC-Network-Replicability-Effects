################## ComBat Harmonization and Analysis #########################

# Working directory
#setwd("~/FC-Network-Replicability-Effects")

# Packages
library(neuroCombat)
library(stringr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(readxl)
library(reshape2)
library(umap)

############### ComBat Harmonization for site batch effect ###################

# Load in df
load("Data/processed_data_network.RData")
within_network <- df

load("Data/processed_data_between_network.RData")
between_network <- df

# Merge df
df <- merge(within_network, between_network, by = c("pipeline","filter","atlas","site","ID"))

# Prepare data in format needed for Combat
dat <- df[,-c(1:5)] # separate FC vectors
dat <- t(dat) # transpose so that matrix is pxn
batch <- df[,"site"] # batch variable for site id

pipeline <- df[,"pipeline"] # variables we want to preserve
filter <- df[,"filter"]
atlas <- df[,"atlas"]

mod <- model.matrix(~pipeline+filter+atlas) # put in model matrix

# Use full Combat to harmonize the data across site
data.harmonized <- neuroCombat(dat=dat, batch=batch, mod = mod)

# Put original data format back together for export
dat.combat <- data.frame(df[,1:5],t(data.harmonized$dat.combat))

# Export to data folder
save(dat.combat, file = "Data/processed_data_combat.RData")

################################################################################

# Clear environment
rm(list = ls())

############### Full LMM with ComBat adjusted data ###################

# Load data
load("Data/processed_data_combat.RData")

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
save(df_regressed, file = "Data/processed_data_combat_regressed.RData")

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

################################################################################

# Clear environment
rm(list = ls())

############### UMAP with ComBat adjusted data ###################

# Load data
load("Data/processed_data_combat_regressed.RData")
df <- df_regressed

# Separate into labels and data
edge.data <- df[,-c(1:5)]
pipeline <- df[,"pipeline"]
atlas <- df[,"atlas"]

# Create umap
custom.config <- umap.defaults
custom.config$random_state <- 1234 # seed for reproducibility
edges.umap <- umap(edge.data, config = custom.config)
edges.umap

# ggplots
pipeline <- as.factor(toupper(pipeline))
levels(pipeline) <- c("CCS","CPAC","DPARSF","NIAK")
atlas <- as.factor(toupper(atlas))
levels(atlas) <- c("AAL",
                   "CC200",
                   "CC400",
                   "DOS160",
                   "EZ",
                   "HO",
                   "TT")
umap.dat <- data.frame(pipeline,atlas,edges.umap$layout)

# pipeline - not faceted 
umap.dat %>% ggplot() +
  geom_point(mapping = aes(x = X1, y = X2, colour = pipeline), alpha = 0.25) + 
  scale_colour_manual(values = c("#A4036F","#00916E","#FFCF00","#A79AB2")) +
  labs(title = "Pipeline", colour = "Pipeline") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          axis.text = element_text(size = 11),
                          legend.position = "inside",
                          legend.position.inside = c(0.09,0.8),
                          plot.title = element_text(size = 16),
                          legend.title = element_blank(),
                          legend.background = element_rect(fill = "transparent"),
                          legend.text = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = aes(size = 3, alpha = 1)))

# pipeline - faceted
umap.dat %>% ggplot(mapping = aes(x = X1, y = X2, colour = pipeline)) +
  geom_point(alpha = 0.15) + 
  scale_colour_manual(values = c("#A4036F","#00916E","#FFCF00","#A79AB2")) +
  labs(title = "Pipeline Faceted") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "none",
                          plot.title = element_text(size = 16)) +
  facet_wrap(~pipeline)

# atlas - not faceted 
umap.dat %>% ggplot() +
  geom_point(mapping = aes(x = X1, y = X2, colour = atlas), alpha = 0.4) + 
  scale_color_manual(values = c("#00E8FC","#FFA3AF","#78290F","#A23B72","#FF7D00","#15616D","#00134D")) +
  labs(title = "Atlas", colour = "Atlas") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "inside",
                          legend.position.inside = c(0.07,0.85),
                          plot.title = element_text(size = 16),
                          legend.title = element_blank(),
                          legend.background = element_rect(fill = "transparent"),
                          legend.text = element_text(size = 10),
                          axis.text = element_text(size = 11)) +
  guides(colour = guide_legend(override.aes = aes(size = 3, alpha = 1)))

# atlas - faceted
umap.dat %>% ggplot(mapping = aes(x = X1, y = X2, colour = atlas)) +
  geom_point(alpha = 0.15) + 
  scale_color_manual(values = c("#00E8FC","#FFA3AF","#78290F","#A23B72","#FF7D00","#15616D","#00134D")) +
  labs(title = "Atlas Faceted") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "none",
                          plot.title = element_text(size = 16)) +
  facet_wrap(~atlas)

################################################################################









