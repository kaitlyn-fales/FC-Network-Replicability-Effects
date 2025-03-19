################### UMAP 2D Embedding #########################################
# Install packages
#install.packages('umap')
#install.packages('scales')
#install.packages('ggplot2')
#install.packages('deldir')

# Packages
library(umap)
library(scales)
library(ggplot2)
library(deldir)

# This code is to reproduce the UMAP, and the code for the figures is in the 
# Figures folder under the appropriate number. 

# Set directories as needed
#setwd("~/FC-Network-Replicability-Effects")

# Load data after random effects have been regressed out
load("Data/processed_data_ranef.RData")
df <- dat.final

# Separate into labels and data
edge.data <- df[, grep("MPFC|LP_R|PCC", colnames(df))]
pipeline <- df[,"pipeline"]
filter <- df[,"filter"]
atlas <- df[,"atlas"]
site <- df[,"site"]

# Create umap
custom.config <- umap.defaults
custom.config$random_state <- 1234 # seed for reproducibility
edges.umap <- umap(edge.data, config = custom.config)
edges.umap

# Export full umap for later use
save(edges.umap, file = "Analysis/UMAP/umap_result.RData")






