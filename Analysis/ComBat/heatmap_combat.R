# Re-creation of heatmap after ComBat harmonization

library(tidyverse)
library(reshape2)

# base directory that contains all .RData
base_dir <- "C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Correlation_Matrices"

# full paths to the files
file_list <- list.files(path = base_dir, pattern = "\\.RData$", full.names = TRUE, ignore.case = TRUE)

# Extract list of names of all 48 preprocessing combinations
combinations <- gsub("\\..*","", basename(file_list))

# Make a df containing the 48 preprocessing combos to use later
preproc_combo_df <- t(data.frame(strsplit(combinations, split = "_")))[,c(1:2,5)] # cols of interest
rownames(preproc_combo_df) <- NULL
colnames(preproc_combo_df) <- c("pipeline","filter","atlas")

# Load in data after harmonization
load("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/all_combos_dataframe_combat.RData")

# Make list of edges in order for how the results will be vectorized
edges <- c("MPFC:LP_L","MPFC:LP_R","LP_L:LP_R","MPFC:PCC","LP_L:PCC","LP_R:PCC")

# Matrix of heatmap results
mean_edge_weights <- matrix(NA, ncol = 6, nrow = length(file_list))
rownames(mean_edge_weights) <- combinations
colnames(mean_edge_weights) <- edges

# Filter by preprocessing combo and calculate mean - for loop
for (i in 1:nrow(preproc_combo_df)){
  temp <- dat.combat %>% 
    filter(pipeline == preproc_combo_df[i,1] & 
             filter == preproc_combo_df[i,2] & 
             atlas == preproc_combo_df[i,3])
  mean_edge_weights[i,] <- colMeans(temp[,6:11])
}

# reformat result
mean_edge_weights_melt <- melt(mean_edge_weights, 
                               varnames = c("Combination", "Edge"), 
                               value.name = "FisherZ")

# heat map
ggplot(mean_edge_weights_melt, aes(x = Edge, y = Combination, fill = FisherZ)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       limit = c(min(mean_edge_weights),
                                 max(mean_edge_weights)),
                       name = "Fisher's Z-transform") +
  labs(title = "Heatmap of Mean DMN Edges after ComBat Harmonization",
       x = "Edge", y = "Preprocessing Combination")


  



