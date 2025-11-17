##################### Exploratory Analysis (Figures 1-2) #######################

# Packages
library(ggplot2)
library(reshape2)
library(stringr)
library(tibble)
library(corrplot)
library(dichromat)
library(dplyr)


################# Figure 1: Heatmap #####################

# Base directory
#setwd("~/FC-Network-Replicability-Effects")
base_dir <- paste0(getwd(),"/Data/Correlation_Matrices")

# full paths to the files
file_list <- list.files(path = base_dir, pattern = "\\.RDATA$", full.names = TRUE, ignore.case = TRUE)

# Extract list of names to use as column names for eventual heatmap 
combinations <- gsub("\\..*","", basename(file_list))

# Matrix of heatmap results
mean_edge_weights <- matrix(NA, ncol = 54, nrow = length(file_list))
rownames(mean_edge_weights) <- combinations

# Function for getting mean Fisher z-transformed values for a preprocessing combination
# This function will return a vector of length 6 that will be one row in the heatmap
fisher_transform <- function(cor_matrices, networks){
  
  for (k in 1:length(networks)){
    network_ind <- grep(networks[k], colnames(cor_matrices[[1]])) # same column/row names for all cor_matrices
    
    # Empty matrix to store result
    result <- matrix(NA, nrow = length(network_ind)*(length(network_ind)-1)/2, 
                     ncol = length(cor_matrices))
    
    # Take each FC matrix and vectorize the upper triangle
    for (j in 1:length(cor_matrices)){
      cor_matrix <- cor_matrices[[j]][network_ind,network_ind]
      r.vector <- cor_matrix*upper.tri(cor_matrix, diag = F)
      result[,j] <- r.vector[r.vector != 0]
    }
    
    # Fisher's z-transform and assign as network
    result <- atanh(result)
    ind <- c(1:nrow(result))
    rownames(result) <- paste(networks[k], ind, sep = "_")
    assign(networks[k], result)
  }
  
  results <- rbind(DefaultMode,SensoriMotor,Visual,Salience,DorsalAttention,
                   FrontoParietal,Language)
  
  # Collapse result to get single column - take mean of each row
  out <- rowMeans(results)
  
  # Return output
  return(out)
}

# For loop to run across all 48 combinations
for (i in 1:length(file_list)){
  # Load in individual file
  load(file_list[i])
  
  # Pull the name of the file into generic form
  temp <- get(combinations[i])
  
  # Network names
  networks <- unique(str_extract(colnames(temp[[1]]), "^[^\\.]+"))
  
  # Apply the fisher_transform function
  result <- fisher_transform(temp, networks)
  mean_edge_weights[i,] <- result
  colnames(mean_edge_weights) <- names(result)
  
  # Remove from environment
  rm(list = combinations[i])
  rm(temp)
}

########## Figure 1a: Pipeline ordering pane 
mean_edge_weights_melt <- melt(mean_edge_weights, 
                               varnames = c("combination", "Edge"), 
                               value.name = "FisherZ")

# Restructuring names
names <- t(data.frame(strsplit(as.character(mean_edge_weights_melt$combination), split = "_")))[,c(1:2,5)]
rownames(names) <- NULL
names[,c(1,3)] <- toupper(names[,c(1,3)])
atlas <- unique(names[,3])
atlas <- gsub('DOSENBACH160','DOS160',atlas)

# heat map
ggplot(mean_edge_weights_melt, aes(x = Edge, y = combination, fill = FisherZ)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       limit = c(min(mean_edge_weights),
                                 max(mean_edge_weights)),
                       name = "Fisher's Z") +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = rep(atlas,8)) +
  labs(x = "", y = "") +
  theme(legend.text = element_text(size = 11,face = "bold"),
        legend.title = element_text(size = 12,face = "bold"),
        axis.title.x = element_text(size = 12,face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,face = "bold",colour = "black")) +
  geom_hline(yintercept = c(14.5,28.5,42.5), linewidth = 1.25) +
  geom_hline(yintercept = c(7.5,21.5,35.5,49.5), linewidth = 0.75, linetype = 2) +
  geom_vline(xintercept = c(6.5,9.5,15.5,36.5,42.5,48.5), linewidth = 1.25)

########### Figure 1b: Altas ordering pane
# Restructuring names
names <- t(data.frame(strsplit(as.character(mean_edge_weights_melt$combination), split = "_")))[,c(1:2,5)]
names[,c(1,3)] <- toupper(names[,c(1,3)])
names <- gsub('filt','Filtering',names)
names <- gsub('noFiltering','No Filtering',names)
names <- gsub('DOSENBACH160','DOS160',names)
mean_edge_weights_melt$names <- paste(names[,3],names[,2],names[,1],sep = ", ")

pipeline <- unique(names[,1])

# Order df based on atlas now
mean_edge_weights_melt <- mean_edge_weights_melt[order(mean_edge_weights_melt$names), ]

# heat map
ggplot(mean_edge_weights_melt, aes(x = Edge, y = names, fill = FisherZ)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       limit = c(min(mean_edge_weights),
                                 max(mean_edge_weights)),
                       name = "Fisher's Z") +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = rep(pipeline,14)) +
  labs(x = "", y = "") +
  theme(legend.text = element_text(size = 11,face = "bold"),
        legend.title = element_text(size = 12,face = "bold"),
        axis.title.x = element_text(size = 12,face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,face = "bold",colour = "black")) +
  geom_hline(yintercept = c(8.5,16.5,24.5,32.5,40.5,48.5), linewidth = 1.25) +
  geom_hline(yintercept = c(4.5,12.5,20.5,28.5,36.5,44.5,52.5), linewidth = 0.75, linetype = 2) +
  geom_vline(xintercept = c(6.5,9.5,15.5,36.5,42.5,48.5), linewidth = 1.25)

########## Figure 1c: Hierarchical clustering pane
dist_mat <- dist(mean_edge_weights, method = "euclidean")
set.seed(1234)
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, h = 1.5)
cut_avg <- data.frame(cut_avg)
cut_avg <- rownames_to_column(cut_avg, var = "combination")
plot(hclust_avg)
rect.hclust(hclust_avg , h = 1.5, border = 2:6)

# Export clustering cut data for baseline use later
save(cut_avg, file = paste0(getwd(),"/Analysis/Exploratory Analysis/hclust_cuts.RData"))

mean_edge_weights_df <- data.frame(mean_edge_weights)
mean_edge_weights_df <- rownames_to_column(mean_edge_weights_df, var = "combination")

mean_edge_weights_df <- merge(mean_edge_weights_df, cut_avg, by = "combination")
mean_edge_weights_df <- mean_edge_weights_df[order(mean_edge_weights_df$cut_avg), ]

# Heatmap with full names
mean_edge_weights_melt <- melt(mean_edge_weights_df, 
                               id.vars = c("combination","cut_avg"),
                               variable.name = "edge", 
                               value.name = "FisherZ")

# Restructuring names
names <- t(data.frame(strsplit(as.character(mean_edge_weights_melt$combination), split = "_")))[,c(1:2,5)]
names[,c(1,3)] <- toupper(names[,c(1,3)])
names <- gsub('filt','Filtering',names)
names <- gsub('noFiltering','No Filtering',names)
names <- gsub('DOSENBACH160','DOS160',names)
mean_edge_weights_melt$names <- paste(names[,1],names[,2],names[,3],sep = ", ")
names_levels <- mean_edge_weights_melt$names[1:56]
mean_edge_weights_melt$names <- factor(mean_edge_weights_melt$names, 
                                       levels = names_levels)

# heat map
ggplot(mean_edge_weights_melt, aes(x = edge, y = names, fill = FisherZ)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       limit = c(min(mean_edge_weights),
                                 max(mean_edge_weights)),
                       name = "Fisher's Z") +
  labs(x = "", y = "Processing Combination", 
       title = "") +
  geom_vline(xintercept = c(6.5,9.5,15.5,36.5,42.5,48.5), linewidth = 1.25) +
  theme(legend.text = element_text(size = 11,face = "bold"),
        legend.title = element_text(size = 12,face = "bold"),
        axis.title.y = element_text(size = 12,face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,face = "bold",colour = "black")) +
  geom_hline(yintercept = c(24.5,36.5,42.5,50.5), linewidth = 0.75, linetype = 2) 
################################################################################

# Clear environment
rm(list = ls())

################# Figure 2: Avg FC Matrix #####################

# Load hierarchical clustering assignments
load("Analysis/Exploratory Analysis/hclust_cuts.RData")

# Reduce to 28 cluster options since filtering does not have pronounced effect
clust <- cut_avg[!grepl("nofilt",cut_avg$combination),]

# Base directory for correlation matrices
#setwd("~/FC-Network-Replicability-Effects")
base_dir <- paste0(getwd(),"/Data/Correlation_Matrices")

# Full paths to the files
file_list <- list.files(path = base_dir, pattern = "\\.RData$", full.names = TRUE, ignore.case = TRUE)
file_list <- file_list[!grepl("nofilt",file_list)]

# Extract list of names of all 28 preprocessing combinations
combos <- clust$combination

# Randomly sample one from each cluster
set.seed(1234)
baseline_samp <- clust %>% group_by(cut_avg) %>% slice_sample(n = 1)

par(mfrow = c(2,3))

# Run through each file and calculate mean
for (i in 1:nrow(baseline_samp)){
  file <- grep(baseline_samp$combination[i], file_list, value = TRUE)
  load(file)
  temp <- get(baseline_samp$combination[i])
  
  arr <- simplify2array(temp)
  mean_matrix <- apply(arr, MARGIN = c(1,2), FUN = mean)
  
  corrplot(mean_matrix, method = "color", is.corr = T, 
           col = colorRampPalette(c("blue","white","red"))(200),
           main = paste0(baseline_samp$combination[i]),
           tl.pos = 'n',mar = c(0, 0, 3, 0))
}

################################################################################