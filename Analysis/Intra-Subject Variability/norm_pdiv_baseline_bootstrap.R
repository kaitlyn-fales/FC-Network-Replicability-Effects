############# Baseline by non-parametric bootstrap #########
# Import portrait divergence functions from github: https://github.com/bagrow/network-portrait-divergence

# Packages
library(abind)
library(tidyverse)
library(foreach)
library(doParallel)
library(reticulate)

# Working directory
#setwd("~/FC-Network-Replicability-Effects")

# Load in df
load("Data/processed_data_network_baseline.RData")
within_network <- df_baseline

load("Data/processed_data_between_network_baseline.RData")
between_network <- df_baseline

# Merge df
df <- merge(within_network, between_network, by = c("pipeline","filter","atlas","site","ID"))

# Disregard combinations that use no filtering - not important enough effect, drop site (not used)
df <- df %>% filter(filter == "filt") %>% select(-c(site,filter))

# Make new variable for treatment - pipeline + atlas combination
df$combo <- paste0(df$pipeline,"_",df$atlas)

# Drop pipeline and atlas and reorder combo
df <- df %>% select(-c(pipeline,atlas)) %>% relocate(combo)

# Pivot df and reform into array to make structure easier to bootstrap
df <- df %>% pivot_longer(cols = -c(1:2), names_to = "edge", values_to = "response") # pivot longer to undo edge
df <- df %>% pivot_wider(names_from = combo, values_from = response) # pivot wider to make columns by combo

df_array <- abind(split(df, df$edge), along = 3) # reform into array
dim(df_array) # final dimensions of array are (subject, combination, network edge)

# Clear environment except for df_array
rm(list = setdiff(ls(), "df_array"))

# Load upper triangle labels to reconstruct correlation matrix
load("Analysis/Intra-Subject Variability/upper_triangle_labels.RData")

# Get number of cores in machine for parallel computing
n_cores <- detectCores()

# Register cluster
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)

# Set seed
set.seed(1234)

# Setting sample size
n <- dim(df_array)[1]

# Setting treatment size
p <- dim(df_array)[2] - 2

# Setting number of bootstrap replicates
B <- 100

########### Functions #######################

# Function to calculate difference in Frobenius norms for a given pair of combinations
calc_frobenius <- function(col, subject_data){
  combo_pair <- strsplit(col, "__")[[1]]
  idx1 <- unlist(strsplit(combo_pair[1], "\\."))
  idx2 <- unlist(strsplit(combo_pair[2], "\\."))
  
  data1 <- subject_data %>%
    filter(pipeline == idx1[1] & atlas == idx1[2]) %>%
    select(-c(1:3))
  
  data2 <- subject_data %>%
    filter(pipeline == idx2[1] & atlas == idx2[2]) %>%
    select(-c(1:3))
  
  if (nrow(data1) == 1 && nrow(data2) == 1) {
    A <- as.matrix(data1)
    B <- as.matrix(data2)
    output <- norm(A - B, type = "F")
  }
  return(output)
}

# Function to apply calc_frobenius to all pairwise combinations for a particular subject
sub_frobenius <- function(x){
  subject <- x[["ID"]]
  subject_data <- subset(df, ID == subject)
  
  output <- results %>% filter(ID == subject) %>% 
    mutate(across(-ID, 
                  .fns = ~calc_frobenius(
                    col = cur_column(), 
                    subject_data = subject_data)))
  
  return(output)
}

# Function to take input data vector and transform back into weighted adjacency matrix
get_cor_mat <- function(input_dat){
  # Extract single vector of z-transformed values corresponding to one network
  cor_vec <- data.frame(t(input_dat))
  colnames(cor_vec) <- "response"
  cor_vec$key <- rownames(cor_vec)
  rownames(cor_vec) <- NULL
  
  # Merge with labels
  cor_matrix_labs <- merge(upper_tri_labs,cor_vec, by = "key")
  
  # Fix ordering
  cor_matrix_labs <- cor_matrix_labs[order(cor_matrix_labs$original_order), ]
  rownames(cor_matrix_labs) <- NULL
  
  # Create an empty n x n matrix
  adj_mat <- matrix(0, nrow = 30, ncol = 30)
  
  # Fill the lower triangle with the vector values
  adj_mat[upper.tri(adj_mat)] <- cor_matrix_labs$response
  
  # Make the matrix symmetric by adding its transpose
  adj_mat <- adj_mat + t(adj_mat) 
  
  # Make any negative values equal to 0 (very few of these) - network alg can't handle negative weights
  adj_mat <- pmax(adj_mat, 0)
  
  return(adj_mat)
}

# Function to calculate portrait divergence for a given pair of combinations
calc_pdiv <- function(col, subject_data){
  combo_pair <- strsplit(col, "__")[[1]]
  idx1 <- unlist(strsplit(combo_pair[1], "\\."))
  idx2 <- unlist(strsplit(combo_pair[2], "\\."))
  
  # Import for use within parallel process - CHANGE PATHS AS NEEDED
  nx <- reticulate::import("networkx")
  pdiv <- reticulate::import_from_path("portrait_divergence", 
                                       path = "~/FC-Network-Replicability-Effects/Analysis/Intra-Subject Variability")
  
  data1 <- subject_data %>%
    filter(pipeline == idx1[1] & atlas == idx1[2]) %>%
    select(-c(1:3))
  
  data2 <- subject_data %>%
    filter(pipeline == idx2[1] & atlas == idx2[2]) %>%
    select(-c(1:3))
  
  if (nrow(data1) == 1 && nrow(data2) == 1) {
    adj_mat1 <- get_cor_mat(data1)
    adj_mat2 <- get_cor_mat(data2)
    
    g1 <- nx$from_numpy_array(adj_mat1)
    g2 <- nx$from_numpy_array(adj_mat2)
    
    output <- pdiv$portrait_divergence_weighted(g1, g2)
  } else {
    output <- NA
  }
  return(output)
}

# Function to apply calc_pdiv to all pairwise combinations for a particular subject
sub_pdiv <- function(x){
  subject <- x[["ID"]]
  subject_data <- subset(df, ID == subject)
  
  output <- results %>% filter(ID == subject) %>% 
    mutate(across(-ID, 
                  .fns = ~calc_pdiv(
                    col = cur_column(), 
                    subject_data = subject_data)))
  
  return(output)
}

#############################################

# Begin bootstrapping process
for (b in 1:B){
    subjects_b <- sample(1:n, size = n, replace = T)
    df_array_boot <- df_array[subjects_b,,]
    
    for (i in 1:n){
      treatments_ib <- sample(3:(p+2), size = p, replace = T) # resample non-id columns
      df_array_boot[i,,] <- df_array_boot[i,c(1:2,treatments_ib),] # create bootstrapped array
    }
    
    # Reassign old unique ID to bootstrapped df to have a unique ID and replicate structure
    df_array_boot[,1,] <- df_array[,1,]
    
    # Transform array back into original df structure
    df_boot <- apply(df_array_boot, 3, as.data.frame)
    df_boot <- do.call(rbind, df_boot)
    
    # Pivot df back to original structure
    df_boot <- df_boot %>% pivot_longer(cols = -c(1:2), names_to = "combo", values_to = "response") # pivot longer to undo combo
    df_boot <- df_boot %>% pivot_wider(names_from = edge, values_from = response) # pivot wider to make columns by combo
    
    # Recreate pipeline and atlas columns
    df_boot <- df_boot %>% separate(col = combo,
                                    into = c("pipeline","atlas"),
                                    sep = "_")
    
    # Convert preprocessing effects as factors
    df_boot$pipeline <- factor(df_boot$pipeline, levels = c("cpac","dparsf","niak","ccs"))
    df_boot$atlas <- factor(df_boot$atlas, levels = c("cc200","cc400","dos160","ez","ho","tt","aal"))
    df_boot$ID <- factor(df_boot$ID)
    
    # Convert network columns to numeric
    df_boot[, 4:ncol(df_boot)] <- lapply(df_boot[, 4:ncol(df_boot)], as.numeric)
    
    # Export bootstrapped data
    save(df_boot, file = paste0("Analysis/Intra-Subject Variability/Bootstrap Data/processed_data_bootstrap",b,".RData"))
    
    # Rename
    df <- df_boot
    
    # Get unique subject IDs
    subjects <- unique(df$ID)
    
    # Create a data frame to store the results
    results <- data.frame(ID = subjects)
    
    # Get unique combinations of pipeline, and atlas
    combinations <- unique(df[, c("pipeline", "atlas")])
    
    # Generate unique pairwise combinations of the 28 combinations
    combo_indices <- combn(seq_len(nrow(combinations)), 2)
    
    combo_names <- apply(combo_indices, 2, function(idx) {
      combo1 <- combinations[idx[1], ]
      combo2 <- combinations[idx[2], ]
      paste(paste(combo1$pipeline, combo1$atlas, sep = "."),
            paste(combo2$pipeline, combo2$atlas, sep = "."),
            sep = "__")
    })
    
    # Initialize the results matrix with NA
    results_matrix <- matrix(NA, nrow = length(subjects), ncol = length(combo_names))
    colnames(results_matrix) <- combo_names
    
    # Add ID column to the results matrix and convert to df
    results <- data.frame(ID = subjects, results_matrix)
    
    # Split the dataframe into a list of smaller dataframes for parallel computing
    data_chunks <- split(results, cut(1:nrow(results), n_cores-1))
    
    # Compute the difference in Frobenius norm and pdiv for each pairwise combination across all subjects in parallel
    results <- foreach(chunk = data_chunks, 
                       .combine = c, 
                       .packages = 'dplyr',
                       .export = c('calc_frobenius','calc_pdiv',
                                   'get_cor_mat','upper_tri_labs')) %dopar% {
                                     result_norm <- apply(chunk, MARGIN = 1, FUN = sub_frobenius)
                                     result_pdiv <- apply(chunk, MARGIN = 1, FUN = sub_pdiv)
                                     
                                     list(result_norm = result_norm, result_pdiv = result_pdiv)
                                   }
    
    # Change format of results from list to df
    results_norm = results[seq(1, length(results), by = 2)]
    results_norm = lapply(results_norm, function(x) do.call(rbind, x))
    results_norm = do.call(rbind, results_norm)
    
    # Save the results to a CSV file
    write.csv(results_norm, file = paste0("Analysis/Intra-Subject Variability/Bootstrap Results/frobenius_norm_bootstrap",b,".csv"), row.names = FALSE)
    
    # Change format of results from list to df
    results_pdiv = results[seq(2, length(results), by = 2)]
    results_pdiv = lapply(results_pdiv, function(x) do.call(rbind, x))
    results_pdiv = do.call(rbind, results_pdiv)
    
    # Save the results to a CSV file
    write.csv(results_pdiv, file = paste0("Analysis/Intra-Subject Variability/Bootstrap Results/pdiv_bootstrap",b,".csv"), row.names = FALSE)

}

# Stop cluster after parallel computing is completed
stopCluster(cl = cluster)
