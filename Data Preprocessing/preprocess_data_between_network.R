### Data Preprocessing - Creating a long form dataframe from the FC matrices ###

# Install packages
#install.packages('stringr')
#install.packages('tidyverse')
#install.packages('readxl')
#install.packages('reshape2')

# Packages
library(stringr)
library(tidyverse)
library(readxl)
library(reshape2)

######### Setting things up - prepping data ##################################
# Base directory that contains all .RData, change if needed
#setwd("~/FC-Network-Replicability-Effects")
base_dir <- paste0(getwd(),"/Data/Correlation_Matrices")

# Full paths to the files
file_list <- list.files(path = base_dir, pattern = "\\.RData$", full.names = TRUE, ignore.case = TRUE)

# Extract list of names of all 56 preprocessing combinations
combinations <- gsub("\\..*","", basename(file_list))

# Make a df containing the 56 preprocessing combos to use later
preproc_combo_df <- t(data.frame(strsplit(combinations, split = "_")))[,c(1:2,5)] # cols of interest
rownames(preproc_combo_df) <- NULL
colnames(preproc_combo_df) <- c("pipeline","filter","atlas")

# Incorporating metadata to get ID column - pulling directory and files
metadata_dir <- paste0(getwd(),"/Preprocessed Data/Metadata")
metadata_files <- list.files(path = metadata_dir, pattern = "\\.RData$", 
                             full.names = TRUE, ignore.case = TRUE)

# Loading files
for (i in 1:length(metadata_files)) load(metadata_files[i])

# Function to extract the unique ID column from the corresponding metadata file
extract_meta <- function(combo, atlas) {
  # Grab metadata file according to which atlas for that combination
  type <- atlas
  
  if (type == "aal") {meta <- meta_aal} else {
    if  (type == "cc200") {meta <- meta_cc200} else {
      if  (type == "cc400") {meta <- meta_cc400} else {
        if (type == "dosenbach160") {meta <- meta_dos} else {
          if  (type == "ez") {meta <- meta_ez} else {
            if  (type == "ho") {meta <- meta_ho} 
            else {meta <- meta_tt
            }
          }
        }
      }
    }
  }
  
  # Extract list of included files
  included_files <- meta[[combo]]$included_files
  
  # Split the string into just the file name (get rid of path)
  included_files <- t(data.frame(strsplit(included_files, split = "/")))[,3]
  
  # Split the string by getting rid of everything after the ID
  included_files <- gsub("_rois.*","\\1",included_files)
  
  # Combine CMU_a and CMU_b to just be CMU (small sample size)
  included_files <- gsub("CMU_a","CMU",included_files)
  included_files <- gsub("CMU_b","CMU",included_files)
  
  # Combine MaxMun_a, MaxMun_b, and MaxMun_d to just be MaxMun (small sample size)
  included_files <- gsub("MaxMun_a","MaxMun",included_files)
  included_files <- gsub("MaxMun_c","MaxMun",included_files)
  included_files <- gsub("MaxMun_d","MaxMun",included_files)
  
  ### For sites with multiple samples, change "_" syntax
  
  # Replacing Leuven_1 with Leuven1 (same for 2)
  included_files <- gsub("Leuven_1","Leuven1",included_files)
  included_files <- gsub("Leuven_2","Leuven2",included_files)
  
  # Replacing UCLA_1 with UCLA1 (same for 2)
  included_files <- gsub("UCLA_1","UCLA1",included_files)
  included_files <- gsub("UCLA_2","UCLA2",included_files)
  
  # Replacing UM_1 with UM1 (same for 2)
  included_files <- gsub("UM_1","UM1",included_files)
  included_files <- gsub("UM_2","UM2",included_files)
  
  ###
  
  # Split string by _ character
  included_files <- t(data.frame(strsplit(included_files, split = "_")))
  rownames(included_files) <- NULL
  colnames(included_files) <- c("site","ID")
  
  return(included_files)
}

# Function to extract and organize between network correlations from one matrix
get_between_network_corr <- function(cor_matrix){
  # Get all unique pairs of variable names
  pairs <- expand.grid(Var1 = colnames(cor_matrix), Var2 = colnames(cor_matrix))
  
  # Filter to keep only unique pairs (avoiding duplicates like A-B and B-A) and exclude self-correlations
  unique_pairs <- pairs[as.numeric(pairs$Var1) > as.numeric(pairs$Var2), ]
  
  # Extract correlations for these unique pairs
  vectorized_cor_alt <- data.frame(
    Var1 = unique_pairs$Var1,
    Var2 = unique_pairs$Var2,
    Correlation = mapply(function(x, y) cor_matrix[x, y], unique_pairs$Var1, unique_pairs$Var2)
  )
  
  # Exclude rows where network is present in both col1 and col2 (we want between networks)
  for (k in 1:length(networks)){
    vectorized_cor_alt <- vectorized_cor_alt[!(grepl(networks[k], vectorized_cor_alt$Var1) & 
                                                 grepl(networks[k], vectorized_cor_alt$Var2)), ]
  }
  
  # Create edge column for between networks
  vectorized_cor_alt$edge <- paste(sub("\\..*", "", vectorized_cor_alt$Var1),
                                   sub("\\..*", "", vectorized_cor_alt$Var2), 
                                   sep = "_")
  
  # Sort by group
  vectorized_cor_alt <- vectorized_cor_alt[order(vectorized_cor_alt$edge),]
  
  # Add unique edge id column by group
  vectorized_cor_alt <- vectorized_cor_alt %>%
    group_by(edge) %>%
    mutate(id = row_number())
  
  # Put together id column and edge column
  vectorized_cor_alt$edge <- paste(vectorized_cor_alt$edge,vectorized_cor_alt$id, sep = "_")
  
  # Extract output
  output <- vectorized_cor_alt %>%
    select(edge, Correlation) %>%
    pivot_wider(names_from = edge, values_from = Correlation)
  
  return(output)
}

# Function to take in a list from 1 of 56 combos, vectorize and Fisher z-transform
fisher_transform <- function(cor_matrices){
  
  # Initialize empty list to store dataframes
  result_list <- list()
  
  # Run get_between_network_corr function for each subject and add to list
  for (j in 1:length(cor_matrices)){
    result_list[[j]] <- get_between_network_corr(cor_matrices[[j]])
  }
  
  # Take list and transform into one large dataframe
  result <- do.call(rbind, result_list)
  
  # Fisher's z-transform
  output <- atanh(result)
  
  # Return output
  return(output)
}

# Create empty list to store results from for loop
result_list <- list()

# For loop to run across all 56 combinations to make master dataframe
for (i in 1:length(file_list)){
  # Load in individual file
  load(file_list[i])
  
  # Pull the name of the file into generic form
  temp <- get(combinations[i])
  
  # Network names
  networks <- unique(str_extract(colnames(temp[[1]]), "^[^\\.]+"))
  
  # Apply the fisher_transform function
  fisher_result <- fisher_transform(temp)
  
  # Take the relevant preprocessing combo and make into design matrix
  combo <- matrix(preproc_combo_df[i,], nrow = nrow(fisher_result), 
                  ncol = ncol(preproc_combo_df), byrow = T)
  
  # Extract site and unique ID column from corresponding metadata file
  metadata <- extract_meta(combinations[i],preproc_combo_df[i,3])
  
  # Make dataframe for that particular combo, plus ID column, and response values
  result_list[[i]] <- cbind(combo, metadata, fisher_result)
}

# Take list and transform into one large dataframe
df <- do.call(rbind, result_list)

# Add in column names
colnames(df)[1:3] <- c("pipeline","filter","atlas")

# Get rid of CMU as site since we only have one entry
df <- df[!grepl("CMU", df$site),]

# Convert preprocessing effects as factors
df$pipeline <- factor(df$pipeline, levels = c("cpac","dparsf","niak","ccs"))
df$filter <- factor(df$filter)
df$atlas <- gsub("dosenbach160","dos160",df$atlas)
df$atlas <- factor(df$atlas, levels = c("cc200","cc400","dos160","ez","ho","tt","aal"))
df$site <- factor(df$site)
df$ID <- factor(df$ID)

# Convert network columns to numeric
df[, 6:ncol(df)] <- lapply(df[, 6:ncol(df)], as.numeric)

# Check structure
str(df)

# Export final dataframe to reference later
save(df, 
     file = paste0(getwd(),"/Data/processed_data_between_network.RData"))


