### Data Preprocessing - Creating a long form dataframe from the FC matrices ###

# Install packages
#install.packages('stringr')
#install.packages('tidyverse')
#install.packages('readxl')

# Packages
library(stringr)
library(tidyverse)
library(readxl)

######### Setting things up - prepping data ##################################
# Base directory that contains all .RData, change if needed
#setwd("~/FC-Network-Replicability-Effects")
base_dir <- paste0(getwd(),"/Data/Correlation_Matrices")

# Full paths to the files
file_list <- list.files(path = base_dir, pattern = "\\.RData$", full.names = TRUE, ignore.case = TRUE)

# Extract list of names of all 48 preprocessing combinations
combinations <- gsub("\\..*","", basename(file_list))

# Make a df containing the 48 preprocessing combos to use later
preproc_combo_df <- t(data.frame(strsplit(combinations, split = "_")))[,c(1:2,5)] # cols of interest
rownames(preproc_combo_df) <- NULL
colnames(preproc_combo_df) <- c("pipeline","filter","atlas")

# Make list of edges in FC network
edges <- c("MPFC.LP_L","MPFC.LP_R","LP_L.LP_R","MPFC.PCC","LP_L.PCC","LP_R.PCC")

# Incorporating metadata to get ID column - pulling directory and files
metadata_dir <- paste0(getwd(),"/Data Preprocessing/Metadata")
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
        if  (type == "ez") {meta <- meta_ez} else {
          if  (type == "ho") {meta <- meta_ho} 
          else {meta <- meta_tt
          }
        }
      }
    }
  }
  
  # Extract list of included files
  included_files <- meta[[combo]]$included_files
  
  # Split the string into just the file name (get rid of path)
  included_files <- t(data.frame(strsplit(included_files, split = "/")))[,3]
  
  # Split the string to get rid of combination
  included_files <- gsub(paste0(combo,"_"),"",included_files)
  
  # Split the string by getting rid of everything after the ID - get rid of file extension
  included_files <- gsub("\\..*", "",included_files)
  
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

# Function to take in a list from 1 of 48 combos, vectorize and Fisher z-transform
fisher_transform <- function(cor_matrices){
  # Empty matrix to store result
  result <- matrix(NA, nrow = length(cor_matrices), ncol = 6)
  
  # Take each FC matrix and vectorize the upper triangle
  for (j in 1:length(cor_matrices)){
    r.vector <- cor_matrices[[j]]*upper.tri(cor_matrices[[j]], diag = F)
    result[j,] <- r.vector[r.vector != 0]
  }
  
  # Fisher's z-transform
  result <- atanh(result)
  
  # Return output
  return(result)
}

# Create empty matrix to store results from for loop (will need to get rid of extra NA at end)
df <- matrix(NA, nrow = 571*48, ncol = 11)
colnames(df) <- c("pipeline","filter","atlas","site","ID",edges)

# Starting running count for filling in df
count <- 0

# For loop to run across all 48 combinations to make master dataframe
for (i in 1:length(file_list)){
  # Load in individual file
  load(file_list[i])
  
  # Pull the name of the file into generic form
  temp <- get(combinations[i])
  
  # Apply the fisher_transform function
  fisher_result <- fisher_transform(temp)
  
  # Take the relevant preprocessing combo and make into design matrix
  combo <- matrix(preproc_combo_df[i,], nrow = nrow(fisher_result), 
                  ncol = ncol(preproc_combo_df), byrow = T)
  
  # Extract site and unique ID column from corresponding metadata file
  metadata <- extract_meta(combinations[i],preproc_combo_df[i,3])
  
  # Make dataframe for that particular combo, plus ID column, and response values
  dat_combo <- cbind(combo, metadata, fisher_result)
  
  # Add dat_combo to final dataframe
  df[(count+1):(count+nrow(dat_combo)),] <- dat_combo
  
  # Update running count of which rows we are at
  count <- count+nrow(dat_combo)
}

# Convert final matrix to dataframe
df <- as.data.frame(df)

# Convert preprocessing effects as factors
df$pipeline <- factor(df$pipeline, levels = c("cpac","dparsf","niak","ccs"))
df$filter <- factor(df$filter)
df$atlas <- factor(df$atlas, levels = c("cc200","cc400","ez","ho","tt","aal"))
df$site <- factor(df$site)
df$ID <- factor(df$ID)
df$MPFC.LP_L <- as.numeric(df$MPFC.LP_L)
df$MPFC.LP_R <- as.numeric(df$MPFC.LP_R)
df$LP_L.LP_R <- as.numeric(df$LP_L.LP_R)
df$MPFC.PCC <- as.numeric(df$MPFC.PCC)
df$LP_L.PCC <- as.numeric(df$LP_L.PCC)
df$LP_R.PCC <- as.numeric(df$LP_R.PCC)
str(df)

# Get rid of extra NA because of making the resulting empty df larger than needed
df <- df[complete.cases(df),]

# Export final dataframe to reference later
save(df, 
     file = paste0(getwd(),"/Data/processed_data.RData"))


