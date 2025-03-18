remove(list = ls())

# Set wd to be general repo folder
#setwd("~/FC-Network-Replicability-Effects")

#### EZ columns & weights ####
load("Data Preprocessing/Metadata/ez.Rdata")
meta_lst = meta_ez
subfolders = names(meta_ez)
rm(meta_ez)

# Change paths as needed
base_path = paste0(getwd(),"/Raw Data Download/Raw_Data") # path to raw data 
setwd(paste0(getwd(),"/Data/Correlation_Matrices")) # current folder to store data

# data based on "DMN ROIs (new)" sheet in "ROI_atlas_labels.xlsx" file.
DMN.ROI_EZ <- rep(c("MPFC", "MPFC", "LP_L", "LP_R", "LP_R", "PCC", "PCC"))
Index_EZ   <- rep(c("X.25", "X.26", 
                    "X.51", 
                    "X.52", "X.86", 
                    "X.67", "X.68"), 
                  each = 1)
Volume.ROI_EZ  <- c(289, 369, 1266, 810, 1545, 1131, 1184)

# Create a data frame with the specified combinations
# ROI.atlas.labels_EZ <- data.frame(DMN.ROI_EZ, Index_EZ, Volume.ROI_EZ)

# calculate weight for each ROI
Weight_MPFC = c((Volume.ROI_EZ[1:2] / sum(Volume.ROI_EZ[1:2])), (rep(0, 5)))
Weight_LP_L = c((rep(0, 2)), (Volume.ROI_EZ[3] / sum(Volume.ROI_EZ[3])), (rep(0, 4)))
Weight_LP_R = c((rep(0, 3)), (Volume.ROI_EZ[4:5] / sum(Volume.ROI_EZ[4:5])), (rep(0, 2)))
Weight_PCC = c((rep(0, 5)), (Volume.ROI_EZ[6:7] / sum(Volume.ROI_EZ[6:7])))

col_index = Index_EZ
rm(Index_EZ, Volume.ROI_EZ)

# list of files to compute correlation matrices for:
all_files = list()
index = 1
for(index in 1:length(subfolders)){
  all_files[[index]] = paste0(base_path, meta_lst[[index]]$included_files)  # currently all files which do not have any 0 columns in columns related to ROI
}


####

###### do not need to change any code below ######
##### require col_index, weight_mpfc, lp_l, lp_r, pcc, all_files
index = 1
for(index in 1:length(subfolders)){
  # initialize a list to store correlation matrices
  correlation_matrices <- list()
  
  # initialize a list to store data frames for each .1D file
  data_frames <- list()
  
  
  
  # compute weighted means
  i = 1
  pb = progress::progress_bar$new(total = length(all_files[[index]]))
  cat("correlation matrices for", subfolders[index], "\n")
  for(i in 1:length(all_files[[index]])){
    pb$tick()
    dat_i = read.table(all_files[[index]][i], comment.char = "", header = TRUE)
    dat_i = as.matrix(dat_i[, col_index])
    
    # compute weighted TS for each ROI
    out_MPFC = dat_i %*% Weight_MPFC
    out_LP_L = dat_i %*% Weight_LP_L
    out_LP_R = dat_i %*% Weight_LP_R
    out_PCC = dat_i %*% Weight_PCC
    
    BOLD_i = cbind(out_MPFC, out_LP_L, out_LP_R, out_PCC)
    correlation_matrices[[i]] = cor(BOLD_i)
  }
  
  # Save the correlation matrices as .RData files
  fileName = subfolders[index]
  fileName_Path = paste0(subfolders[index], ".Rdata")
  assign(fileName, correlation_matrices)
  save(list = fileName, file = fileName_Path)
}

