remove(list = ls())

# Set wd to be general repo folder
#setwd("~/FC-Network-Replicability-Effects")

#### CC200 columns & weights ####
load("Data Preprocessing/Metadata/cc200.Rdata")
meta_lst = meta_cc200
subfolders = names(meta_cc200)
rm(meta_cc200)

# Change paths as needed
base_path = paste0(getwd(),"/Raw Data Download/Raw_Data") # path to raw data 
setwd(paste0(getwd(),"/Data/Correlation_Matrices")) # current folder to store data

# data based on "DMN ROIs (new)" sheet in "ROI_atlas_labels.xlsx" file.
DMN.ROI_CC200 <- rep(c("MPFC", "MPFC", "MPFC", "LP_L", "LP_L", "LP_L", "LP_R", "LP_R", "LP_R", "PCC", "PCC", "PCC", "PCC"))
Index_CC200   <- rep(c("X.51", "X.109", "X.139",
                       "X.82", "X.97", "X.114",
                       "X.14", "X.166", "X.170",
                       "X.3", "X.19", "X.163", "X.174"),
                     each = 1)
Volume.ROI_CC200  <- c(222, 193, 202, 211, 239, 287, 289, 252, 222, 284, 291, 266, 350)

# Create a data frame with the specified combinations
# ROI.atlas.labels_CC200 <- data.frame(DMN.ROI_CC200, Index_CC200, Volume.ROI_CC200)

# calculate weight for each ROI
Weight_MPFC = c((Volume.ROI_CC200[1:3] / sum(Volume.ROI_CC200[1:3])), (rep(0, 10)))
Weight_LP_L = c((rep(0, 3)), (Volume.ROI_CC200[4:6] / sum(Volume.ROI_CC200[4:6])), (rep(0, 7)))
Weight_LP_R = c((rep(0, 6)), (Volume.ROI_CC200[7:9] / sum(Volume.ROI_CC200[7:9])), (rep(0, 4)))
Weight_PCC = c((rep(0, 9)), (Volume.ROI_CC200[10:13] / sum(Volume.ROI_CC200[10:13])))

col_index = Index_CC200
rm(Index_CC200, Volume.ROI_CC200)

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

