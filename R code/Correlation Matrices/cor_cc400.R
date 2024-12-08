remove(list = ls())

#### change the paths for your local paths ####
# current folder
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-ThePennsylvaniaStateUniversity/Fales, Kaitlyn Rose - Scanner Heterogeneity Project/Correlation_Matrices")
base_path = "/Users/xuruizhi/Library/CloudStorage/OneDrive-SharedLibraries-ThePennsylvaniaStateUniversity/Fales, Kaitlyn Rose - Scanner Heterogeneity Project/Preprocessed Data"
# do not put / at the end

#### CC400 columns & weights ####
load("../Metadata/cc400.Rdata")
meta_lst = meta_cc400
subfolders = names(meta_cc400)
rm(meta_cc400)

# data based on "DMN ROIs (new)" sheet in "ROI_atlas_labels.xlsx" file.
DMN.ROI_CC400 <- rep(c("MPFC", "MPFC", "MPFC", "MPFC", "LP_L", "LP_L", "LP_L", "LP_R", "LP_R", "LP_R", "LP_R", "PCC"))
Index_CC400   <- rep(c("X.84", "X.142", "X.232", "X.264",
                       "X.6", "X.120", "X.189",
                       "X.26", "X.135", "X.252", "X.341",
                       "X.193"),
                     each = 1)
Volume.ROI_CC400  <- c(72, 120, 79, 81, 127, 148, 112, 112, 126, 109, 132, 145)

# Create a data frame with the specified combinations
# ROI.atlas.labels_CC400 <- data.frame(DMN.ROI_CC400, Index_CC400, Volume.ROI_CC400)

# calculate weight for each ROI
Weight_MPFC = c((Volume.ROI_CC400[1:4] / sum(Volume.ROI_CC400[1:4])), (rep(0, 8)))
Weight_LP_L = c((rep(0, 4)), (Volume.ROI_CC400[5:7] / sum(Volume.ROI_CC400[5:7])), (rep(0, 5)))
Weight_LP_R = c((rep(0, 7)), (Volume.ROI_CC400[8:11] / sum(Volume.ROI_CC400[8:11])), (rep(0, 1)))
Weight_PCC = c((rep(0, 11)), (Volume.ROI_CC400[12] / sum(Volume.ROI_CC400[12])))

col_index = Index_CC400
rm(Index_CC400, Volume.ROI_CC400)

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

