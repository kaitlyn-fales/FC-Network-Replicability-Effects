remove(list = ls())

# Set wd to be general repo folder
#setwd("~/FC-Network-Replicability-Effects")

#### AAL columns & weights ####
load("Data Preprocessing/Metadata/aal.Rdata")
meta_lst = meta_aal
subfolders = names(meta_aal)
rm(meta_aal)

# Change paths as needed
base_path = paste0(getwd(),"/Raw Data Download/Raw_Data") # path to raw data 
setwd(paste0(getwd(),"/Data/Correlation_Matrices")) # current folder to store data

# data based on "DMN ROIs (new)" sheet in "ROI_atlas_labels.xlsx" file.
DMN.ROI_AAL <- rep(c("MPFC", "MPFC", "LP_L", "LP_R", "LP_R", "PCC", "PCC"))
Index_AAL   <- rep(c("X.2611", "X.2612",
                     "X.5201",
                     "X.5202", "X.6222",
                     "X.6301", "X.6302"),
                   each = 1)
Volume.ROI_AAL  <- c(234, 283, 1349, 861, 787, 1203, 1211)

# Create a data frame with the specified combinations
# ROI.atlas.labels_AAL <- data.frame(DMN.ROI_AAL, Index_AAL, Volume.ROI_AAL)

# calculate weight for each ROI
Weight_MPFC = c((Volume.ROI_AAL[1:2] / sum(Volume.ROI_AAL[1:2])), (rep(0, 5)))
Weight_LP_L = c((rep(0, 2)), (Volume.ROI_AAL[3] / sum(Volume.ROI_AAL[3])), (rep(0, 4)))
Weight_LP_R = c((rep(0, 3)), (Volume.ROI_AAL[4:5] / sum(Volume.ROI_AAL[4:5])), (rep(0, 2)))
Weight_PCC = c((rep(0, 5)), (Volume.ROI_AAL[6:7] / sum(Volume.ROI_AAL[6:7])))
col_index = Index_AAL
rm(Index_AAL, Volume.ROI_AAL)

# list of files to compute correlation matrices for:
all_files = list()
index=1
for(index in 1:length(subfolders)){
  all_files[[index]] = paste0(base_path, meta_lst[[index]]$included_files)  # currently all files which do not have any 0 columns in columns related to ROI
}


####

###### do not need to change any code below ######
##### require col_index, weight_mpfc, lp_l,lp_r,pcc, all_files
index=1
for(index in 1:length(subfolders)){
  # initialize a list to store correlation matrices
  correlation_matrices <- list()
  
  # initialize a list to store data frames for each .1D file
  data_frames <- list()
  
  
  
  # compute weighted means
  i = 1
  pb = progress::progress_bar$new(total=length(all_files[[index]]))
  cat("correlation matrices for",subfolders[index],"\n")
  for(i in 1:length(all_files[[index]])){
    pb$tick()
    dat_i = read.table(all_files[[index]][i], comment.char = "", header = TRUE)
    dat_i = as.matrix(dat_i[,col_index])
    
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

