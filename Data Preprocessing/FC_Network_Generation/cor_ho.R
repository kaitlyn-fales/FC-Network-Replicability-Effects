remove(list = ls())
# change paths to your local paths
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Correlation_Matrices") # current folder
base_path = "C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Preprocessed Data" # do not put / at the end
atlas_path = "C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Atlas Parcellation"

#### AAL columns & weights ####
load("../Metadata/ho.Rdata")
meta_lst = meta_ho
subfolders = names(meta_ho)
rm(meta_ho)


# Load atlas file which contains parcellation information and weights
load(paste(atlas_path,"ho_roi.RData",sep = "/"))
roi_lst = ho_roi
rm(ho_roi)


####

###### do not need to change any code below ######
##### require all_files, roi_lst, meta_lst

# Get list of networks and ROIs to name correlation matrices
column_names = rep(0,30)
for (k in 1:30){
  column_names[k] <- paste(roi_lst[[k]]$network,roi_lst[[k]]$ROI,sep = ".")
}

# list of files to compute correlation matrices for:
all_files = list()
index=1
for(index in 1:length(subfolders)){
  all_files[[index]] = paste0(base_path, meta_lst[[index]]$included_files)  # currently all files which do not have any 0 columns in columns related to ROI
}

# set loop
index=1
for(index in 1:length(subfolders)){
  # initialize a list to store correlation matrices
  correlation_matrices <- list()
  
  # compute weighted means
  i = 1
  pb = progress::progress_bar$new(total=length(all_files[[index]]))
  cat("correlation matrices for",subfolders[index],"\n")
  for(i in 1:length(all_files[[index]])){
    pb$tick()
    dat_i = read.table(all_files[[index]][i], comment.char = "", header = TRUE)
    
    BOLD_i = matrix(NA, nrow = nrow(dat_i), ncol = 30) # empty matrix to store weighted BOLD TS
    
    for (j in 1:ncol(BOLD_i)){
      # compute weighted TS for each ROI
      dat_j = as.matrix(dat_i[,roi_lst[[j]]$col_index])
      BOLD_i[,j] = dat_j %*% roi_lst[[j]]$parcels$proportion
    }
    colnames(BOLD_i) <- column_names
    correlation_matrices[[i]] = cor(BOLD_i)
  }
  
  # Save the correlation matrices as .RData files
  fileName = subfolders[index]
  fileName_Path = paste0(subfolders[index], ".Rdata")
  assign(fileName, correlation_matrices)
  save(list = fileName, file = fileName_Path)
}

