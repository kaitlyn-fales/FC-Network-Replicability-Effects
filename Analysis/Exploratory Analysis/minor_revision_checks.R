
###### Checking power spectrum of signals for filtering vs no for NIAK vs others ######
path <- "Raw Data Download/Raw_Data"

# Example of a filtered pipeline to check, we check several IDs
filt <- read.table(file = paste(path,"dparsf_filt_noglobal_rois_aal/Caltech_0051493_rois_aal.1D",sep = "/"),
                   comment.char = "", header = TRUE)


nofilt <- read.table(file = paste(path,"dparsf_nofilt_noglobal_rois_aal/Caltech_0051493_rois_aal.1D",sep = "/"),
                    comment.char = "", header = TRUE)

power_spectra <- function(x){
  # example for one time series x
  spec <- spectrum(x, plot = FALSE)
  
  # frequencies (cycles per time unit)
  freq <- spec$freq
  
  # power
  power <- spec$spec
  
  TR <- 2 # Caltech TR 
  freq_hz <- freq / TR
  
  plot(freq_hz, power, type = "l")
  abline(v = c(0.01, 0.1), col = "red", lty = 2)
}

par(mfrow = c(1,2))
power_spectra(filt[,1])
power_spectra(nofilt[,1])

# Example of a NIAK pipeline to check, we check several IDs - not filtered
filt <- read.table(file = paste(path,"niak_filt_noglobal_rois_aal/Caltech_0051493_rois_aal.1D",sep = "/"),
                   comment.char = "", header = TRUE)


nofilt <- read.table(file = paste(path,"niak_nofilt_noglobal_rois_aal/Caltech_0051493_rois_aal.1D",sep = "/"),
                     comment.char = "", header = TRUE)

par(mfrow = c(1,2))
power_spectra(filt[,1])
power_spectra(nofilt[,1])

#######################################

###### Checking possible impact of pipelines not dropping TRs for different scanners #####

# Rerun correlation matrices for CPAC and NIAK dropping first 4 TRs

remove(list = ls())

#### AAL columns & weights ####
load("Data Preprocessing/Metadata/aal.Rdata")
meta_lst = meta_aal
subfolders = names(meta_aal)[c(3:4,7:8)]
meta_lst <- meta_lst[subfolders]
rm(meta_aal)

# Change paths as needed
base_path = paste0(getwd(),"/Raw Data Download/Raw_Data") # path to raw data 
atlas_path = paste0(getwd(),"/Data Preprocessing/Atlas Parcellation")

dir.create("Data/Correlation_Matrices_Volume_Removal", showWarnings = FALSE)
setwd(paste0(getwd(),"/Data/Correlation_Matrices_Volume_Removal")) # current folder to store data output

# Load atlas file which contains parcellation information and weights
load(paste(atlas_path,"aal_roi.RData",sep = "/"))
roi_lst = aal_roi
rm(aal_roi)


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
    
    # REMOVE FIRST 4 TRs
    dat_i <- dat_i[-c(1:4),]
    
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

# Check the correlation between the regular FC matrices and the TR removed versions
remove(list = ls())

cor_check <- function(i){
  m1 <- no_removal[[i]]
  m2 <- removal[[i]]
  
  idx <- upper.tri(m1, diag = FALSE)
  correlation <- cor(m1[idx], m2[idx])
  
  return(correlation)
}

# CPAC filt
load("Data/Correlation_Matrices/cpac_filt_noglobal_rois_aal.Rdata")
no_removal <- cpac_filt_noglobal_rois_aal

load("Data/Correlation_Matrices_Volume_Removal/cpac_filt_noglobal_rois_aal.Rdata")
removal <- cpac_filt_noglobal_rois_aal

rm(cpac_filt_noglobal_rois_aal)

cpac_filt_check <- sapply(1:length(no_removal), cor_check)
hist(cpac_filt_check)

# CPAC no filt
load("Data/Correlation_Matrices/cpac_nofilt_noglobal_rois_aal.Rdata")
no_removal <- cpac_nofilt_noglobal_rois_aal

load("Data/Correlation_Matrices_Volume_Removal/cpac_nofilt_noglobal_rois_aal.Rdata")
removal <- cpac_nofilt_noglobal_rois_aal

rm(cpac_nofilt_noglobal_rois_aal)

cpac_nofilt_check <- sapply(1:length(no_removal), cor_check)
hist(cpac_nofilt_check)

# NIAK filt
load("Data/Correlation_Matrices/niak_filt_noglobal_rois_aal.Rdata")
no_removal <- niak_filt_noglobal_rois_aal

load("Data/Correlation_Matrices_Volume_Removal/niak_filt_noglobal_rois_aal.Rdata")
removal <- niak_filt_noglobal_rois_aal

rm(niak_filt_noglobal_rois_aal)

niak_filt_check <- sapply(1:length(no_removal), cor_check)
hist(niak_filt_check)

# NIAK no filt
load("Data/Correlation_Matrices/niak_nofilt_noglobal_rois_aal.Rdata")
no_removal <- niak_nofilt_noglobal_rois_aal

load("Data/Correlation_Matrices_Volume_Removal/niak_nofilt_noglobal_rois_aal.Rdata")
removal <- niak_nofilt_noglobal_rois_aal

rm(niak_nofilt_noglobal_rois_aal)

niak_nofilt_check <- sapply(1:length(no_removal), cor_check)
hist(niak_nofilt_check)

