setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Atlas Parcellation")

library(RNifti)
library(modeest)
library(writexl)

#### Change Working Directory if Necessary ####
#setwd("~/FC-Network-Replicability-Effects")

# Set atlas parcellation directory path
base_directory <- paste0(getwd(),"/Data Preprocessing/Atlas Parcellation")

# Packages
#install.packages('RNifti')
library(RNifti)

# Reading in each atlas file and creating a table for the volume within each ROI
# Obtain each atlas .nii file from ABIDE website: http://preprocessed-connectomes-project.org/abide/Pipelines.html
# You can download each file from the above link (or links provided below)

# Once files are downloaded and within the ABIDE_Atlases subfolder, you can run the code below

# AAL atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/aal_roi_atlas.nii.gz
aal <- readNifti(paste0(base_directory,"/ABIDE_Atlases/aal_roi_atlas.nii.gz"))

# CC200 atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/cc200_roi_atlas.nii.gz
cc200 <- readNifti(paste0(base_directory,"/ABIDE_Atlases/cc200_roi_atlas.nii.gz"))

# CC400 atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/cc400_roi_atlas.nii.gz
cc400 <- readNifti(paste0(base_directory,"/ABIDE_Atlases/cc400_roi_atlas.nii.gz"))

# DOS atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/dos160_roi_atlas.nii.gz
dos <- readNifti(paste0(base_directory,"/ABIDE_Atlases/dos160_roi_atlas.nii.gz"))

# EZ atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/ez_roi_atlas.nii.gz
ez <- readNifti(paste0(base_directory,"/ABIDE_Atlases/ez_roi_atlas.nii.gz"))

# HO atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/ho_roi_atlas.nii.gz
ho <- readNifti(paste0(base_directory,"/ABIDE_Atlases/ho_roi_atlas.nii.gz"))

# TT atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/tt_roi_atlas.nii.gz
tt <- readNifti(paste0(base_directory,"/ABIDE_Atlases/tt_roi_atlas.nii.gz"))

# Load CONN atlas
networks <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rnetworks_65_77_63.nii"))

# Loading CONN networks names table
networks_table <- read.table(paste0(base_directory,"/CONN_Atlas_Resliced/networks_table.txt"), sep = ",", header = T)

############# Functions #######
# Function for finding potential ROI
get_parcels <- function(atlas,region){
  props <- prop.table(table(atlas[region == 1])) # pull values that match the region
  props <- data.frame(props)
  colnames(props) <- c('region','proportion')
  props <- props[props$region != 0,] # get rid of 0 values - outside brain atlas
  props$proportion <- props$proportion / sum(props$proportion) # reweight for 0 values
  return(props)
}

# Loop through CONN networks and create list of dataframes and props for each atlas
get_roi_list <- function(atlas){
  atlas_roi <- list()
  for (i in 1:dim(networks)[4]){
    parcels <- get_parcels(atlas, networks[,,,i])
    atlas_roi[[i]] <- list(network = networks_table[i,1],
                           ROI = networks_table[i,2],
                           parcels = parcels,
                           col_index = paste0("X.",as.character(parcels$region))) # for metadata
  }
  return(atlas_roi)
}
###############################

# Load in atlas - AAL
aal_roi <- get_roi_list(aal)
save(aal_roi, file = "aal_roi.RData")

# Load in atlas - EZ
ez_roi <- get_roi_list(ez)
save(ez_roi, file = "ez_roi.RData")

# Load in atlas - HO
ho_roi <- get_roi_list(ho)
save(ho_roi, file = "ho_roi.RData")

# Load in atlas - TT
tt_roi <- get_roi_list(tt)
save(tt_roi, file = "tt_roi.RData")


#### CC200 and CC400
# Reload CONN atlas with updated bounding box
networks <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rnetworks_63_75_61.nii"))

# Load in atlas - CC200
cc200_roi <- get_roi_list(cc200)
save(cc200_roi, file = "cc200_roi.RData")

# Load in atlas - CC400
cc400_roi <- get_roi_list(cc400)
save(cc400_roi, file = "cc400_roi.RData")


#### DOS 160
# Reload CONN atlas with updated bounding box
networks <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rnetworks_61_73_61.nii"))

# Load in atlas - DOS
dos <- readNifti("dos160_roi_atlas.nii")
dos_roi <- get_roi_list(dos)

# Issue with 18 and 20 having no parcels within region - due to not having full region
# Use the parcel with the minimum distance to the region instead

# Get centroid for SMG_R (18)
region_coord <- which(networks[,,,18] == 1, arr.ind = T) # pull values that match the region
region_centroid_SMG <- round(colMeans(region_coord))

# Get centroid for FEF_R (20)
region_coord <- which(networks[,,,20] == 1, arr.ind = T) # pull values that match the region
region_centroid_FEF <- round(colMeans(region_coord))

# Get centroids for DOS atlas
centroids <- matrix(NA, nrow = 160, ncol = 3)
for (i in 1:160){
  coord <- which(dos == i, arr.ind = T)
  centroids[i,] <- round(colMeans(coord))
}
centroids <- data.frame(centroids)
colnames(centroids) <- c('x','y','z')

# Initialize vectors to store distances
euclidean_SMG <- numeric()
euclidean_FEF <- numeric()
manhattan_SMG <- numeric()
manhattan_FEF <- numeric()

p = 5
minkowski_SMG <- numeric()
minkowski_FEF <- numeric()

# Calculate Euclidean dist to networks region 18 (SMG_R) and 20 (FEF_R)
for (i in 1:160){
  euclidean_SMG[i] <- sqrt(sum((centroids[i,]-region_centroid_SMG)^2))
  euclidean_FEF[i] <- sqrt(sum((centroids[i,]-region_centroid_FEF)^2))
  manhattan_SMG[i] <- sum(abs(centroids[i,]-region_centroid_SMG))
  manhattan_FEF[i] <- sum(abs(centroids[i,]-region_centroid_FEF))
  minkowski_SMG[i] <- sum(abs(centroids[i,]-region_centroid_SMG)^p)^(1/p)
  minkowski_FEF[i] <- sum(abs(centroids[i,]-region_centroid_FEF)^p)^(1/p)
}

# Join as dataframe
distances_SMG <- data.frame(parcel = c(1:160),euclidean_SMG,manhattan_SMG,minkowski_SMG)
distances_FEF <- data.frame(parcel = c(1:160),euclidean_FEF,manhattan_FEF,minkowski_FEF)

# Sort by parcels with the shortest distance SMG and find mode
euclidean_choice_SMG <- head(distances_SMG[order(distances_SMG$euclidean_SMG),], 1)
manhattan_choice_SMG <- head(distances_SMG[order(distances_SMG$manhattan_SMG),], 1)
minkowski_choice_SMG <- head(distances_SMG[order(distances_SMG$minkowski_SMG),], 1)
parcel_SMG <- mfv(c(euclidean_choice_SMG$parcel,manhattan_choice_SMG$parcel,
                    minkowski_choice_SMG$parcel))
parcel_SMG

# Sort by parcels with the shortest distance FEF and find mode
euclidean_choice_FEF <- head(distances_FEF[order(distances_FEF$euclidean_FEF),],1)
manhattan_choice_FEF <- head(distances_FEF[order(distances_FEF$manhattan_FEF),],1)
minkowski_choice_FEF <- head(distances_FEF[order(distances_FEF$minkowski_FEF),],1)
parcel_FEF <- mfv(c(euclidean_choice_FEF$parcel,manhattan_choice_FEF$parcel,
                    minkowski_choice_FEF$parcel))
parcel_FEF

# Insert information into dos_roi object for export
dos_roi[[18]]$parcels <- data.frame(region = parcel_SMG, proportion = 1)
dos_roi[[20]]$parcels <- data.frame(region = parcel_FEF, proportion = 1)

dos_roi[[18]]$col_index <- paste0("X.",parcel_SMG)
dos_roi[[20]]$col_index <- paste0("X.",parcel_FEF)

save(dos_roi, file = "dos_roi.RData")

##############################################
# Code used to export final object to xlsx workbooks (example)
lst <- lapply(aal_roi, function(x) {
  parcels_sorted <- x$parcels[order(x$parcels$proportion, decreasing = TRUE), ]
  x$col_index <- NULL
  
  data.frame(
    Network = x$network,
    "CONN ROI" = x$ROI,
    Parcel = as.numeric(as.character(parcels_sorted$region)),
    "Label" = NA,
    Proportion = round(parcels_sorted$proportion, digits = 4),
    row.names = NULL
  )
})
df <- do.call(rbind, lst)
split_list <- split(df, df$Network)
split_list <- lapply(split_list, function(x) {
  x$Network <- NULL 
  return(x)
})

# Write to Excel (each element = one sheet)
write_xlsx(split_list, path = paste0(base_directory,"/aal_parcels.xlsx"))


