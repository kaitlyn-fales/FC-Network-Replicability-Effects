########### Atlas Parcellation Preprocessing ###########################################

# Step 1: Getting tables of atlas volume by ROI - values used in ROI_atlas_labels.csv
# Step 2: Overlap calculations - determining the parcels to include in each ROI within ROI_atlas_labels.csv

# The code within this file generates the information manually input in the ROI_atlas_labels.csv file

########### Step 1: Getting tables of atlas volume by ROI ##############################

#### Change Working Directory if Necessary ####
#setwd("~/FC-Network-Replicability-Effects")

# Set atlas parcellation directory path
base_directory <- paste0(getwd(),"/Data Preprocessing/Atlas Parcellation")

# Packages
library(RNifti)

# Reading in each atlas file and creating a table for the volume within each ROI
# Obtain each atlas .nii file from ABIDE website: http://preprocessed-connectomes-project.org/abide/Pipelines.html
# You can download each file from the above link (or links provided below)

# Once files are downloaded and within the ABIDE_Atlases subfolder, you can run the code below

# AAL atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/aal_roi_atlas.nii.gz
aal <- readNifti(paste0(base_directory,"/ABIDE_Atlases/aal_roi_atlas.nii.gz"))

table.aal <- data.frame(table(aal))

# CC200 atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/cc200_roi_atlas.nii.gz
cc200 <- readNifti(paste0(base_directory,"/ABIDE_Atlases/cc200_roi_atlas.nii.gz"))

table.cc200 <- data.frame(table(cc200))

# CC400 atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/cc400_roi_atlas.nii.gz
cc400 <- readNifti(paste0(base_directory,"/ABIDE_Atlases/cc400_roi_atlas.nii.gz"))

table.cc400 <- data.frame(table(cc400))

# DOS atlas - can't do volume

# EZ atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/ez_roi_atlas.nii.gz
ez <- readNifti(paste0(base_directory,"/ABIDE_Atlases/ez_roi_atlas.nii.gz"))

table.ez <- data.frame(table(ez))

# HO atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/ho_roi_atlas.nii.gz
ho <- readNifti(paste0(base_directory,"/ABIDE_Atlases/ho_roi_atlas.nii.gz"))

table.ho <- data.frame(table(ho))

# TT atlas
# Download link: https://fcp-indi.s3.amazonaws.com/data/Projects/ABIDE_Initiative/Resources/tt_roi_atlas.nii.gz
tt <- readNifti(paste0(base_directory,"/ABIDE_Atlases/tt_roi_atlas.nii.gz"))

table.tt <- data.frame(table(tt))

# Optional: Export as list .RData object 
# These values are used for the volume numbers in the ROI_atlas_labels.csv
#atlas_volumes <- list(aal = table.aal, cc200 = table.cc200, cc400 = table.cc400,
#                      ez = table.ez, ho = table.ho, tt = table.tt)
#save(atlas_volumes, file = "atlas_volumes.RData")




########### Step 2: Overlap calculations ###############################################

# Load resliced CONN atlas - reslicing done in SPM12
mpfc <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rmpfc_65_77_63.nii"))
lp.l <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rlp_l_65_77_63.nii"))
lp.r <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rlp_r_65_77_63.nii"))
pcc <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rpcc_65_77_63.nii"))

############# Functions #######
# Function for finding potential ROI
get_potential_ROI <- function(atlas,region){
  props <- prop.table(table(atlas[region == 1])) # pull values that match the region
  prop_reduce <- as.numeric(names(which(props>=0.001))) 
  prop_reduce <- prop_reduce[prop_reduce!=0] # get rid of 0 values - not meaningful
  return(prop_reduce)
}

# Function to find overlap
get_overlap <- function(atlas,region,atlas.ROI){
  length(intersect(which(region==1),which(atlas==atlas.ROI)))/
    (length(which(region==1)) + length(which(atlas==atlas.ROI)))
}

# Combine functions to determine ROIs with at least % overlap
get_ROI <- function(atlas,region,prop=0.08){
  pot.ROI <- get_potential_ROI(atlas,region)
  
  overlap <- rep(NA,length(pot.ROI))
  for (i in 1:length(pot.ROI)){
    overlap[i] <- get_overlap(atlas,region,pot.ROI[i])
  }
  names(overlap) <- as.character(pot.ROI)
  result <- as.numeric(names(which(overlap >= prop))) # only take ROI with at least 8% overlap
  
  return(result)
}

# Run each set of functions for a particular atlas
get_DMN_ROIs <- function(atlas,prop=0.08){
  mpfc.ROI <- get_ROI(atlas,mpfc,prop)
  lp.l.ROI <- get_ROI(atlas,lp.l,prop)
  lp.r.ROI <- get_ROI(atlas,lp.r,prop)
  pcc.ROI <- get_ROI(atlas,pcc,prop)
  
  out <- list(mpfc = mpfc.ROI, lp.l = lp.l.ROI, lp.r = lp.r.ROI, pcc = pcc.ROI)
  return(out)
}
###############################

############# Apply functions to get parcels for each atlas with at least 8% overlap with CONN ###########
# These results are used as the atlas labels found in the ROI_atlas_labels.csv
# AAL
get_DMN_ROIs(aal)

# EZ
get_DMN_ROIs(ez)

# HO
get_DMN_ROIs(ho)

# TT
get_DMN_ROIs(tt)

# Reload resliced CONN atlas with updated bounding box for the Craddock atlases
mpfc <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rmpfc_63_75_61.nii"))
lp.l <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rlp_l_63_75_61.nii"))
lp.r <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rlp_r_63_75_61.nii"))
pcc <- readNifti(paste0(base_directory,"/CONN_Atlas_Resliced/rpcc_63_75_61.nii"))

# CC200
get_DMN_ROIs(cc200)

# CC400
get_DMN_ROIs(cc400)

