########### Getting tables of atlas volume by ROI ##############################

# Set wd
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Atlas Parcellation")

# Packages
library(RNifti)

# AAL atlas
aal <- readNifti("aal_roi_atlas.nii")

table.aal <- data.frame(table(aal))

# CC200 atlas
cc200 <- readNifti("cc200_roi_atlas.nii")

table.cc200 <- data.frame(table(cc200))

# CC400 atlas
cc400 <- readNifti("cc400_roi_atlas.nii")

table.cc400 <- data.frame(table(cc400))

# DOS atlas - can't do volume

# EZ atlas
ez <- readNifti("ez_roi_atlas.nii")

table.ez <- data.frame(table(ez))

# HO atlas
ho <- readNifti("ho_roi_atlas.nii")

table.ho <- data.frame(table(ho))

# TT atlas
tt <- readNifti("tt_roi_atlas.nii")

table.tt <- data.frame(table(tt))

# Export as list .RData object
atlas_volumes <- list(aal = table.aal, cc200 = table.cc200, cc400 = table.cc400,
                      ez = table.ez, ho = table.ho, tt = table.tt)
save(atlas_volumes, file = "atlas_volumes.RData")




