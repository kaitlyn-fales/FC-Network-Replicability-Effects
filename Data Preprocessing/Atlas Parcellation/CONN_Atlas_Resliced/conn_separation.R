############ Separate CONN networks atlas into four separate DMN ROI files for reslicing ###############

#### Change Working Directory if Necessary ####
#setwd("~/FC-Network-Replicability-Effects")

# Set atlas parcellation directory path
base_directory <- paste0(getwd(),"/Data Preprocessing/Atlas Parcellation")

# Packages
library(RNifti)

# Obtain CONN toolbox networks atlas, download it and load it here
# We do not include the networks.nii file as it is publicly available with a CONN toolbox account
conn <- readNifti("networks.nii")

# Separate the CONN networks atlas into four separate files - one for each ROI of the DMN
# Write those files into the corresponding folder to be resliced in SPM12
mpfc <- conn[,,,1]
writeNifti(mpfc, "CONN_Atlas_Resliced/mpfc.nii", template = conn)

lp.l <- conn[,,,2]
writeNifti(lp.l, "CONN_Atlas_Resliced/lp_l.nii", template = conn)

lp.r <- conn[,,,3]
writeNifti(lp.r, "CONN_Atlas_Resliced/lp_r.nii", template = conn)

pcc <- conn[,,,4]
writeNifti(pcc, "CONN_Atlas_Resliced/pcc.nii", template = conn)

