setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Atlas Parcellation")

library(RNifti)

# Load CONN atlas
mpfc <- readNifti("CONN_atlas/rmpfc_65_77_63.nii")
lp.l <- readNifti("CONN_atlas/rlp_l_65_77_63.nii")
lp.r <- readNifti("CONN_atlas/rlp_r_65_77_63.nii")
pcc <- readNifti("CONN_atlas/rpcc_65_77_63.nii")

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
  result <- as.numeric(names(which(overlap >= prop))) # only take ROI with at least 5% overlap
  
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

# Load in atlas - AAL
aal <- readNifti("aal_roi_atlas.nii")

get_DMN_ROIs(aal)

# Load in atlas - EZ
ez <- readNifti("ez_roi_atlas.nii")

get_DMN_ROIs(ez)

# Load in atlas - HO
ho <- readNifti("ho_roi_atlas.nii")

get_DMN_ROIs(ho)

# Load in atlas - TT
tt <- readNifti("tt_roi_atlas.nii")

get_DMN_ROIs(tt)

# Reload CONN atlas with updated bounding box
mpfc <- readNifti("CONN_atlas/rmpfc_63_75_61.nii")
lp.l <- readNifti("CONN_atlas/rlp_l_63_75_61.nii")
lp.r <- readNifti("CONN_atlas/rlp_r_63_75_61.nii")
pcc <- readNifti("CONN_atlas/rpcc_63_75_61.nii")

# Load in atlas - CC200
cc200 <- readNifti("cc200_roi_atlas.nii")

get_DMN_ROIs(cc200)

# Load in atlas - CC400
cc400 <- readNifti("cc400_roi_atlas.nii")

get_DMN_ROIs(cc400)




########## Disregard below - not using DOS160 atlas after 2/9/24 ###############

# Restructure CONN atlas (adjust array to updated bounding box)
mpfc <- mpfc[2:64,2:74,]
lp.l <- lp.l[2:64,2:74,]
lp.r <- lp.r[2:64,2:74,]
pcc <- pcc[2:64,2:74,]

# Load in atlas - DOS
dos <- readNifti("dos160_roi_atlas.nii")

get_potential_ROI(dos,mpfc)
get_potential_ROI(dos,lp.l)
get_potential_ROI(dos,lp.r)
get_potential_ROI(dos,pcc)
