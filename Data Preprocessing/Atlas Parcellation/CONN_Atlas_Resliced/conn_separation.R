setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Atlas Parcellation/CONN_atlas")

library(RNifti)

conn <- readNifti("networks.nii")

mpfc <- conn[,,,1]
writeNifti(mpfc, "mpfc.nii", template = conn)

lp.l <- conn[,,,2]
writeNifti(lp.l, "lp_l.nii", template = conn)

lp.r <- conn[,,,3]
writeNifti(lp.r, "lp_r.nii", template = conn)

pcc <- conn[,,,4]
writeNifti(pcc, "pcc.nii", template = conn)

