# Examining two randomly selected files from ABIDE and cross-checking mean BOLD signal

setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Packages
library(RNifti)

# Load in first file
dat1 <- readNifti("Mean BOLD Signal Re-creation/CMU_a_0050666_func_preproc.nii.gz")
dim(dat1)

# Load in randomly selected atlas - AAL
AAL <- readNifti("Mean BOLD Signal Re-creation/aal_roi_atlas.nii")
dim(AAL)

# Update AAL bounding box to match that of data file (confirmed match in MRIcroGL)
AAL <- AAL[3:63,3:75,1:61]
dim(AAL)
writeNifti(AAL, "Mean BOLD Signal Re-creation/AAL.nii", template = dat1)

# Create empty data file to store resulting mean BOLD signal
labels <- as.character(data.frame(table(AAL))[,1])[2:117]
numeric.labels <- as.numeric(labels)
results <- matrix(NA, nrow = dim(dat1)[4], ncol = length(labels))
colnames(results) <- labels

for (i in 1:length(numeric.labels)){
  # Pull coordinates for AAL ROI
  coord <- which(AAL == numeric.labels[i], arr.ind = T)
  
  # Create empty matrix to store preproc BOLD signal
  BOLD <- matrix(NA, nrow = dim(dat1)[4], ncol = nrow(coord))
  
  # Pull signals according to coordinaes
  for (j in 1:nrow(coord)){
    BOLD[,j] <- dat1[coord[j,1],coord[j,2],coord[j,3],]
  }
  
  # Take the mean BOLD signal within ROI and store in results
  results[,i] <- rowMeans(BOLD)
}

# Load in corresponding .1d file
ABIDE <- read.table("Preprocessed Data/ccs_filt_noglobal_rois_aal/CMU_a_0050666_rois_aal.1D",
                    comment.char = "", header = TRUE)

# Plot overlapping BOLD signals - very different!!
index=2
plot(ABIDE[,index],type = 'l')
lines(results[,index], lty = "dashed", col=2)

# Recalculate correlation matrix to see how similar
DMN.ROI_AAL <- rep(c("MPFC", "MPFC", "LP_L", "LP_R", "LP_R", "PCC", "PCC"))
Index_AAL   <- rep(c("X.2611", "X.2612",
                     "X.5201",
                     "X.5202", "X.6222",
                     "X.6301", "X.6302"),
                   each = 1)
Volume.ROI_AAL  <- c(234, 283, 1349, 861, 787, 1203, 1211)

# Create a data frame with the specified combinations
ROI.atlas.labels_AAL <- data.frame(DMN.ROI_AAL, Index_AAL, Volume.ROI_AAL)

# calculate weight for each ROI
Weight_MPFC = c((Volume.ROI_AAL[1:2] / sum(Volume.ROI_AAL[1:2])), (rep(0, 5)))
Weight_LP_L = c((rep(0, 2)), (Volume.ROI_AAL[3] / sum(Volume.ROI_AAL[3])), (rep(0, 4)))
Weight_LP_R = c((rep(0, 3)), (Volume.ROI_AAL[4:5] / sum(Volume.ROI_AAL[4:5])), (rep(0, 2)))
Weight_PCC = c((rep(0, 5)), (Volume.ROI_AAL[6:7] / sum(Volume.ROI_AAL[6:7])))

# Calculate matrix (ABIDE)
dat_i = as.matrix(ABIDE[,Index_AAL])
out_MPFC = dat_i %*% Weight_MPFC
out_LP_L = dat_i %*% Weight_LP_L
out_LP_R = dat_i %*% Weight_LP_R
out_PCC = dat_i %*% Weight_PCC

BOLD_i = cbind(out_MPFC, out_LP_L, out_LP_R, out_PCC)
correlation_matrix_ABIDE = cor(BOLD_i)

# Calculate matrix (mine)
Index_AAL   <- rep(c("2611", "2612",
                     "5201",
                     "5202", "6222",
                     "6301", "6302"),
                   each = 1)
dat_i = as.matrix(results[,Index_AAL])
out_MPFC = dat_i %*% Weight_MPFC
out_LP_L = dat_i %*% Weight_LP_L
out_LP_R = dat_i %*% Weight_LP_R
out_PCC = dat_i %*% Weight_PCC

BOLD_i = cbind(out_MPFC, out_LP_L, out_LP_R, out_PCC)
correlation_matrix_result = cor(BOLD_i)

# Compare matrices
correlation_matrix_ABIDE
correlation_matrix_result
round(abs(correlation_matrix_ABIDE-correlation_matrix_result), digits = 3)




