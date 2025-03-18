The contents of this folder are as follows:

**ABIDE_Atlases:** This is an empty folder to contain the downloaded atlas .nii files from the ABIDE website.

**CONN_Atlas_Resliced:** This folder contains the resliced .nii files from the CONN toolbox networks atlas. 

**ROI_atlas_labels.xlsx** This spreadsheet contains the final atlas information corresponding to which parcels from each atlas will form the ROIs of the DMN. This is the same as the information found within Table 1 of the paper, and Table S1 of the Supplementary Materials. This is considered the output file of this folder.

**atlas_parcellation_preprocessing.R** This R script generates the volume information for each atlas, as well as the overlap calculations for which parcels from each atlas are selected to form the DMN in the paper. This code generates the output found in the ROI_atlas_labels.xlsx spreadsheet.
