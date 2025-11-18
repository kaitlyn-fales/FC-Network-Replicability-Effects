The contents of this folder are as follows:

**Correlation_Matrices:** This folder contains the FC matrix .RData files for each of the 48 preprocessing combinations that are used as the basis for the remainder of the analysis. The files in this folder can be considered the output of the Data folder as a whole.

**processed_data_ComBat.RData** The long format dataframe that is identical in structure to the combined processed_data_network.RData and processed_data_between_network.RData files, but is the resulting Fisher's *z*-transformed coefficients after performing ComBat harmonization to control for the batch effect of site.

**processed_data_combat_regressed.RData** The long format dataframe with the resulting Fisher's *z*-transformed coefficients after performing ComBat harmonization to control for the batch effect of site and after subtracting out the estimated random effect for subject ID (only ID, not site as ComBat controls for site effect).

**processed_data_between_network.RData**: The long format dataframe after vectorizing the matrices in the Correlation_Matrices folder, combining the necessary neuroimaging sites (as explained in the code comments and paper), and applying Fisher's *z*-transformation to the correlation coefficients. This is the **between** network version. 

**processed_data_between_network_baseline.RData** The long format dataframe that is identical in structure to the processed_data.RData file, but is the resulting Fisher's *z*-transformed coefficients after controlling for the fixed effects from edgewise LMMs; used for constructing the baseline distribution for intra-subject variability. This is the **between** network version.

**processed_data_between_network_regressed.RData** The long format dataframe that is identical in structure to the processed_data.RData file, but is the resulting Fisher's *z*-transformed coefficients after controlling for the site effect by subtracting out the estimated random effects from the edgewise LMMs. This is the **between** network version. 

**processed_data_network.RData**: The long format dataframe after vectorizing the matrices in the Correlation_Matrices folder, combining the necessary neuroimaging sites (as explained in the code comments and paper), and applying Fisher's *z*-transformation to the correlation coefficients. This is the **within** network version. 

**processed_data_network_baseline.RData** The long format dataframe that is identical in structure to the processed_data.RData file, but is the resulting Fisher's *z*-transformed coefficients after controlling for the fixed effects from edgewise LMMs; used for constructing the baseline distribution for intra-subject variability. This is the **within** network version.

**processed_data_network_regressed.RData** The long format dataframe that is identical in structure to the processed_data.RData file, but is the resulting Fisher's *z*-transformed coefficients after controlling for the site effect by subtracting out the estimated random effects from the edgewise LMMs. This is the **within** network version. 

