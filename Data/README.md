The contents of this folder are as follows:

**Correlation_Matrices:** This folder contains the FC matrix .RData files for each of the 48 preprocessing combinations that are used as the basis for the remainder of the analysis. The files in this folder can be considered the output of the Data folder as a whole.

**processed_data.RData**: The long format dataframe after vectorizing the matrices in the Correlation_Matrices folder, combining the necessary neuroimaging sites (as explained in the code comments and paper), and applying Fisher's *z*-transformation to the correlation coefficients. 

**processed_data_ComBat.RData** The long format dataframe that is identical in structure to the processed_data.RData file, but is the resulting Fisher's *z*-transformed coefficients after performing ComBat harmonization to control for the batch effect of site. 

**processed_data_ranef.RData** The long format dataframe that is identical in structure to the processed_data.RData file, but is the resulting Fisher's *z*-transformed coefficients after controlling for the site effect by subtracting out the estimated random effects from the edgewise LMMs. 

