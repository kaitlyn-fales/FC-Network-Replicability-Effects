The contents of this folder are as follows:

**Atlas Parcellation:** This folder contains the R scripts for determining which parcels from each parcellation would be included in each of the ROIs for the seven networks.

**FC_Network_Generation:** This folder contains the R scripts to create the .RData files in the Correlation_Matrices subfolder within the Data folder. There is one R script file for each of the seven parcellations we used. The only difference from one file to the next is the weights used for the size of each parcel, as explained in the paper.

**Metadata:** This folder contains the R scripts for extracting the metadata from the raw data files, including a spreadsheet with the flagged data files that needed to be removed from the rest of the analysis. 

**preprocess_data.R**: This R script takes the output matrices in the Correlation_Matrices subfolder, and constructs a long-format dataframe out of them for use in the Analysis step. 
