The contents of this folder are as follows:

**Correlation_Matrices:** This folder contains the FC matrix .RData files for each of the 48 preprocessing combinations that are used as the basis for the remainder of the analysis. The files in this folder can be considered the output of the Data folder as a whole.

**FC_Network_Generation:** This folder contains the R codes to create the .RData files in the Correlation_Matrices folder. There is one R script file for each of the six (6) parcellations we used. The only difference from one file to the next is the weights used for the size of each parcel, as explained in the paper.

**Preprocessed_Data:** This is an empty folder that is the destination directory for the raw data files after being downloaded using the script Data_Download.R.

**Data_Download.R:** This script downloads all of the raw data from the ABIDE website into the Preprocessed_Data folder.

**Summary Data_Fixed.csv:** This file contains the participant level information from the ABIDE website, excluding the two subject IDs for which there is no data for, as explained in the paper. This is also elaborated on more in the Data_Download.R file.
