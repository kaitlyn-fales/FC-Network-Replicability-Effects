This subfolder contains all of the CONN toolbox network atlas files. In order to run the conn_separation.R code, you must first download the networks.nii file from the CONN toolbox, which is publicly available once you have created an account. 

All other files are the resulting files created from the conn_separation.R script, and then after reslicing into 3mm space with two different bounding boxes using SPM12. The .mat files are the batch files used from SPM12. 

The resliced files all have the prefix "r", and are dependencies of the atlas_parcellation_preprocessing.R script. 
