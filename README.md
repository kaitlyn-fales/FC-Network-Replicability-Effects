# Replicability of FC in the DMN Project Repository
 
This repository contains all of the information and code required to download the data, run the analysis, and reproduce the results from our study on the effects of pipeline, band-pass filtering, and brain parcellation on the replicability of functional brain networks. **Please note, this repository is still in progress. Thank you for your understanding!**

**Title:** Replicability of Functional Brain Networks: A Study Through the Lens of the Default Mode Network

**Authors:** Kaitlyn R. Fales, Xurui (Ian) Zhi, Hyebin Song, Nicole A. Lazar

**Abstract:** The study of brain networks is essential for improving our understanding of how the human brain functions. Functional connectivity analysis (FC) is a widely used approach for studying co-activating patterns among brain regions by estimating their temporal dependencies and constructing an undirected network. Preprocessing is a critical step before estimating a subject's functional network, but the absence of a standardized pipeline serves as a source of heterogeneity in results, especially in multi-site studies. The default mode network (DMN) is well studied, stable, and active when an individual is unfocused or at rest, making it an ideal network to focus on for studying how preprocessing choices affect the replicability of functional connectivity networks. We use the DMN to assess the impact of various preprocessing choices, including preprocessing pipelines, band-pass filtering, and brain parcellation, on the replicability of functional connectivity estimates for multi-site resting-state fMRI (rs-fMRI) data from the Autism Brain Imaging Data Exchange (ABIDE). Finally, we provide some practical recommendations for how researchers should proceed with preprocessing choices in the face of these effects.

-----

All of the script files in the repository are set up so that each file can run on its own, with the exception of the few script files that load the raw data directly (metadata.R and the scripts in FC_Network_Generation folder). 

To run the code and reproduce the results in the paper from start to finish including downloading the data, start from Step 1 below. Please note that the raw data will take a little over two hours to download from the ABIDE website (http://preprocessed-connectomes-project.org/abide/download.html) and is about 16 GB in size. We do not provide the raw data within this repository as it is not owned by us, and is already publicly available. 

If you do not wish to download the raw data, you can still reproduce the results by skipping Steps 1 and 2, and starting from Step 3.

## Step 1: Raw Data Download

First, navigate to the Raw Data Download folder. In that folder is the "Data_Download.R" script file. To download all of the .1D files from the ABIDE website, run this script file. You may need to update/check your directories before running this file. All the downloaded files should go into the Raw_Data subfolder, and will take about two hours to complete, depending on your machine. Once completed, you may move to Step 2. 

## Step 2: Data Preprocessing

All steps for data preprocessing and preparation are completed from the Data Preprocessing folder. 

### Step 2.1: Atlas Parcellation

This step includes the code and output files for the atlas parcellation and deciding which parcels from each atlas are included in the ROIs of the default mode network. Within the Atlas Parcellation subfolder, the "ROI_atlas_labels.xlsx" spreadsheet is considered the output file from this subfolder for use in preprocessing. To get the regions and values specified in this file, use the following procedure. 

First, download each of the atlases from the ABIDE website and save them to the ABIDE_Atlases subfolder (the link to download is provided in the subfolder). Then, make or login to your CONN toolbox account and download the "networks.nii" file and save it to the CONN_Atlas_Resliced subfolder. 

Next, run the "conn_separation.R" script to separate the CONN atlas into a file for each ROI within the DMN. These files are then resliced in SPM12. The Matlab batch files are also included in this folder to reproduce the output 3mm resliced files (all files with the "r" prefix). You are also welcome to skip this step and proceed to the next step, as all of the output dependencies for the next step are already loaded in the repository. 

Third, you can run the "atlas_parcellation_preprocessing.R" script to obtain all of the overlap calculations and volume tables to reproduce the values found in the "ROI_atlas_labels.xlsx" spreadsheet.

### Step 2.2: Metadata

The Metadata subfolder contains the code and the output .Rdata files with all the metadata from the ABIDE dataset. To reproduce the .RData files, you need only run the "metadata.R" script, which uses "summarize_metadata.R" as the source file. The "removed_files.xlsx" spreadsheet summarizes the results from the Metadata folder by identifying the files that needed to be excluded from the analysis (explained in the manuscript). 

### Step 2.3: FC_Network_Generation

This step takes the raw data and generates the functional connectivity correlation matrices for every subject and preprocessing combination. There is an R script file corresponding to each atlas, with the regions and weights pulled from the "ROI_atlas_labels.xlsx" spreadsheet. The Metadata folder is used to exclude the necessary files as well. 

To complete this step, run the R script for each atlas (e.g., "cor_aal.R"), and the output .Rdata files can be found in the Data -> Correlation_Matrices subfolder. 

### Step 2.4: Creation of long-form dataframe

The final step of preprocessing is to take the output .Rdata files from Step 2.3 and combine them all into one long format dataframe for analysis. To complete this step, you need only run the "preprocess_data.R" file, and the resulting .RData file is located in the Data folder. This script compiles all of the output files into a single dataframe, cleans the data formatting up, and applies Fisher's *z*-transformation to the correlation coefficients. 

## Step 3: Analysis

## Step 4: Figures

## Optional: Supplementary Materials



