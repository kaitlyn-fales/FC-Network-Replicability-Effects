# Replicability of FC in the Seven Resting-State Brain Networks Project Repository
 
This repository contains all of the information and code required to download the data, run the analysis, and reproduce the results from our study on the effects of pipeline, band-pass filtering, and brain parcellation on the replicability of functional brain networks.

**Title:** Replicability of Functional Brain Networks: A Study Through the Lens of Seven Resting-State Networks

**Authors:** Kaitlyn R. Fales, Xurui (Ian) Zhi, Hyebin Song, Nicole A. Lazar

**Abstract:** The study of brain networks is essential for improving our understanding of how the human brain functions. Functional connectivity analysis (FC) is a widely used approach for studying co-activating patterns among brain regions by estimating their temporal dependencies and constructing an undirected network. Data processing is critical before estimating a subject’s functional network, but the absence of a standardized procedure serves as a source of heterogeneity in results, especially in multi-site studies. Commonly studied functional networks include the default mode, sensorimotor, visual, salience, dorsal attention, frontoparietal, and language networks. These networks are stable and still exhibit intrinsic activation when an individual is at rest, making them ideal networks to focus on for studying how processing choices affect the replicability of functional connectivity networks. We use the aforementioned seven networks to assess the impact of various processing choices, including preprocessing pipeline, band-pass filtering, and brain parcellation, on the replicability of functional connectivity estimates for multi-site resting-state fMRI (rs-fMRI) data from the Autism Brain Imaging Data Exchange (ABIDE). Finally, we provide some practical recommendations for how researchers should proceed with processing choices in the face of these effects.

-----

All of the script files in the repository are set up so that each file can run on its own, with the exception of the few script files that load the raw data directly (metadata.R and the scripts in FC_Network_Generation folder). 

To run the code and reproduce the results in the paper from start to finish including downloading the data, start from Step 1 below. Please note that the raw data will take a little over two hours to download from the ABIDE website (http://preprocessed-connectomes-project.org/abide/download.html) and is about 16 GB in size. We do not provide the raw data within this repository as it is not owned by us, and is already publicly available. 

If you do not wish to download the raw data, you can still reproduce the results by skipping Steps 1 and 2, and starting from Step 3.

## Step 1: Raw Data Download

First, navigate to the Raw Data Download folder. In that folder is the "Data_Download.R" script file. To download all of the .1D files from the ABIDE website, run this script file. You may need to update/check your directories before running this file. All the downloaded files should go into the Raw_Data subfolder, and will take about two hours to complete, depending on your machine. Once completed, you may move to Step 2. 

## Step 2: Data Preprocessing

All steps for data preprocessing and preparation are completed from the Data Preprocessing folder. 

### Step 2.1: Atlas Parcellation

This step includes the code and output files for the atlas parcellation and deciding which parcels from each atlas are included in the ROIs of the seven networks. Within the Atlas Parcellation subfolder, the ".xlsx" spreadsheets are considered the output files from this subfolder for use in preprocessing. To get the regions and values specified in these files, use the following procedure. 

First, download each of the atlases from the ABIDE website and save them to the ABIDE_Atlases subfolder (the link to download is provided in the subfolder). Then, make or login to your CONN toolbox account and download the "networks.nii" file and save it to the CONN_Atlas_Resliced subfolder. The Matlab batch files are also included in this folder to reproduce the output 3mm resliced files (all files with the "r" prefix). You are also welcome to skip this step and proceed to the next step, as all of the output dependencies for the next step are already loaded in the repository. 

Second, you can run the "parcellation.R" script to obtain all of the parcels and proportions to reproduce the values found in the ".xlsx" spreadsheets.

### Step 2.2: Metadata

The Metadata subfolder contains the code and the output .Rdata files with all the metadata from the ABIDE dataset. To reproduce the .RData files, you need only run the "metadata.R" script, which uses "summarize_metadata.R" as the source file. 

### Step 2.3: FC_Network_Generation

This step takes the raw data and generates the functional connectivity correlation matrices for every subject and preprocessing combination. There is an R script file corresponding to each atlas, with the regions and weights pulled from the ".xlsx" spreadsheets. The Metadata folder is used to exclude the necessary files as well. 

To complete this step, run the R script for each atlas (e.g., "cor_aal.R"), and the output .Rdata files can be found in the Data -> Correlation_Matrices subfolder. 

### Step 2.4: Creation of long-form dataframe

The final step of preprocessing is to take the output .Rdata files from Step 2.3 and combine them all into one long format dataframe for analysis. To complete this step, you need only run the "preprocess_data_network.R"  and "preprocess_data_between_network.R" scripts, and the resulting .RData files are located in the Data folder. These scripts compile all of the output files into a single dataframe, one for each file, cleans the data formatting up, and applies Fisher's *z*-transformation to the correlation coefficients. There is one file for the network edges *within* functional networks, and the other for network edges that are *between* two functional networks. 

## Step 3: Analysis

All steps for data analysis are completed from the Analysis folder. 

### Step 3.1: Exploratory Analysis

The script within the Exporatory Analysis subfolder is used to construct all three heatmaps in *Figure 1* of the manuscript, as well as the randomly selected mean FC networks plotted in *Figure 2.* Also included is the code for *Figure S1* in the supplement. 

### Step 3.2: Linear Mixed Effects Modeling

The main step of the analysis is to construct and run the linear mixed effects models for each edge. Navigate to the LMM subfolder, and run the "lmm.R" script. This will run all of the edgewise full LMMs discussed and reported on in the paper, and produces the outputs for *Tables 4-5* in the manuscript and *Tables S1-S3* in the Supplmentary Materials. The data used in this step is "processed_data_network.RData" and "processed_data_between_network.RData" which are the long-form dataframes produced at the end of Step 2. 

Other outputs of the "lmm.R" script include: the .RData files that estimate and control for (or subtract out from the response for) the random effects of site and subject, which are the .RData files with the suffix "regressed", and the .RData files that control for the estimated fixed effects for use in the baseline bootstrapping procedure later, which are the files with the suffix "baseline". These output files are in the Data folder, for use in later steps. 

The "pipeline_atlas_interaction_dmn.R" script examines the interaction effect between pipeline and atlas for the edges in the DMN, and reproduces *Figure 3.* 

The remaining steps can be performed in any order, as they use the outputs from Step 3.2. These outputs are all loaded in the Data folder already, and you may jump directly to these steps. We present them here in the order they are discussed within the paper. 

### Step 3.3: Functional Network Block Structure

The script within the Functional Network Block Structure folder uses the .RData files with the "regressed" suffix, and reproduces *Table 6.*

### Step 3.4: UMAP

The script within the UMAP folder uses the .RData files with the "regressed" suffix, and reproduces *Figures 4 and 7* in the manuscript, as well as *Figures S2-S3* in the Supplementary Materials.

### Step 3.5: Intra-Subject Variability

### Step 3.6: ComBat Harmonization (Yu et al. 2018) and Analysis

The script within the ComBat folder performs ComBat harmonization, and produces the output data files with the "combat" suffix found within the Data folder. Note there are two dataframes produced. The first is after ComBat harmonization, which controls for the site batch effect alone. The second .RData file has the suffix "combat_regressed" because it is the result after fitting the LMMs for each edge, estimating the random effect for the repeated measures of each subject, and subtracting that out from the response variable. The results from fitting the edgewise LMMs after ComBat harmonization reproduces the result shown in *Table 7*. 

The regressed data file is what is then used in the ComBat UMAP results. These results reproduce *Figure S4*. 





