# Replicability of FC in the DMN Project Repository
 
This repository contains all of the information and code required to download the data, run the analysis, and reproduce the results from our study on the effects of pipeline, band-pass filtering, and brain parcellation on the replicability of functional brain networks. **Please note, this repository is still in progress. Thank you for your understanding!**

**Title:** Replicability of Functional Brain Networks: A Study Through the Lens of the Default Mode Network

**Authors:** Kaitlyn R. Fales, Xurui (Ian) Zhi, Hyebin Song, Nicole Lazar

**Abstract:** The study of brain networks is widely used for improving our understanding how the human brain functions. Functional connectivity (FC) is one way to approach brain networks, as it examines temporal dependencies among brain regions in an undirected network. Before estimating a subject's functional network, the data must be preprocessed, but there is no consensus on a single preprocessing stream, giving way to sources of heterogeneity, especially for multi-site data. The default mode network (DMN) is well studied, stable, and active when an individual is unfocused or at rest, making it ideal for studying how sources of heterogeneity affect the replicability of a functional connectivity network. We use the DMN as the tool for assessing the impact of preprocessing pipeline, band-pass filtering, and brain parcellation on the replicability of functional connectivity estimates for multi-site resting-state fMRI (rs-fMRI) data from the Autism Brain Imaging Data Exchange (ABIDE). We also provide practical recommendations for how researchers should proceed with preprocessing choices in the face of these effects.

-----

All of the script files in the repository are set up so that each file can run on its own, with the exception of the few script files that load the raw data directly (metadata.R and the scripts in FC_Network_Generation folder). 

To run the code and reproduce the results in the paper from start to finish including downloading the data, start from Step 1 below. Please note that the raw data will take a little over one hour to download from the ABIDE website (http://preprocessed-connectomes-project.org/abide/download.html). We do not provide the raw data within this repository as it is not owned by us, and is already publicly available. 

If you do not wish to download the raw data, you can still reproduce the results by skipping Steps 1 and 2, and starting from Step 3.

## Step 1: Raw Data Download

First, navigate to the Raw Data Download folder. In that folder is the Data_Download.R script file. To download all of the .1D files from the ABIDE website, run this script file. You may need to update/check your directories before running this file. All the downloaded files should go into the Raw_Data subfolder, and will take about an hour to complete. Once completed, you may move to Step 2. 

## Step 2: Data Preprocessing

## Step 3: Analysis

## Step 4: Figures

## Optional: Supplementary Materials



