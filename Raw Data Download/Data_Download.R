##### The following code will download the raw preprocessed data #####


#### Change Working Directory if Necessary ####
#setwd("~/FC-Network-Replicability-Effects")


#### (Install and) Load Packages ####
#install.packages("dplyr")
#install.packages("curl")
library(dplyr)
library(curl)


#### Import and Prepare Summary Spreadsheet ####

# NOTE:
  ## after downloading the original summary spreadsheet, we observed missing FILE_ID values ("no_filename")
  ## however these values can be manually input based on FILE_ID pattern, SUB_ID, and SITE_ID
  ## after manually inputting all missing FILE_ID values in Microsoft Excel

# read in the fixed summary table
summary_data <- read.csv("Raw Data Download/Summary_Data_Fixed.csv", header = TRUE)

# filter the data to make it only contain data for control group (DX_GROUP == 2)
filtered_data <- summary_data %>% filter(DX_GROUP == 2)

# view/check part of the filtered data
head(filtered_data)


#### file_id Vector ####
# vector for FILE_ID
file_id <- filtered_data$FILE_ID

# check number of subjects (573 total)
length(file_id)


#### Download All .1D Raw Files (about 150 mins and 16 GB) ####
# Note:
  ## Combinations and URL Templates for each single data sheet
    ### pipeline        (ccs, cpac, dparsf, niak)                                                           = 4
    ### strategy        (filt_noglobal, nofilt_noglobal)                                                    = 2
    ### derivative      (rois_aal, rois_cc200, rois_cc400, (rois_dosenbach160), rois_ez, rois_ho, rois_tt)  = 6 (dropped dosenbach160)
    ### file identifier (FILE_ID)                                                                           = 573
    ### ext             (1D)
  ## Total of 4*2*6=48 combinations and each combination contains 573 subjects

# set base directory to save the downloaded 56 folders of data (change save path if necessary)
base_directory <- paste0(getwd(),"/Raw Data Download/Raw_Data")

# define template URL and vectors
template_url      <- "https://s3.amazonaws.com/fcp-indi/data/Projects/ABIDE_Initiative/Outputs/[pipeline]/[strategy]/[derivative]/[file_identifier]_[derivative].1D"
pipelines         <- c("ccs", "cpac", "dparsf", "niak")
strategies        <- c("filt_noglobal", "nofilt_noglobal")
derivatives       <- c("rois_aal", "rois_cc200", "rois_cc400", "rois_dosenbach160", "rois_ez", "rois_ho", "rois_tt")
file_identifiers  <- file_id

# create directories for each combination (56 total)
for (pipeline in pipelines) {
  for (strategy in strategies) {
    for (derivative in derivatives) {
      folder_path <- file.path(base_directory, paste0(pipeline, "_", strategy, "_", derivative))
      dir.create(folder_path, showWarnings = TRUE)
      
      # Download files for each file identifier
      for (file_id in file_identifiers) {
        url <- gsub("\\[pipeline\\]", pipeline, template_url)
        url <- gsub("\\[strategy\\]", strategy, url)
        url <- gsub("\\[derivative\\]", derivative, url)
        url <- gsub("\\[file_identifier\\]", toString(file_id), url)
        custom_file_name <- paste0(pipeline, "_", strategy, "_", derivative, "_", toString(file_id), ".1D")
        download_path <- file.path(folder_path, custom_file_name)
        # Print a message if the download fails and when download completes
        tryCatch({
          download.file(url, destfile = download_path, method = "curl", quiet = TRUE, mode = "wb")
        }, error = function(e) {
          cat(paste("Error downloading file from:", url, "\n", "Error message:", e$message, "\n"))
        })
      }
      cat(paste("Downloaded all files for", folder_path, "\n"))
    }
  }
}

# Note:
  ## These data are served as raw data, and should NOT be modified in this project.
  ## Although the downloading process was successful for all 573 subjects, there are 2 subjects (UCLA_1_0051270 and UCLA_2_0051310) have empty data, which the downloading URLs will show "NoSuchKey".
  ## There are .1D files for these two subjects but these .1D files are empty/unreadable.



