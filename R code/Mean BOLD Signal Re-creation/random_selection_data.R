######## Random selection of File ID and preproc combos to examine #############

setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Packages
library(tidyverse)

# Load in phenotype summary csv file
summary <- read_csv("R code/Phenotypic_V1_0b_preprocessed1.csv")

# Filter to control group
summary <- summary %>% filter(DX_GROUP == 2)

# For those with no_filename, add them manually
for (i in 1:length(summary$FILE_ID)){
  if (summary$FILE_ID[i] == "no_filename") {
    summary$FILE_ID[i] <- paste(summary$SITE_ID[i],"_00",summary$subject[i],sep = "")
  }
}

# Grab just file_ID
file_ID <- summary$FILE_ID

# Get rid of two IDs that don't have data
file_ID <- file_ID[! file_ID %in% c("UCLA_1_0051270","UCLA_2_0051310")]

# Split data into entries with 0 in data and those that don't
problem_ID <- c("Caltech_0051478","Caltech_0051490","CMU_a_0050666","Leuven_2_0050727",
                "SDSU_0050195","SDSU_0050209","Yale_0050557")

good_ID <- file_ID[! file_ID %in% problem_ID]

##### Randomly select two problem file ID
set.seed(1)
sub_problem_ID <- sample(problem_ID, 2)

# Need to select preprocessing pipeline for each

### Caltech
# Select atlas
set.seed(100)
atlases <- c("aal","ez","ho","tt","cc200","cc400")
atlas_Caltech <- sample(atlases, 1)

# Caltech only issue with cpac pipeline - no selection needed
# Selection of filtering
set.seed(100)
preproc_Caltech <- c("cpac_filt_noglobal","cpac_nofilt_noglobal")
selection_Caltech <- sample(preproc_Caltech, 1)

#### Leuven
# Leuven only issue with CC200 atlas - no selection needed
# Leuven selection
set.seed(100)
preproc_Leuven <- c("ccs_filt_noglobal","ccs_nofilt_noglobal",
                    "dparsf_filt_noglobal","dparsf_nofilt_noglobal")
selection_Leuven <- sample(preproc_Leuven, 1)

# Final files for inspection
problem_ID_final <- cbind("pipeline" = c("cpac","ccs"), 
                       "strategy" = rep("nofilt_noglobal",2),  
                       "derivative" = c(atlas_Caltech,"cc200"),
                       "file identifier" = sub_problem_ID)

##### Randomly select two good file ID
set.seed(1)
sub_good_ID <- sample(good_ID, 2)

# Randomly select preprocessing pipeline
preproc <- c("ccs","cpac","dparsf","niak")
filt <- c("filt_noglobal","nofilt_noglobal")

set.seed(100)
good_ID_final <- cbind("pipeline" = sample(preproc, 2, replace = T), 
                       "strategy" = sample(filt, 2, replace = T),  
                       "derivative" = "func_preproc",
                       "file identifier" = sub_good_ID)

# Generate URLs
URL_start <- "https://s3.amazonaws.com/fcp-indi/data/Projects/ABIDE_Initiative/Outputs/"
ext <- ".nii.gz"

# Put dataframes together
info_URL <- data.frame(rbind(problem_ID_final,good_ID_final))
URLs <- rep(NA,4)
for (i in 1:length(URLs)){
  URLs[i] <- paste(URL_start,info_URL$pipeline[i],"/",info_URL$strategy[i],"/func_preproc/",
                   info_URL$file.identifier[i],"_func_preproc",ext, sep = "")
}






