library(foreach)
library(dplyr)
remove(list = ls())

#### Change Working Directory if Necessary ####
#setwd("~/FC-Network-Replicability-Effects")

# Set atlas parcellation directory path
base_directory <- getwd()

#### change the paths for your local paths ####
setwd(paste0(base_directory,"/Data Preprocessing/Metadata"))
source("summarize_metadata.R")
base_path = paste0(base_directory,"/Raw Data Download/Raw_Data")
atlas_path = paste0(base_directory,"/Data Preprocessing/Atlas Parcellation")
# do not put / at the end


###
generate_metafiles = function(base_path, subfolders, col_index, included_file_ID) {
  meta_lst = foreach(index = 1:length(subfolders)) %do% {
    subfolder = subfolders[index]
    cat("subfolder:", subfolder, "\n")
    out = summarize_metadata(base_path = base_path, subfolder = subfolder, 
                             col_index = col_index, included_file_ID = included_file_ID)
    assign(paste0("meta_", subfolder), out)
  }
  
  names(meta_lst) = subfolders
  return(meta_lst)
}


# Load QA/QC inclusion criteria file with IDs
summary_file <- read.csv("Summary_Data_Filter_Inclusion.csv")
included_ID <- summary_file$FILE_ID


##### AAL
aal_subfolders <- c("ccs_filt_noglobal_rois_aal", "ccs_nofilt_noglobal_rois_aal", 
                    "cpac_filt_noglobal_rois_aal", "cpac_nofilt_noglobal_rois_aal", 
                    "dparsf_filt_noglobal_rois_aal", "dparsf_nofilt_noglobal_rois_aal", 
                    "niak_filt_noglobal_rois_aal", "niak_nofilt_noglobal_rois_aal")
load(paste(atlas_path,"aal_roi.RData",sep = "/"))
Index_AAL <- unique(unlist(sapply(aal_roi, "[[", "col_index")))

meta_aal = generate_metafiles(base_path = base_path, subfolders = aal_subfolders, 
                              col_index = Index_AAL, included_file_ID = included_ID)
save(meta_aal, file = "aal.Rdata")


##### EZ
ez_subfolders <- c("ccs_filt_noglobal_rois_ez", "ccs_nofilt_noglobal_rois_ez", 
                   "cpac_filt_noglobal_rois_ez", "cpac_nofilt_noglobal_rois_ez", 
                   "dparsf_filt_noglobal_rois_ez", "dparsf_nofilt_noglobal_rois_ez", 
                   "niak_filt_noglobal_rois_ez", "niak_nofilt_noglobal_rois_ez")
load(paste(atlas_path,"ez_roi.RData",sep = "/"))
Index_EZ <- unique(unlist(sapply(ez_roi, "[[", "col_index")))

meta_ez = generate_metafiles(base_path = base_path, subfolders = ez_subfolders, 
                             col_index = Index_EZ, included_file_ID = included_ID)
save(meta_ez, file = "ez.Rdata")


##### HO
ho_subfolders <- c("ccs_filt_noglobal_rois_ho", "ccs_nofilt_noglobal_rois_ho", 
                   "cpac_filt_noglobal_rois_ho", "cpac_nofilt_noglobal_rois_ho", 
                   "dparsf_filt_noglobal_rois_ho", "dparsf_nofilt_noglobal_rois_ho", 
                   "niak_filt_noglobal_rois_ho", "niak_nofilt_noglobal_rois_ho")
load(paste(atlas_path,"ho_roi.RData",sep = "/"))
Index_HO <- unique(unlist(sapply(ho_roi, "[[", "col_index")))

meta_ho = generate_metafiles(base_path = base_path, subfolders = ho_subfolders, 
                             col_index = Index_HO, included_file_ID = included_ID)
save(meta_ho, file = "ho.Rdata")


##### TT
tt_subfolders <- c("ccs_filt_noglobal_rois_tt", "ccs_nofilt_noglobal_rois_tt", 
                   "cpac_filt_noglobal_rois_tt", "cpac_nofilt_noglobal_rois_tt", 
                   "dparsf_filt_noglobal_rois_tt", "dparsf_nofilt_noglobal_rois_tt", 
                   "niak_filt_noglobal_rois_tt", "niak_nofilt_noglobal_rois_tt")
load(paste(atlas_path,"tt_roi.RData",sep = "/"))
Index_TT <- unique(unlist(sapply(tt_roi, "[[", "col_index")))

meta_tt = generate_metafiles(base_path = base_path, subfolders = tt_subfolders, 
                             col_index = Index_TT, included_file_ID = included_ID)
save(meta_tt, file = "tt.Rdata")


##### CC200
cc200_subfolders <- c("ccs_filt_noglobal_rois_cc200", "ccs_nofilt_noglobal_rois_cc200", 
                      "cpac_filt_noglobal_rois_cc200", "cpac_nofilt_noglobal_rois_cc200", 
                      "dparsf_filt_noglobal_rois_cc200", "dparsf_nofilt_noglobal_rois_cc200", 
                      "niak_filt_noglobal_rois_cc200", "niak_nofilt_noglobal_rois_cc200")
load(paste(atlas_path,"cc200_roi.RData",sep = "/"))
Index_CC200 <- unique(unlist(sapply(cc200_roi, "[[", "col_index")))

meta_cc200 = generate_metafiles(base_path = base_path, subfolders = cc200_subfolders, 
                                col_index = Index_CC200, included_file_ID = included_ID)
save(meta_cc200, file = "cc200.Rdata")


##### CC400
cc400_subfolders <- c("ccs_filt_noglobal_rois_cc400", "ccs_nofilt_noglobal_rois_cc400", 
                      "cpac_filt_noglobal_rois_cc400", "cpac_nofilt_noglobal_rois_cc400", 
                      "dparsf_filt_noglobal_rois_cc400", "dparsf_nofilt_noglobal_rois_cc400", 
                      "niak_filt_noglobal_rois_cc400", "niak_nofilt_noglobal_rois_cc400")
load(paste(atlas_path,"cc400_roi.RData",sep = "/"))
Index_CC400 <- unique(unlist(sapply(cc400_roi, "[[", "col_index")))

meta_cc400 = generate_metafiles(base_path = base_path, subfolders = cc400_subfolders, 
                                col_index = Index_CC400, included_file_ID = included_ID)
save(meta_cc400, file = "cc400.Rdata")

##### DOS
dos_subfolders <- c("ccs_filt_noglobal_rois_dosenbach160", "ccs_nofilt_noglobal_rois_dosenbach160", 
                    "cpac_filt_noglobal_rois_dosenbach160", "cpac_nofilt_noglobal_rois_dosenbach160", 
                    "dparsf_filt_noglobal_rois_dosenbach160", "dparsf_nofilt_noglobal_rois_dosenbach160", 
                    "niak_filt_noglobal_rois_dosenbach160", "niak_nofilt_noglobal_rois_dosenbach160")
load(paste(atlas_path,"dos_roi.RData",sep = "/"))
Index_DOS <- unique(unlist(sapply(dos_roi, "[[", "col_index")))

meta_dos = generate_metafiles(base_path = base_path, subfolders = dos_subfolders, 
                              col_index = Index_DOS, included_file_ID = included_ID)
save(meta_dos, file = "dos.Rdata")


# investigate and record files in the excel spreadsheet (sample code)
meta_aal$ccs_filt_noglobal_rois_aal$meta_info_summary %>% View()
meta_aal$ccs_filt_noglobal_rois_aal$meta_info %>% View()
meta_aal$ccs_filt_noglobal_rois_aal$removed_files

