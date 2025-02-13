library(foreach)
library(dplyr)
remove(list=ls())

#### change the paths for your local paths ####
setwd("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Scanner Heterogeneity Project/Metadata")
source("summarize_metadata.R")
base_path = "/Users/hyebin/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Scanner Heterogeneity Project/Preprocessed Data" # do not put / at the end
###

generate_metafiles = function(base_path, subfolders, col_index){
  meta_lst = foreach(index =1:length(subfolders))%do%{
    subfolder = subfolders[index]
    cat("subfolder:",subfolder,"\n")
    out = summarize_metadata(base_path = base_path,subfolder = subfolder,col_index = col_index)
    assign(paste0("meta_",subfolder), out)
  }
  
  names(meta_lst) =subfolders
  return(meta_lst)
}

##### AAL
aal_subfolders <- c("ccs_filt_noglobal_rois_aal", "ccs_nofilt_noglobal_rois_aal", 
                    "cpac_filt_noglobal_rois_aal", "cpac_nofilt_noglobal_rois_aal", 
                    "dparsf_filt_noglobal_rois_aal", "dparsf_nofilt_noglobal_rois_aal", 
                    "niak_filt_noglobal_rois_aal", "niak_nofilt_noglobal_rois_aal")
Index_AAL   <- rep(c("X.2611", "X.2612",
                     "X.5201",
                     "X.5202", "X.6222",
                     "X.6301", "X.6302"),
                   each = 1)


meta_aal = generate_metafiles(base_path = base_path, subfolders = aal_subfolders,col_index = Index_AAL)
save(meta_aal,file = "aal.Rdata")

# investigate and record files in the excel spreadsheet
meta_aal$ccs_filt_noglobal_rois_aal$meta_info_summary %>% View()
meta_aal$ccs_filt_noglobal_rois_aal$meta_info %>% View()
meta_aal$ccs_filt_noglobal_rois_aal$removed_files

##### tt