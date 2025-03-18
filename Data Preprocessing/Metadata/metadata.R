library(foreach)
library(dplyr)
remove(list = ls())


#### change the paths for your local paths ####
setwd("/Users/xuruizhi/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Scanner Heterogeneity Project/Metadata")
source("summarize_metadata.R")
base_path = "/Users/xuruizhi/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Scanner Heterogeneity Project/Preprocessed Data"
# do not put / at the end


###
generate_metafiles = function(base_path, subfolders, col_index) {
  meta_lst = foreach(index = 1:length(subfolders)) %do% {
    subfolder = subfolders[index]
    cat("subfolder:", subfolder, "\n")
    out = summarize_metadata(base_path = base_path, subfolder = subfolder, col_index = col_index)
    assign(paste0("meta_", subfolder), out)
  }
  
  names(meta_lst) = subfolders
  return(meta_lst)
}




##### AAL
aal_subfolders <- c("ccs_filt_noglobal_rois_aal", "ccs_nofilt_noglobal_rois_aal", 
                    "cpac_filt_noglobal_rois_aal", "cpac_nofilt_noglobal_rois_aal", 
                    "dparsf_filt_noglobal_rois_aal", "dparsf_nofilt_noglobal_rois_aal", 
                    "niak_filt_noglobal_rois_aal", "niak_nofilt_noglobal_rois_aal")
Index_AAL <- rep(c("X.2611", "X.2612", 
                   "X.5201", 
                   "X.5202", "X.6222", 
                   "X.6301", "X.6302"), 
                 each = 1)

meta_aal = generate_metafiles(base_path = base_path, subfolders = aal_subfolders, col_index = Index_AAL)
save(meta_aal, file = "aal.Rdata")


##### EZ
ez_subfolders <- c("ccs_filt_noglobal_rois_ez", "ccs_nofilt_noglobal_rois_ez", 
                   "cpac_filt_noglobal_rois_ez", "cpac_nofilt_noglobal_rois_ez", 
                   "dparsf_filt_noglobal_rois_ez", "dparsf_nofilt_noglobal_rois_ez", 
                   "niak_filt_noglobal_rois_ez", "niak_nofilt_noglobal_rois_ez")
Index_EZ <- rep(c("X.25", "X.26", 
                  "X.51", 
                  "X.52", "X.86", 
                  "X.67", "X.68"), 
                each = 1)

meta_ez = generate_metafiles(base_path = base_path, subfolders = ez_subfolders, col_index = Index_EZ)
save(meta_ez, file = "ez.Rdata")


##### HO
ho_subfolders <- c("ccs_filt_noglobal_rois_ho", "ccs_nofilt_noglobal_rois_ho", 
                   "cpac_filt_noglobal_rois_ho", "cpac_nofilt_noglobal_rois_ho", 
                   "dparsf_filt_noglobal_rois_ho", "dparsf_nofilt_noglobal_rois_ho", 
                   "niak_filt_noglobal_rois_ho", "niak_nofilt_noglobal_rois_ho")
Index_HO <- rep(c("X.2501", 
                  "X.2202", 
                  "X.2201", 
                  "X.3101", "X.3102"), 
                each = 1)

meta_ho = generate_metafiles(base_path = base_path, subfolders = ho_subfolders, col_index = Index_HO)
save(meta_ho, file = "ho.Rdata")


##### TT
tt_subfolders <- c("ccs_filt_noglobal_rois_tt", "ccs_nofilt_noglobal_rois_tt", 
                   "cpac_filt_noglobal_rois_tt", "cpac_nofilt_noglobal_rois_tt", 
                   "dparsf_filt_noglobal_rois_tt", "dparsf_nofilt_noglobal_rois_tt", 
                   "niak_filt_noglobal_rois_tt", "niak_nofilt_noglobal_rois_tt")
Index_TT <- rep(c("X.5201", "X.5202",
                  "X.3702",
                  "X.3501", "X.4101",
                  "X.4501", "X.4502"),
                each = 1)

meta_tt = generate_metafiles(base_path = base_path, subfolders = tt_subfolders, col_index = Index_TT)
save(meta_tt, file = "tt.Rdata")


##### CC200
cc200_subfolders <- c("ccs_filt_noglobal_rois_cc200", "ccs_nofilt_noglobal_rois_cc200", 
                      "cpac_filt_noglobal_rois_cc200", "cpac_nofilt_noglobal_rois_cc200", 
                      "dparsf_filt_noglobal_rois_cc200", "dparsf_nofilt_noglobal_rois_cc200", 
                      "niak_filt_noglobal_rois_cc200", "niak_nofilt_noglobal_rois_cc200")
Index_CC200 <- rep(c("X.51", "X.109", "X.139", 
                     "X.82", "X.97", "X.114", 
                     "X.14", "X.166", "X.170", 
                     "X.3", "X.19", "X.163", "X.174"), 
                   each = 1)

meta_cc200 = generate_metafiles(base_path = base_path, subfolders = cc200_subfolders, col_index = Index_CC200)
save(meta_cc200, file = "cc200.Rdata")


##### CC400
cc400_subfolders <- c("ccs_filt_noglobal_rois_cc400", "ccs_nofilt_noglobal_rois_cc400", 
                      "cpac_filt_noglobal_rois_cc400", "cpac_nofilt_noglobal_rois_cc400", 
                      "dparsf_filt_noglobal_rois_cc400", "dparsf_nofilt_noglobal_rois_cc400", 
                      "niak_filt_noglobal_rois_cc400", "niak_nofilt_noglobal_rois_cc400")
Index_CC400 <- rep(c("X.84", "X.142", "X.232", "X.264", 
                     "X.6", "X.120", "X.189", 
                     "X.26", "X.135", "X.252", "X.341", 
                     "X.193"), 
                   each = 1)

meta_cc400 = generate_metafiles(base_path = base_path, subfolders = cc400_subfolders, col_index = Index_CC400)
save(meta_cc400, file = "cc400.Rdata")


# investigate and record files in the excel spreadsheet (sample code)
meta_aal$ccs_filt_noglobal_rois_aal$meta_info_summary %>% View()
meta_aal$ccs_filt_noglobal_rois_aal$meta_info %>% View()
meta_aal$ccs_filt_noglobal_rois_aal$removed_files

