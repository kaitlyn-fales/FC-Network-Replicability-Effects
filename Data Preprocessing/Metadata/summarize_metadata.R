summarize_metadata = function(base_path, subfolder, col_index){
  
  ## check for each file in sub_folder, whether there exist any columns in col_index with all zeros
  ## summarize nrow, any NA
  
  # read each .1D file in the sub-folder
  all_files = list.files(file.path(base_path, subfolder),
                         pattern = "\\.1D",
                         full.names = TRUE)
   
  # index for empty file
  ind_rm = 
    c(grep(pattern = c("UCLA_1_0051270"), x = all_files),
      grep(pattern = c("UCLA_2_0051310"), x = all_files))
  
  rm_files = all_files[ind_rm]
  all_files = all_files[-ind_rm]
  
  # load each file and check whether there is any columns with all zero entries
  pb = progress::progress_bar$new(total=length(all_files))
  n_zero_col = foreach(i = 1:length(all_files), .combine="c")%do%{
    pb$tick()
    file = all_files[i]
    dat_i = read.table(file, comment.char = "", header = TRUE)
    dat_i = dat_i[,col_index]
    sum(apply(abs(dat_i), 2, sum)  == 0)
  }
  
  rm_files = c(rm_files, all_files[which(n_zero_col>0)])
  if(length(which(n_zero_col>0))){
    all_files = all_files[-which(n_zero_col>0)]
  }
  
  
  ### meta_information for each file in all_files ###
  
  # obtain location info for each file
  center = gsub(pattern = paste0(base_path,"/"), replacement = "", x = all_files) %>% 
    gsub(pattern = paste0(subfolder,"/"), replacement = "") %>% 
    strsplit(split = "_0") %>% 
    sapply(function(x)x[1])
  
  
  # load each file and obtain meta information
  pb = progress::progress_bar$new(total=length(all_files))
  meta_info = foreach(i = 1:length(all_files), .combine="rbind")%do%{
    pb$tick()
    # print(i)
    file = all_files[i]
    dat_i = read.table(file, comment.char = "", header = TRUE)
    dat_i = dat_i[,col_index]
    
    data.frame(
      n_row = nrow(dat_i), # nrow
      n_na = sum(is.na(dat_i)), # number of n/a
      n_zero_elem = sum(abs(dat_i) ==0), # number of zero element
      n_zero_col = sum(apply(abs(dat_i), 2, sum)  == 0)) # number of columns with all zero entries
  }
  meta_info = data.frame(file=
                           gsub(pattern = paste0(base_path,"/"), replacement = "", x = all_files) %>% 
                           gsub(pattern = paste0(subfolder,"/"), replacement = ""),
                         center,meta_info)
  
  meta_info_summary = meta_info[,2:ncol(meta_info)] %>% group_by(center) %>% summarise_all(.funs = c("mean","sd"))
  included_files = gsub(all_files,pattern=base_path, replacement="") # strip out the base path
  removed_files = gsub(rm_files, pattern=base_path,replacement="")
  
  return(list(meta_info = meta_info, 
              meta_info_summary = meta_info_summary,
              included_files = included_files,
              removed_files = removed_files))
  
}

