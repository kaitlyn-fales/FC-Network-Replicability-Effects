######### Linear mixed effect modeling of preprocessing pipelines ############

# Packages
library(stringr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(readxl)

######### Setting things up - prepping data ##################################
# Base directory that contains all .RData
base_dir <- "~/The Pennsylvania State University - Fales, Kaitlyn Rose - Scanner Heterogeneity Project/Correlation_Matrices"

# Base directory (Kaitlyn's local machine)
base_dir <- "C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Correlation_Matrices"

# Full paths to the files
file_list <- list.files(path = base_dir, pattern = "\\.RData$", full.names = TRUE, ignore.case = TRUE)

# Extract list of names of all 48 preprocessing combinations
combinations <- gsub("\\..*","", basename(file_list))

# Make a df containing the 48 preprocessing combos to use later
preproc_combo_df <- t(data.frame(strsplit(combinations, split = "_")))[,c(1:2,5)] # cols of interest
rownames(preproc_combo_df) <- NULL
colnames(preproc_combo_df) <- c("pipeline","filter","atlas")

# Make list of edges in FC network
edges <- c("MPFC.LP_L","MPFC.LP_R","LP_L.LP_R","MPFC.PCC","LP_L.PCC","LP_R.PCC")

# Incorporating metadata to get ID column - pulling directory and files
metadata_dir <- c("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project/Metadata")
metadata_files <- list.files(path = metadata_dir, pattern = "\\.RData$", 
                             full.names = TRUE, ignore.case = TRUE)

# Loading files
for (i in 1:length(metadata_files)) load(metadata_files[i])

# Function to extract the unique ID column from the corresponding metadata file
extract_meta <- function(combo, atlas) {
  # Grab metadata file according to which atlas for that combination
  type <- atlas
  
  if (type == "aal") {meta <- meta_aal} else {
    if  (type == "cc200") {meta <- meta_cc200} else {
      if  (type == "cc400") {meta <- meta_cc400} else {
        if  (type == "ez") {meta <- meta_ez} else {
          if  (type == "ho") {meta <- meta_ho} 
          else {meta <- meta_tt
          }
        }
      }
    }
  }
  
  # Extract list of included files
  included_files <- meta[[combo]]$included_files
  
  # Split the string into just the file name (get rid of path)
  included_files <- t(data.frame(strsplit(included_files, split = "/")))[,3]
  
  # Split the string by getting rid of everything after the ID
  included_files <- gsub("_rois.*","\\1",included_files)
  
  # Combine CMU_a and CMU_b to just be CMU (small sample size)
  included_files <- gsub("CMU_a","CMU",included_files)
  included_files <- gsub("CMU_b","CMU",included_files)
  
  # Combine MaxMun_a, MaxMun_b, and MaxMun_d to just be MaxMun (small sample size)
  included_files <- gsub("MaxMun_a","MaxMun",included_files)
  included_files <- gsub("MaxMun_c","MaxMun",included_files)
  included_files <- gsub("MaxMun_d","MaxMun",included_files)
  
  ### For sites with multiple samples, change "_" syntax
  
  # Replacing Leuven_1 with Leuven1 (same for 2)
  included_files <- gsub("Leuven_1","Leuven1",included_files)
  included_files <- gsub("Leuven_2","Leuven2",included_files)
  
  # Replacing UCLA_1 with UCLA1 (same for 2)
  included_files <- gsub("UCLA_1","UCLA1",included_files)
  included_files <- gsub("UCLA_2","UCLA2",included_files)
  
  # Replacing UM_1 with UM1 (same for 2)
  included_files <- gsub("UM_1","UM1",included_files)
  included_files <- gsub("UM_2","UM2",included_files)
  
  ###
  
  # Split string by _ character
  included_files <- t(data.frame(strsplit(included_files, split = "_")))
  rownames(included_files) <- NULL
  colnames(included_files) <- c("site","ID")
  
  return(included_files)
}

# Function to take in a list from 1 of 48 combos, vectorize and Fisher z-transform
fisher_transform <- function(cor_matrices){
  # Empty matrix to store result
  result <- matrix(NA, nrow = length(cor_matrices), ncol = 6)
  
  # Take each FC matrix and vectorize the upper triangle
  for (j in 1:length(cor_matrices)){
    r.vector <- cor_matrices[[j]]*upper.tri(cor_matrices[[j]], diag = F)
    result[j,] <- r.vector[r.vector != 0]
  }
  
  # Fisher's z-transform
  result <- atanh(result)
  
  # Return output
  return(result)
}

# Create empty matrix to store results from for loop (will need to get rid of extra NA at end)
df <- matrix(NA, nrow = 571*48, ncol = 11)
colnames(df) <- c("pipeline","filter","atlas","site","ID",edges)

# Starting running count for filling in df
count <- 0

# For loop to run across all 48 combinations to make master dataframe
for (i in 1:length(file_list)){
  # Load in individual file
  load(file_list[i])
  
  # Pull the name of the file into generic form
  temp <- get(combinations[i])
  
  # Apply the fisher_transform function
  fisher_result <- fisher_transform(temp)
  
  # Take the relevant preprocessing combo and make into design matrix
  combo <- matrix(preproc_combo_df[i,], nrow = nrow(fisher_result), 
                  ncol = ncol(preproc_combo_df), byrow = T)
  
  # Extract site and unique ID column from corresponding metadata file
  metadata <- extract_meta(combinations[i],preproc_combo_df[i,3])
  
  # Make dataframe for that particular combo, plus ID column, and response values
  dat_combo <- cbind(combo, metadata, fisher_result)
  
  # Add dat_combo to final dataframe
  df[(count+1):(count+nrow(dat_combo)),] <- dat_combo
  
  # Update running count of which rows we are at
  count <- count+nrow(dat_combo)
}

# Convert final matrix to dataframe
df <- as.data.frame(df)

# Convert preprocessing effects as factors
df$pipeline <- factor(df$pipeline, levels = c("cpac","dparsf","niak","ccs"))
df$filter <- factor(df$filter)
df$atlas <- factor(df$atlas, levels = c("cc200","cc400","ez","ho","tt","aal"))
df$site <- factor(df$site)
df$ID <- factor(df$ID)
df$MPFC.LP_L <- as.numeric(df$MPFC.LP_L)
df$MPFC.LP_R <- as.numeric(df$MPFC.LP_R)
df$LP_L.LP_R <- as.numeric(df$LP_L.LP_R)
df$MPFC.PCC <- as.numeric(df$MPFC.PCC)
df$LP_L.PCC <- as.numeric(df$LP_L.PCC)
df$LP_R.PCC <- as.numeric(df$LP_R.PCC)
str(df)

# Get rid of extra NA because of making the resulting empty df larger than needed
df <- df[complete.cases(df),]

# Export final dataframe to reference later
save(df, 
     file = "C:/Users/kaitl/OneDrive - The Pennsylvania State University/
     Scanner Heterogeneity Project/all_combos_dataframe.RData")

# Clear out everything else in environment except for dataframe
rm(list=setdiff(ls(), "df"))
##############################################################################

######### Additive linear mixed models for each edge #########################
# Additive LMM with explanatory variables of pipeline, filter, and atlas
# and a random intercept for each unique ID.

### MPFC.LP_L ###
# Fit model
MPFC.LP_L <- lmer(MPFC.LP_L ~ pipeline + filter + atlas + (1|ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

# Model summary
summary(MPFC.LP_L)

# Type III ANOVA (Partial Sum of Squares approach for unbalanced design)
MPFC.LP_L_anova <- anova(MPFC.LP_L, type = 3)[,c(1:2,5)]

# Diagnostic plots
plot(MPFC.LP_L, col = 1, main = "Standardized Residuals vs. Fitted Values")
qqnorm(residuals(MPFC.LP_L))
qqline(residuals(MPFC.LP_L))
plot(MPFC.LP_L,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1, col = 1,
     main = "Scale-Location", ylab = "sqrt(|Standardized Residuals|)")
plot(MPFC.LP_L, rstudent(.) ~ hatvalues(.), col = 1,
     ylab = "Studentized Residuals", xlab = "Leverage",
     main = "Studentized Residuals vs. Leverage")

### MPFC.LP_R ###
# Fit model
MPFC.LP_R <- lmer(MPFC.LP_R ~ pipeline + filter + atlas + (1|ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

# Model summary
summary(MPFC.LP_R)

# Type III ANOVA (Partial Sum of Squares approach for unbalanced design)
MPFC.LP_R_anova <- anova(MPFC.LP_R, type = 3)[,c(1:2,5)]

# Diagnostic plots
plot(MPFC.LP_R, col = 1, main = "Standardized Residuals vs. Fitted Values")
qqnorm(residuals(MPFC.LP_R))
qqline(residuals(MPFC.LP_R))
plot(MPFC.LP_R,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1, col = 1,
     main = "Scale-Location", ylab = "sqrt(|Standardized Residuals|)")
plot(MPFC.LP_R, rstudent(.) ~ hatvalues(.), col = 1,
     ylab = "Studentized Residuals", xlab = "Leverage",
     main = "Studentized Residuals vs. Leverage")

### LP_L.LP_R ###
# Fit model
LP_L.LP_R <- lmer(LP_L.LP_R ~ pipeline + filter + atlas + (1|ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

# Model summary
summary(LP_L.LP_R)

# Type III ANOVA (Partial Sum of Squares approach for unbalanced design)
LP_L.LP_R_anova <- anova(LP_L.LP_R, type = 3)[,c(1:2,5)]

# Diagnostic plots
plot(LP_L.LP_R, col = 1, main = "Standardized Residuals vs. Fitted Values")
qqnorm(residuals(LP_L.LP_R))
qqline(residuals(LP_L.LP_R))
plot(LP_L.LP_R,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1, col = 1,
     main = "Scale-Location", ylab = "sqrt(|Standardized Residuals|)")
plot(LP_L.LP_R, rstudent(.) ~ hatvalues(.), col = 1,
     ylab = "Studentized Residuals", xlab = "Leverage",
     main = "Studentized Residuals vs. Leverage")

### MPFC.PCC ###
# Fit model
MPFC.PCC <- lmer(MPFC.PCC ~ pipeline + filter + atlas + (1|ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# Model summary
summary(MPFC.PCC)

# Type III ANOVA (Partial Sum of Squares approach for unbalanced design)
MPFC.PCC_anova <- anova(MPFC.PCC, type = 3)[,c(1:2,5)]

# Diagnostic plots
plot(MPFC.PCC, col = 1, main = "Standardized Residuals vs. Fitted Values")
qqnorm(residuals(MPFC.PCC))
qqline(residuals(MPFC.PCC))
plot(MPFC.PCC,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1, col = 1,
     main = "Scale-Location", ylab = "sqrt(|Standardized Residuals|)")
plot(MPFC.PCC, rstudent(.) ~ hatvalues(.), col = 1,
     ylab = "Studentized Residuals", xlab = "Leverage",
     main = "Studentized Residuals vs. Leverage")

### LP_L.PCC ###
# Fit model
LP_L.PCC <- lmer(LP_L.PCC ~ pipeline + filter + atlas + (1|ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# Model summary
summary(LP_L.PCC)

# Type III ANOVA (Partial Sum of Squares approach for unbalanced design)
LP_L.PCC_anova <- anova(LP_L.PCC, type = 3)[,c(1:2,5)]

# Diagnostic plots
plot(LP_L.PCC, col = 1, main = "Standardized Residuals vs. Fitted Values")
qqnorm(residuals(LP_L.PCC))
qqline(residuals(LP_L.PCC))
plot(LP_L.PCC,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1, col = 1,
     main = "Scale-Location", ylab = "sqrt(|Standardized Residuals|)")
plot(LP_L.PCC, rstudent(.) ~ hatvalues(.), col = 1,
     ylab = "Studentized Residuals", xlab = "Leverage",
     main = "Studentized Residuals vs. Leverage")

### LP_R.PCC ###
# Fit model
LP_R.PCC <- lmer(LP_R.PCC ~ pipeline + filter + atlas + (1|ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# Model summary
summary(LP_R.PCC)

# Type III ANOVA (Partial Sum of Squares approach for unbalanced design)
LP_R.PCC_anova <- anova(LP_R.PCC, type = 3)[,c(1:2,5)]

# Diagnostic plots
plot(LP_R.PCC, col = 1, main = "Standardized Residuals vs. Fitted Values")
qqnorm(residuals(LP_R.PCC))
qqline(residuals(LP_R.PCC))
plot(LP_R.PCC,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1, col = 1,
     main = "Scale-Location", ylab = "sqrt(|Standardized Residuals|)")
plot(LP_R.PCC, rstudent(.) ~ hatvalues(.), col = 1,
     ylab = "Studentized Residuals", xlab = "Leverage",
     main = "Studentized Residuals vs. Leverage")
##############################################################################

######### Exporting results for additive models ##############################
# Extract coefficients and standard errors from each model to make table
results <- round(cbind(coef(summary(MPFC.LP_L))[,1:2],
                       coef(summary(MPFC.LP_R))[,1:2],
                       coef(summary(LP_L.LP_R))[,1:2],
                       coef(summary(MPFC.PCC))[,1:2],
                       coef(summary(LP_L.PCC))[,1:2],
                       coef(summary(LP_R.PCC))[,1:2]), digits = 4)
rownames(results) <- c("(Intercept)",levels(df$pipeline)[1:3],
                       levels(df$filter)[1],levels(df$atlas)[1:5])

# Write csv file with results
write.csv(results, 
          file = "C:/Users/kaitl/OneDrive - The Pennsylvania State University/
          Scanner Heterogeneity Project/preproc_combo_lmm_result.csv")


# Get results from ANOVA as well
DF <- c(3,1,5)
anova_results <- round(cbind(DF,MPFC.LP_L_anova,
                             MPFC.LP_R_anova,
                             LP_L.LP_R_anova,
                             MPFC.PCC_anova,
                             LP_L.PCC_anova,
                             LP_R.PCC_anova), digits = 4)

# Write csv with results
write.csv(anova_results, 
          file = "C:/Users/kaitl/OneDrive - The Pennsylvania State University/
          Scanner Heterogeneity Project/preproc_combo_lmm_anova_result.csv")
##############################################################################

######### Full linear mixed models for each edge (main results) ##############
# Full LMM with explanatory variables of pipeline, filter, and atlas
# and all possible interactions and a random intercept for ID nested 
# within each site.

### MPFC.LP_L ### 
# Fit model
MPFC.LP_L <- lmer(MPFC.LP_L ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

# Model summary
summary(MPFC.LP_L)

# ANOVA tables
MPFC.LP_L_result <- list(anova = anova(MPFC.LP_L, type = 3),
                         ranova = ranova(MPFC.LP_L))

### MPFC.LP_R ### 
# Fit model
MPFC.LP_R <- lmer(MPFC.LP_R ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

# Model summary
summary(MPFC.LP_R)

# ANOVA table
MPFC.LP_R_result <- list(anova = anova(MPFC.LP_R, type = 3),
                         ranova = ranova(MPFC.LP_R))

### LP_L.LP_R ### 
# Fit model
LP_L.LP_R <- lmer(LP_L.LP_R ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

# Model summary
summary(LP_L.LP_R)

# ANOVA table
LP_L.LP_R_result <- list(anova = anova(LP_L.LP_R, type = 3),
                         ranova = ranova(LP_L.LP_R))

### MPFC.PCC ### 
# Fit model
MPFC.PCC <- lmer(MPFC.PCC ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# Model summary
summary(MPFC.PCC)

# ANOVA table
MPFC.PCC_result <- list(anova = anova(MPFC.PCC, type = 3),
                        ranova = ranova(MPFC.PCC))

### LP_L.PCC ### 
# Fit model
LP_L.PCC <- lmer(LP_L.PCC ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# Model summary
summary(LP_L.PCC)

# ANOVA table
LP_L.PCC_result <- list(anova = anova(LP_L.PCC, type = 3),
                        ranova = ranova(LP_L.PCC))

### LP_R.PCC ### 
# Fit model
LP_R.PCC <- lmer(LP_R.PCC ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# Model summary
summary(LP_R.PCC)

# ANOVA table
LP_R.PCC_result <- list(anova = anova(LP_R.PCC, type = 3),
                        ranova = ranova(LP_R.PCC))
##############################################################################

######### Exporting results for full models ##################################
# Taking each individual edge list result and putting together in one list
fullmod_results <- list(MPFC.LP_L = MPFC.LP_L_result,
                        MPFC.LP_R = MPFC.LP_R_result,
                        LP_L.LP_R = LP_L.LP_R_result,
                        MPFC.PCC = MPFC.PCC_result,
                        LP_L.PCC = LP_L.PCC_result,
                        LP_R.PCC = LP_R.PCC_result)

# Saving as .RData
save(fullmod_results, file = "LMM_fullmodel_results.RData")
##############################################################################

######### Including scanner brand ############################################
load("all_combos_dataframe.RData")

# Load in scanning parameters
scan_param <- read_excel("Functional Scan Parameters/scan_param_summary.xlsx")

scan_param <- scan_param[,1:2]
colnames(scan_param) <- c("site","scanner")

# Merge df
df <- merge(df,scan_param,by = "site")

######### Full linear mixed models for each edge (with scanner) ##############
# Full LMM with explanatory variables of pipeline, filter, and atlas
# and all possible interactions and a random intercept for ID nested 
# within each site. Brand of scanner also included

### MPFC.LP_L ### 
# Fit model
MPFC.LP_L <- lmer(MPFC.LP_L ~ pipeline*filter*atlas + scanner + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum",
                                   scanner = "contr.sum"))

# Model summary
summary(MPFC.LP_L)

# ANOVA tables
MPFC.LP_L_result <- list(anova = anova(MPFC.LP_L, type = 3),
                         ranova = ranova(MPFC.LP_L))

### MPFC.LP_R ### 
# Fit model
MPFC.LP_R <- lmer(MPFC.LP_R ~ pipeline*filter*atlas + scanner + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum",
                                   scanner = "contr.sum"))

# Model summary
summary(MPFC.LP_R)

# ANOVA table
MPFC.LP_R_result <- list(anova = anova(MPFC.LP_R, type = 3),
                         ranova = ranova(MPFC.LP_R))

### LP_L.LP_R ### 
# Fit model
LP_L.LP_R <- lmer(LP_L.LP_R ~ pipeline*filter*atlas + scanner + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum",
                                   scanner = "contr.sum"))

# Model summary
summary(LP_L.LP_R)

# ANOVA table - scanner appears to be slightly more influential here
LP_L.LP_R_result <- list(anova = anova(LP_L.LP_R, type = 3),
                         ranova = ranova(LP_L.LP_R))

### MPFC.PCC ### 
# Fit model
MPFC.PCC <- lmer(MPFC.PCC ~ pipeline*filter*atlas + scanner + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum",
                                  scanner = "contr.sum"))

# Model summary
summary(MPFC.PCC)

# ANOVA table
MPFC.PCC_result <- list(anova = anova(MPFC.PCC, type = 3),
                        ranova = ranova(MPFC.PCC))

### LP_L.PCC ### 
# Fit model
LP_L.PCC <- lmer(LP_L.PCC ~ pipeline*filter*atlas + scanner + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum",
                                  scanner = "contr.sum"))

# Model summary
summary(LP_L.PCC)

# ANOVA table - scanner appears to be slightly more influential here
LP_L.PCC_result <- list(anova = anova(LP_L.PCC, type = 3),
                        ranova = ranova(LP_L.PCC))

### LP_R.PCC ### 
# Fit model
LP_R.PCC <- lmer(LP_R.PCC ~ pipeline*filter*atlas + scanner + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum",
                                  scanner = "contr.sum"))

# Model summary
summary(LP_R.PCC)

# ANOVA table - scanner appears to be slightly more influential here
LP_R.PCC_result <- list(anova = anova(LP_R.PCC, type = 3),
                        ranova = ranova(LP_R.PCC))

######### Exporting results for full models + brand ##########################
# Taking each individual edge list result and putting together in one list
fullmod_results_brand <- list(MPFC.LP_L = MPFC.LP_L_result,
                        MPFC.LP_R = MPFC.LP_R_result,
                        LP_L.LP_R = LP_L.LP_R_result,
                        MPFC.PCC = MPFC.PCC_result,
                        LP_L.PCC = LP_L.PCC_result,
                        LP_R.PCC = LP_R.PCC_result)

# Saving as .RData
save(fullmod_results_brand, file = "LMM_fullmodel_results_brand.RData")
##############################################################################



