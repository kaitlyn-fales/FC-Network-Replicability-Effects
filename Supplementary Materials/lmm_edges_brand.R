######### Linear mixed effect modeling of preprocessing pipelines ############

# Install packages
#install.packages('lme4')
#install.packages('lmerTest')
#install.packages('tidyverse')
#install.packages('readxl')

# Packages
library(lme4)
library(lmerTest)
library(tidyverse)
library(readxl)

# Set directories as needed
#setwd("~/FC-Network-Replicability-Effects")

# Load preprocessed data
load("Data/processed_data.RData")

######### Including scanner brand ############################################

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



