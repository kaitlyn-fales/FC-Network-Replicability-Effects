######### Linear mixed effect modeling of preprocessing pipelines ############

# Install packages
#install.packages('lme4')
#install.packages('lmerTest')
#install.packages('tidyverse')

# Packages
library(lme4)
library(lmerTest)
library(tidyverse)

# Set directories as needed
#setwd("~/FC-Network-Replicability-Effects")

# Load preprocessed data
load("Data/processed_data.RData")

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
MPFC.LP_R <- lmer(MPFC.LP_R ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

# Model summary
summary(MPFC.LP_R)

# ANOVA table
MPFC.LP_R_result <- list(anova = anova(MPFC.LP_R, type = 3),
                         ranova = ranova(MPFC.LP_R))

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
LP_L.LP_R <- lmer(LP_L.LP_R ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

# Model summary
summary(LP_L.LP_R)

# ANOVA table
LP_L.LP_R_result <- list(anova = anova(LP_L.LP_R, type = 3),
                         ranova = ranova(LP_L.LP_R))

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
MPFC.PCC <- lmer(MPFC.PCC ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# Model summary
summary(MPFC.PCC)

# ANOVA table
MPFC.PCC_result <- list(anova = anova(MPFC.PCC, type = 3),
                        ranova = ranova(MPFC.PCC))

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
LP_L.PCC <- lmer(LP_L.PCC ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# Model summary
summary(LP_L.PCC)

# ANOVA table
LP_L.PCC_result <- list(anova = anova(LP_L.PCC, type = 3),
                        ranova = ranova(LP_L.PCC))

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
LP_R.PCC <- lmer(LP_R.PCC ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# Model summary
summary(LP_R.PCC)

# ANOVA table
LP_R.PCC_result <- list(anova = anova(LP_R.PCC, type = 3),
                        ranova = ranova(LP_R.PCC))

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




