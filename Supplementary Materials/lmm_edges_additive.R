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