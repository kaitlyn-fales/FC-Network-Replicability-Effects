# Use of Combat to harmonize batch effect of site

# Working directory
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Package
library(neuroCombat)

# Load dataframe
load("all_combos_dataframe.RData")

# Prepare data in format needed for Combat
dat <- df[, grep("MPFC|LP_R|PCC", colnames(df))] # separate FC vectors
dat <- t(dat) # transpose so that matrix is pxn
batch <- df[,"site"] # batch variable for site id

pipeline <- df[,"pipeline"] # variables we want to preserve
filter <- df[,"filter"]
atlas <- df[,"atlas"]

mod <- model.matrix(~pipeline+filter+atlas) # put in model matrix

# Use full Combat to harmonize the data across site
data.harmonized <- neuroCombat(dat=dat, batch=batch, mod = mod)

# Put original data format back together for export
dat.combat <- data.frame(df[,1:5],t(data.harmonized$dat.combat))

# Export
save(dat.combat, file = "all_combos_dataframe_combat.RData")


















