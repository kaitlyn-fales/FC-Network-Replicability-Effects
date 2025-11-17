# Use of Combat to harmonize batch effect of site

# Working directory
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Package
library(neuroCombat)

# Load in regressed df
load("all_combos_dataframe_network.RData")
within_network <- df

load("all_combos_dataframe_between_network.RData")
between_network <- df

# Merge df
df <- merge(within_network, between_network, by = c("pipeline","filter","atlas","site","ID"))

# Prepare data in format needed for Combat
dat <- df[,-c(1:5)] # separate FC vectors
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


















