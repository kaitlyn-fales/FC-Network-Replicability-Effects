# To convert .RData to csv

load("R code/Frobenius Norm/all_combos_dataframe_regressed.RData")

write.csv(dat.final, "R code/Frobenius Norm/all_combos_dataframe_regressed.csv")