##### The code below converts .RData file to .csv file #####

load("R code/Frobenius Norm/all_combos_dataframe_regressed.RData")

write.csv(dat.final, "R code/Frobenius Norm/all_combos_dataframe_regressed.csv")
