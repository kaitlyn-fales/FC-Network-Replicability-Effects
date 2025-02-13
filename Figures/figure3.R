# Set wd
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Packages
library(tidyverse)
library(reshape2)

# Loading data - data after subtracting out random effect of id(site)
load("all_combos_dataframe_regressed.RData")

######### Plotting Pipeline:Atlas interaction ################################
# Getting fitted values
fitted_vals <- dat.final %>% 
  group_by(pipeline,atlas) %>% 
  summarise("MPFC & LP-L" = mean(MPFC.LP_L), "MPFC & LP-R" = mean(MPFC.LP_R),
            "LP-L & LP-R" = mean(LP_L.LP_R), "MPFC & PCC" = mean(MPFC.PCC),
            "LP-L & PCC" = mean(LP_L.PCC), "LP-R & PCC" = mean(LP_R.PCC))

fitted_vals$pipeline <- toupper(fitted_vals$pipeline)
fitted_vals$atlas <- toupper(fitted_vals$atlas)

# Melt data into long format
fitted_vals_melted <- melt(fitted_vals, id.vars = c("pipeline","atlas"))

# Creating plot
fitted_vals_melted %>% ggplot(aes(x = atlas, y = value, group = pipeline)) + 
  geom_line(aes(color = pipeline, linetype = pipeline), linewidth = 1.5) + 
  scale_colour_manual(values = c("#A4036F","#00916E","#FFCF00","#A79AB2")) +
  ggtitle("Edgewise Fitted Values by Preprocessing Pipeline and Brain Parcellation") + 
  xlab("Brain Parcellation") + ylab("Fitted Values") + 
  labs(color = "Pipeline", linetype = "Pipeline") +
  theme_classic() +
  facet_wrap(~variable) + theme(legend.position="bottom")

##############################################################################