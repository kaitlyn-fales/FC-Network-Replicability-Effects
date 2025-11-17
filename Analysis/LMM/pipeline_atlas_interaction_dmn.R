# Set wd
#setwd("~/FC-Network-Replicability-Effects")

# Packages
library(tidyverse)
library(reshape2)

# Loading data - data after subtracting out random effect of id(site)
load("Data/processed_data_network_regressed.RData")

######### Plotting Pipeline:Atlas interaction ################################
# Getting fitted values
fitted_vals <- df_regressed %>% 
  select(pipeline,atlas,starts_with("DefaultMode")) %>% 
  group_by(pipeline,atlas) %>% 
  summarise("MPFC & LP-L" = mean(DefaultMode_1), "MPFC & LP-R" = mean(DefaultMode_2),
            "LP-L & LP-R" = mean(DefaultMode_3), "MPFC & PCC" = mean(DefaultMode_4),
            "LP-L & PCC" = mean(DefaultMode_5), "LP-R & PCC" = mean(DefaultMode_6))

fitted_vals$pipeline <- toupper(fitted_vals$pipeline)
fitted_vals$atlas <- toupper(fitted_vals$atlas)

# Melt data into long format
fitted_vals_melted <- melt(fitted_vals, id.vars = c("pipeline","atlas"))

# Creating plot
fitted_vals_melted %>% ggplot(aes(x = atlas, y = value, group = pipeline)) + 
  geom_line(aes(color = pipeline, linetype = pipeline), linewidth = 1.5) + 
  scale_colour_manual(values = c("#A4036F","#00916E","#FFCF00","#A79AB2")) +
  ggtitle("Edgewise Fitted Values by Preprocessing Pipeline and Brain Parcellation in the DMN") + 
  xlab("Brain Parcellation") + ylab("Fitted Values") + 
  labs(color = "Pipeline", linetype = "Pipeline") +
  theme_classic() +
  facet_wrap(~variable) + theme(legend.position="bottom")

##############################################################################