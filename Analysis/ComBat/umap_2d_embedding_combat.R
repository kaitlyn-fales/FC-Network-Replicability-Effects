# UMAP
library(umap)
library(scales)
library(ggplot2)
library(deldir)

# Directory (Kaitlyn's local machine)
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Load data
load("all_combos_dataframe_combat_regressed.RData")
df <- df_regressed

# Load umap
load("umap_post_regression_combat.RData")

# Separate into labels and data
edge.data <- df[,-c(1:5)]
pipeline <- df[,"pipeline"]
atlas <- df[,"atlas"]

# Create umap
custom.config <- umap.defaults
custom.config$random_state <- 1234 # seed for reproducibility
#edges.umap <- umap(edge.data, config = custom.config)
#edges.umap

# Export full umap for later use
#save(edges.umap, file = "umap_post_regression_combined.RData")




# ggplots
pipeline <- as.factor(toupper(pipeline))
levels(pipeline) <- c("CCS","CPAC","DPARSF","NIAK")
atlas <- as.factor(toupper(atlas))
levels(atlas) <- c("AAL",
                   "CC200",
                   "CC400",
                   "DOS160",
                   "EZ",
                   "HO",
                   "TT")
umap.dat <- data.frame(pipeline,atlas,edges.umap$layout)

# pipeline - not faceted 
umap.dat %>% ggplot() +
  geom_point(mapping = aes(x = X1, y = X2, colour = pipeline), alpha = 0.25) + 
  scale_colour_manual(values = c("#A4036F","#00916E","#FFCF00","#A79AB2")) +
  labs(title = "Pipeline", colour = "Pipeline") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          axis.text = element_text(size = 11),
                          legend.position = "inside",
                          legend.position.inside = c(0.09,0.8),
                          plot.title = element_text(size = 16),
                          legend.title = element_blank(),
                          legend.background = element_rect(fill = "transparent"),
                          legend.text = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = aes(size = 3, alpha = 1)))


# pipeline - faceted
umap.dat %>% ggplot(mapping = aes(x = X1, y = X2, colour = pipeline)) +
  geom_point(alpha = 0.15) + 
  scale_colour_manual(values = c("#A4036F","#00916E","#FFCF00","#A79AB2")) +
  labs(title = "Pipeline Faceted") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "none",
                          plot.title = element_text(size = 16)) +
  facet_wrap(~pipeline)



# atlas - not faceted 
umap.dat %>% ggplot() +
  geom_point(mapping = aes(x = X1, y = X2, colour = atlas), alpha = 0.4) + 
  scale_color_manual(values = c("#00E8FC","#FFA3AF","#78290F","#A23B72","#FF7D00","#15616D","#00134D")) +
  labs(title = "Atlas", colour = "Atlas") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "inside",
                          legend.position.inside = c(0.07,0.85),
                          plot.title = element_text(size = 16),
                          legend.title = element_blank(),
                          legend.background = element_rect(fill = "transparent"),
                          legend.text = element_text(size = 10),
                          axis.text = element_text(size = 11)) +
  guides(colour = guide_legend(override.aes = aes(size = 3, alpha = 1)))


# atlas - faceted
umap.dat %>% ggplot(mapping = aes(x = X1, y = X2, colour = atlas)) +
  geom_point(alpha = 0.15) + 
  scale_color_manual(values = c("#00E8FC","#FFA3AF","#78290F","#A23B72","#FF7D00","#15616D","#00134D")) +
  labs(title = "Atlas Faceted") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "none",
                          plot.title = element_text(size = 16)) +
  facet_wrap(~atlas)












