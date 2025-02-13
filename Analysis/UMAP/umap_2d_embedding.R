# UMAP
library(umap)
library(scales)
library(ggplot2)
library(deldir)

# Directory (Kaitlyn's local machine)
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Load data
load("all_combos_dataframe_regressed.RData")
df <- dat.final

# Load umap
load("umap_post_regression.RData")

# Separate into labels and data
edge.data <- df[, grep("MPFC|LP_R|PCC", colnames(df))]
pipeline <- df[,"pipeline"]
filter <- df[,"filter"]
atlas <- df[,"atlas"]
site <- df[,"site"]

# Create umap
custom.config <- umap.defaults
custom.config$random_state <- 1234 # seed for reproducibility
#edges.umap <- umap(edge.data, config = custom.config)
#edges.umap

# Export full umap for later use
#save(edges.umap, file = "umap_post_regression.RData")

# Run k-means algorithm with k chosen to match the levels of each factor
pipeline_kmeans <- kmeans(edges.umap$layout, centers = 4, nstart = 25)
filter_kmeans <- kmeans(edges.umap$layout, centers = 2, nstart = 25)
atlas_kmeans <- kmeans(edges.umap$layout, centers = 6, nstart = 25)

# Create Voronoi tesselation from K-means centers for pipeline and atlas
pipeline_tesselation <- deldir(pipeline_kmeans$centers[,1], pipeline_kmeans$centers[,2])
pipeline_tiles <- tile.list(pipeline_tesselation)



# ggplots
pipeline <- as.factor(toupper(pipeline))
levels(pipeline) <- c("CCS","CPAC","DPARSF","NIAK")
atlas <- as.factor(toupper(atlas))
levels(atlas) <- c("AAL",
                   "CC200",
                   "CC400",
                   "EZ",
                   "HO",
                   "TT")
umap.dat <- data.frame(pipeline,filter,atlas,site,edges.umap$layout)

# pipeline - not faceted + k-means
umap.dat %>% ggplot() +
  geom_point(mapping = aes(x = X1, y = X2, colour = pipeline), alpha = 0.25) + 
  scale_colour_manual(values = c("#A4036F","#00916E","#FFCF00","#A79AB2")) +
  geom_point(data = data.frame(pipeline_kmeans$centers), 
             mapping = aes(x = X1, y = X2), size = 6, shape = 18) +
  labs(title = "Pipeline with K-means Centers", colour = "Pipeline") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          axis.text = element_text(size = 11),
                          legend.position = "inside",
                          legend.position.inside = c(0.09,0.84),
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


# filter
filt_names <- c(
  filt = "Filtering",
  nofilt = "No Filtering"
)

umap.dat %>% ggplot(mapping = aes(x = X1, y = X2, colour = filter)) +
  geom_point(alpha = 0.15) +
  labs(title = "Band-Pass Filtering Faceted") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "none",
                          plot.title = element_text(size = 16)) +
  facet_wrap(~filter, labeller = as_labeller(filt_names))

# atlas - not faceted + k-means
umap.dat %>% ggplot() +
  geom_point(mapping = aes(x = X1, y = X2, colour = atlas), alpha = 0.4) + 
  scale_color_manual(values = c("#00E8FC","#FFA3AF","#78290F","#A23B72","#FF7D00","#15616D")) +
  geom_point(data = data.frame(atlas_kmeans$centers), 
             mapping = aes(x = X1, y = X2), size = 6, shape = 18) +
  labs(title = "Atlas with K-means Centers", colour = "Atlas") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "inside",
                          legend.position.inside = c(0.07,0.82),
                          plot.title = element_text(size = 16),
                          legend.title = element_blank(),
                          legend.background = element_rect(fill = "transparent"),
                          legend.text = element_text(size = 10),
                          axis.text = element_text(size = 11)) +
  guides(colour = guide_legend(override.aes = aes(size = 3, alpha = 1)))


# atlas - faceted
umap.dat %>% ggplot(mapping = aes(x = X1, y = X2, colour = atlas)) +
  geom_point(alpha = 0.15) + 
  scale_color_manual(values = c("#00E8FC","#FFA3AF","#78290F","#A23B72","#FF7D00","#15616D")) +
  labs(title = "Atlas Faceted") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "none",
                          plot.title = element_text(size = 16)) +
  facet_wrap(~atlas)




 

###################################################



# Calculate Voronoi Tesselation and tiles
tesselation <- deldir(pipeline_kmeans$centers[,1], pipeline_kmeans$centers[,2])
tiles <- tile.list(tesselation)

plot(umap.dat$X1, umap.dat$X2,pch=19,xlab="",ylab="",
     col=alpha(factor(pipeline_kmeans$cluster), 0.03))
plot(tiles, pch = 19, add=T)

# Atlas
tesselation <- deldir(atlas_kmeans$centers[,1], atlas_kmeans$centers[,2])
tiles <- tile.list(tesselation)

plot(umap.dat$X1, umap.dat$X2,pch=19,xlab="",ylab="",
     col=alpha(factor(atlas_kmeans$cluster), 0.03))
plot(tiles, pch = 19, add=T)








