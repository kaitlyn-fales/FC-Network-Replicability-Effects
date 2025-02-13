# UMAP
library(umap)
library(scales)
library(RColorBrewer)
library(tidyverse)

# Directory (Kaitlyn's local machine)
setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

# Custom plotting function
plot.umap <- function(x, labels,
                      main="A UMAP visualization (ComBat)",
                      colors=c(1:4),
                      pad=0.1, cex=0.6, pch=19, add=FALSE, legend.suffix="",
                      cex.main=1, cex.legend=0.85) {
  
  layout <- x
  if (is(x, "umap")) {
    layout <- x$layout
  } 
  
  xylim <- range(layout)
  xylim <- xylim + ((xylim[2]-xylim[1])*pad)*c(-0.5, 0.5)
  if (!add) {
    par(mar=c(0.2,0.7,1.2,0.7), ps=10)
    plot(xylim, xylim, type="n", axes=F, frame=F)
    rect(xylim[1], xylim[1], xylim[2], xylim[2], border="#aaaaaa", lwd=0.25)  
  }
  points(layout[,1], layout[,2], col=colors[as.integer(labels)],
         cex=cex, pch=pch)
  mtext(side=3, main, cex=cex.main)
  
  labels.u <- unique(labels)
  legend.pos <- "topleft"
  legend.text <- as.character(labels.u)
  if (add) {
    legend.pos <- "bottomleft"
    legend.text <- paste(as.character(labels.u), legend.suffix)
  }
  
  legend(legend.pos, legend=legend.text, inset=0.03,
         col=colors[as.integer(labels.u)],
         bty="n", pch=pch, cex=cex.legend)
}

# Load data
load("all_combos_dataframe_combat.RData")
df <- dat.combat

# Load umap
load("umap_post_combat.RData")

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
#save(edges.umap, file = "umap_post_combat.RData")

# Making plots
pipeline_col <- brewer.pal(4, "Dark2")
plot.umap(edges.umap, pipeline, colors = alpha(pipeline_col, 0.4), pch = 1)
pipeline_kmeans <- kmeans(edges.umap$layout, centers = 4, nstart = 25)
points(pipeline_kmeans$centers, pch = 19, cex = 2)


plot.umap(edges.umap, filter, colors = alpha(c(2,4), 0.5), pch = 1)
filter_kmeans <- kmeans(edges.umap$layout, centers = 2, nstart = 25)
points(filter_kmeans$centers, pch = 19, cex = 2)

atlas_col <- brewer.pal(6, "Dark2")
plot.umap(edges.umap, atlas, colors = alpha(atlas_col, 0.4), pch = 1)
atlas_kmeans <- kmeans(edges.umap$layout, centers = 6, nstart = 25)
points(atlas_kmeans$centers, pch = 19, cex = 2)

plot.umap(edges.umap, site, colors = alpha(c(1:20), 0.4), pch = 1)

# Faceted ggplots
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

# pipeline
umap.dat %>% ggplot(mapping = aes(x = X1, y = X2, colour = pipeline)) +
  geom_point(alpha = 0.15) + 
  scale_colour_manual(values = c("#A4036F","#00916E","#FFCF00","#A79AB2")) +
  labs(title = "Pipeline Faceted after ComBat") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "none",
                          plot.title = element_text(size = 16)) + 
  ylim(-15,8) + xlim(-8,6) +
  facet_wrap(~pipeline)

# filter
filt_names <- c(
  filt = "Filtering",
  nofilt = "No Filtering"
)

umap.dat %>% ggplot(mapping = aes(x = X1, y = X2, colour = filter)) +
  geom_point(alpha = 0.15) +
  labs(title = "UMAP Projection by High Band-Pass Filtering (ComBat)") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "none") +
  ylim(-15,8) + xlim(-8,6) +
  facet_wrap(~filter, labeller = as_labeller(filt_names))

# atlas
umap.dat %>% ggplot(mapping = aes(x = X1, y = X2, colour = atlas)) +
  geom_point(alpha = 0.15) + 
  scale_color_manual(values = c("#00E8FC","#FFA3AF","#78290F","#A23B72","#FF7D00","#15616D")) +
  labs(title = "Atlas Faceted after ComBat") + 
  theme_classic() + theme(axis.title.x = element_blank(), 
                          axis.title.y = element_blank(),
                          legend.position = "none",
                          plot.title = element_text(size = 16)) +
  ylim(-15,8) + xlim(-8,6) +
  facet_wrap(~atlas) 





# Color-blind friendly
check_cb <- function(palette, return_cb_palettes = FALSE, ...) {
  
  # make an empty list
  cb_palettes <- setNames(vector("list", length = 3), 
                          nm = c("protan", "deutan", "tritan"))
  
  # generate colorblindness approximations
  for (i in 1:length(cb_palettes)) {
    cb_palettes[[i]] <- dichromat::dichromat(palette, names(cb_palettes)[i])
  }
  
  # reset graphical parameters when function exits:
  current_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(current_par))
  
  # plot for comparison
  layout(matrix(1:4, nrow = 4)); par(mar = rep(1, 4))
  recolorize::plotColorPalette(palette, main = "Trichromacy", ...)
  pnames <- c("Protanopia", "Deutanopia", "Tritanopia")
  for (i in 1:3) {
    recolorize::plotColorPalette(cb_palettes[[i]], main = pnames[i], ...)
  }
  
  if (return_cb_palettes) {
    return(cb_palettes)
  }
}
ggplot_colors <- scales::hue_pal()(2)
check_cb(RColorBrewer::brewer.pal(6, "Dark2"))
check_cb(ggplot_colors)








