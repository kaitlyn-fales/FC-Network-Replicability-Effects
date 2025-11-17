# Portrait divergence comparison - bootstrap sample dist vs real data ####################

# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(ggpubr)

setwd("C:/Users/kaitl/OneDrive - The Pennsylvania State University/Scanner Heterogeneity Project")

#### Calculate mean and variance *within each individual* for pipeline pairwise comparisons ####
get_pdiv_ind_summary <- function(df){
  output <- df %>%
    pivot_longer(cols = -ID, names_to = "comparison", values_to = "value") %>%
    # Separate the comparison into two combinations using separator "__"
    separate(comparison, into = c("Combination1", "Combination2"), sep = "__") %>%
    # Separate each combination into Pipeline and Atlas components
    separate(Combination1, into = c("Pipeline1", "Atlas1"), sep = "\\.") %>%
    separate(Combination2, into = c("Pipeline2", "Atlas2"), sep = "\\.") %>%
    # Capitalize Pipeline1 and Pipeline2
    mutate(Pipeline1 = toupper(Pipeline1),
           Pipeline2 = toupper(Pipeline2)) %>%
    # Filter out self-comparisons
    filter(Pipeline1 != Pipeline2) %>%
    # Treat Pipeline1 vs Pipeline2 the same as Pipeline2 vs Pipeline1
    mutate(Pair = ifelse(Pipeline1 < Pipeline2,
                         paste(Pipeline1, Pipeline2, sep = " vs "),
                         paste(Pipeline2, Pipeline1, sep = " vs "))) %>%
    # Group by ID and Pair to calculate individual-level mean and variance
    group_by(ID, Pair) %>%
    summarize(mean = mean(value, na.rm = TRUE), .groups = "drop")
  
  return(output)
}

# Read in regular data result 
dat_result <- read.csv("Results/pdiv_result.csv")

# Get individual level summaries for pipeline comparison
dat_summary <- get_pdiv_ind_summary(dat_result)
colnames(dat_summary) <- c(colnames(dat_summary)[1:2],"mean_dat")

# Initialize df to store results
boot_summary <- data.frame(dat_summary[,1:2])

# Run loop through bootstrap samples
for (b in 1:100){
  
  # Read in bootstrap data from one sample 
  boot_result <- read.csv(paste0("Results/Bootstrap Results/pdiv_bootstrap",b,".csv"))
  
  # Compute summary for pipeline
  temp_summary <- get_pdiv_ind_summary(boot_result)
  colnames(temp_summary) <- c(colnames(temp_summary)[1:2],paste0("mean_boot",b))
  
  # Merge with other df
  boot_summary <- merge(boot_summary, temp_summary, by = c("ID","Pair"))
  
}

# Unique pairs to filter by for creating plots
comparisons <- unique(dat_summary$Pair)

# Create matrix to store mean density curves for baseline
baseline_curves <- matrix(data = NA, nrow = 500, ncol = length(comparisons))
colnames(baseline_curves) <- comparisons

# Create a common grid to interpolate over
x <- seq(0, 1, length.out = 500)

# Plot densities of bootstrap samples and find "average" density
par(mfrow = c(3,2))

for (i in 1:length(comparisons)){
  temp <- boot_summary %>% filter(Pair == comparisons[i])
  
  # Empty list for densities for comparison i
  densities <- list()
  
  # Calculate densities for comparison i and store in list
  for (b in 1:100){
    densities[[b]] <- density(temp[,paste0("mean_boot",b)])
  }
  
  # Use densities to interpolate and average to find the mean density curve
  boot_avg <- rowMeans(sapply(densities, function(d) approx(d$x, d$y, x, yleft=0, yright=0)$y))
  
  plot(densities[[1]], 
       xlim=c(min(boot_summary[,3:102]),max(boot_summary[,3:102])), 
       ylim = c(0,6), main = paste0("Bootstrap Densities: ",comparisons[i]))
  for (j in 1:100){
    lines(densities[[j]])
  }
  lines(x, boot_avg, col = "red", lwd = 2)
  baseline_curves[,i] <- boot_avg
}

# Add in x values
baseline_curves <- as.data.frame(cbind(x,baseline_curves))

# Plotting mean curves on top of one another as sanity check
# if proper baseline, then curves should be nearly identical - yes
par(mfrow = c(1,1))
plot(baseline_curves$x, baseline_curves[,2], type = "l")
for (k in 3:7){
  lines(baseline_curves$x, baseline_curves[,k], type = "l", col = k)
}

# Save baseline curves
save(baseline_curves, file = "Results/baseline_curves_pdiv_pipeline.RData")

# Load baseline curves
load("Results/baseline_curves_pdiv_pipeline.RData")

# Plot baseline densities along with data comparison
for (i in 1:length(comparisons)){
  plot <- dat_summary %>% filter(Pair == comparisons[i]) %>% 
    ggplot() + geom_area(data = baseline_curves, 
                         aes(x = .data[[colnames(baseline_curves)[1]]], y = .data[[colnames(baseline_curves)[i+1]]]), 
                         fill = "lightblue", alpha = 0.5) +
    geom_line(data = baseline_curves, 
              aes(x = .data[[colnames(baseline_curves)[1]]], y = .data[[colnames(baseline_curves)[i+1]]],
                  color = "Baseline"), linewidth = 1, linetype = 2) +
    geom_density(aes(x = mean_dat, color="Observed Difference"), 
                 fill = "pink", alpha = 0.5, linewidth = 1) + 
    labs(title = paste0(comparisons[i]),
         x = "Mean Individual Portrait Divergence",
         y = "Density") + xlim(0,1) + ylim(0,8) +
    scale_color_manual(name = "Source", values = c("blue", "red")) +
    theme_classic() +
    guides(color = guide_legend(override.aes = list(linetype = c(0,1), shape = 22, fill = c("lightblue", "pink"), 
                                                    size = 5))) +
    theme(legend.position = c(0.85,0.8), legend.key = element_rect(fill = c("lightblue","pink")))
  
  assign(paste0("p",i),plot)
}

# Grid arrange plots
ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3,
          common.legend = T, legend = "bottom")

