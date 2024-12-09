#### Visualization - Frobenius Norm - Atlas ####

# For each individual (ID):
  # Find all values associated with a pairwise atlas comparison pair (e.g. AAL vs CC200) (several values due to different filters or atlases).
  # Compute the mean and variance of these values for that individual.
# Across all individuals:
  # Compute the average of individuals means and the average of variances calculated above.
  # Explains how much variability exists for that comparison pair across individuals.


# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Read in data (from working directory)
df <- read.csv("R Code/Frobenius Norm/result_matrix.csv")


#### Calculate mean and variance *within each individual* for atlas pairwise comparisons ####
df_atlas_individual <- df %>%
  pivot_longer(cols = -ID, names_to = "comparison", values_to = "value") %>%
  # Separate the comparison into two combinations using separator "__"
  separate(comparison, into = c("Combination1", "Combination2"), sep = "__") %>%
  # Separate each combination into Pipeline, Filter, and Atlas components
  separate(Combination1, into = c("Pipeline1", "Filter1", "Atlas1"), sep = "\\.") %>%
  separate(Combination2, into = c("Pipeline2", "Filter2", "Atlas2"), sep = "\\.") %>%
  # Capitalize Pipeline1 and Pipeline2
  mutate(Atlas1 = toupper(Atlas1),
         Atlas2 = toupper(Atlas2)) %>%
  # Filter out self-comparisons
  filter(Atlas1 != Atlas2) %>%
  # Treat Atlas1 vs Atlas2 the same as Atlas2 vs Atlas1
  mutate(Pair = ifelse(Atlas1 < Atlas2,
                       paste(Atlas1, Atlas2, sep = " vs "),
                       paste(Atlas2, Atlas1, sep = " vs "))) %>%
  # Group by ID and Pair to calculate individual-level mean and variance
  group_by(ID, Pair) %>%
  summarize(individual_mean = mean(value, na.rm = TRUE),
            individual_variance = var(value, na.rm = TRUE),
            .groups = "drop")

# Brief view of df_atlas_individual
head(df_atlas_individual)


#### Calculate mean and variance *across individuals* for each atlas pair ####
df_atlas_summary <- df_atlas_individual %>%
  group_by(Pair) %>%
  summarize(mean_of_individual_means = mean(individual_mean, na.rm = TRUE),
            mean_of_individual_variances = mean(individual_variance, na.rm = TRUE),
            .groups = "drop")


#### Visualization for Overall Mean of Individual Means for Atlas Pairs ####
# Initialize an empty 6x6 matrix
atlas_vertical_names <- c("AAL", "CC200", "CC400", "EZ", "HO", "TT")
atlas_horizontal_names <- rev(atlas_vertical_names)  # Reversed order for the top (horizontal) axis

# Initialize a 6x6 matrix with NA values
mean_matrix_atlas <- matrix(NA, nrow = 6, ncol = 6, dimnames = list(atlas_vertical_names, atlas_horizontal_names))

# Fill the upper left half of the matrix with mean values from df_atlas_summary
for (row in 1:length(atlas_vertical_names)) {
  for (col in 1:length(atlas_horizontal_names)) {
    if (atlas_vertical_names[row] != atlas_horizontal_names[col]) {  # Skip self-comparisons
      pair <- paste(atlas_vertical_names[row], atlas_horizontal_names[col], sep = " vs ")  # Correct comparison pair
      mean_value <- df_atlas_summary %>% filter(Pair == pair) %>% pull(mean_of_individual_means)
      if (length(mean_value) > 0) {
        mean_matrix_atlas[row, col] <- mean_value
      }
    }
  }
}

# Convert the matrix to a long format suitable for ggplot, keeping only upper left triangle values
mean_data_atlas <- as.data.frame(as.table(mean_matrix_atlas)) %>%
  filter(!is.na(Freq))  # Filter to keep only the non-NA upper triangle values
colnames(mean_data_atlas) <- c("Atlas1", "Atlas2", "MeanValue")

# Plot the heatmap for overall mean of individual means with x-axis labels at the top and values inside tiles
plot1 <- ggplot(mean_data_atlas, aes(x = Atlas1, y = Atlas2, fill = MeanValue)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(aes(label = round(MeanValue, 2)), color = "black", size = 7) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey") +
  labs(
    x = "Atlas 1",
    y = "Atlas 2",
    fill = "Mean Value"
  ) +
  theme_grey() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_colorbar(barwidth = 2, barheight = 10)) +
  coord_fixed()

plot1


# Calculate the mean of individual variances for each atlas pair.
# This captures how much variance exists for each atlas comparison across individuals.

#### Visualization for Overall Variance (Mean of Individual Variances) for Atlas Pairs ####
# Initialize a 6x6 matrix with NA values for variances
variance_matrix_atlas <- matrix(NA, nrow = 6, ncol = 6, dimnames = list(atlas_vertical_names, atlas_horizontal_names))

# Fill the upper left half of the matrix with variance values from df_atlas_summary
for (row in 1:length(atlas_vertical_names)) {
  for (col in 1:length(atlas_horizontal_names)) {
    if (atlas_vertical_names[row] != atlas_horizontal_names[col]) {  # Skip self-comparisons
      pair <- paste(atlas_vertical_names[row], atlas_horizontal_names[col], sep = " vs ")  # Correct comparison pair
      variance_value <- df_atlas_summary %>% filter(Pair == pair) %>% pull(mean_of_individual_variances)
      if (length(variance_value) > 0) {
        variance_matrix_atlas[row, col] <- variance_value
      }
    }
  }
}

# Convert the variance matrix to a long format suitable for ggplot, keeping only upper left triangle values
variance_data_atlas <- as.data.frame(as.table(variance_matrix_atlas)) %>%
  filter(!is.na(Freq))  # Filter to keep only the non-NA upper triangle values
colnames(variance_data_atlas) <- c("Atlas1", "Atlas2", "VarianceValue")

# Plot the heatmap for mean of individual variances with x-axis labels at the top and values inside tiles
plot2 <- ggplot(variance_data_atlas, aes(x = Atlas1, y = Atlas2, fill = VarianceValue)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(aes(label = round(VarianceValue, 2)), color = "black", size = 7) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey") +
  labs(
    x = "Atlas 1",
    y = "Atlas 2",
    fill = "Variance Value"
  ) +
  theme_grey() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_colorbar(barwidth = 2, barheight = 10)) +
  coord_fixed()

plot2


#### Save final plots (if needed) ####
# Combine plot1 and plot2 side by side
combined_plot <- grid.arrange(
  arrangeGrob(
    textGrob("Mean", gp = gpar(fontsize = 16, fontface = "bold"), vjust = 12),
    plot1, 
    ncol = 1, 
    heights = c(1, 10)
  ),
  arrangeGrob(
    textGrob("Variance", gp = gpar(fontsize = 16, fontface = "bold"), vjust = 12),
    plot2, 
    ncol = 1, 
    heights = c(1, 10)
  ),
  ncol = 2,  # Arrange the plots in two columns
  top = textGrob(
    "Atlas Pairwise Comparison", 
    gp = gpar(fontsize = 20, fontface = "bold"),
    vjust = 10  # Adjust title vertical position (increase value to lower the title)
  )
)

# Save the combined plot
ggsave(
  filename = "Atlas_plot.jpeg", # Output file name
  plot = combined_plot,         # The plot object to save
  width = 12,                   # Width in inches
  height = 9,                   # Height in inches
  dpi = 300                     # Resolution in dots per inch
)

# To save individual plots
ggsave(
  filename = #"atlas_tile_mean.jpeg", # Output file name
  plot = #plot1,                      # The plot object to save
  width = 8,                          # Width in inches
  height = 6,                         # Height in inches
  dpi = 300                           # Resolution in dots per inch
)

