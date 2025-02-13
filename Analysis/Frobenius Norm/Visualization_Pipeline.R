#### Visualization - Frobenius Norm - Pipeline ####

# For each individual (ID): 
  # Find all values associated with a pairwise pipeline comparison pair (e.g. CCS vs CPAC) (several values due to different filters or atlases).
  # Compute the mean and variance of these values for that individual.
# Across all individuals:
  # Compute the average of the individuals means and the average of the variances calculated above.
  # Explains how much variability exists for that comparison pair across individuals.


# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Read in data (from working directory)
df <- read.csv("R code/Frobenius Norm/result_matrix.csv")


#### Calculate mean and variance *within each individual* for pipeline pairwise comparisons ####
df_pipeline_individual <- df %>%
  pivot_longer(cols = -ID, names_to = "comparison", values_to = "value") %>%
  # Separate the comparison into two combinations using separator "__"
  separate(comparison, into = c("Combination1", "Combination2"), sep = "__") %>%
  # Separate each combination into Pipeline, Filter, and Atlas components
  separate(Combination1, into = c("Pipeline1", "Filter1", "Atlas1"), sep = "\\.") %>%
  separate(Combination2, into = c("Pipeline2", "Filter2", "Atlas2"), sep = "\\.") %>%
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
  summarize(individual_mean = mean(value, na.rm = TRUE),
            individual_variance = var(value, na.rm = TRUE),
            .groups = "drop")

# Brief view of df_pipeline_individual
head(df_pipeline_individual)


#### Calculate mean and variance *across individuals* for each pipeline pair ####
df_pipeline_summary <- df_pipeline_individual %>%
  group_by(Pair) %>%
  summarize(mean_of_individual_means = mean(individual_mean, na.rm = TRUE),
            mean_of_individual_variances = mean(individual_variance, na.rm = TRUE),
            .groups = "drop")


#### Visualization for Overall Mean of Individual Means for Pipeline Pairs ####
# Initialize an empty 4x4 matrix
pipeline_vertical_names <- c("CCS", "CPAC", "DPARSF", "NIAK")
pipeline_horizontal_names <- rev(pipeline_vertical_names)  # Reversed order for the top (horizontal) axis

# Initialize a 4x4 matrix with NA values
mean_matrix_pipeline <- matrix(NA, nrow = 4, ncol = 4, dimnames = list(pipeline_vertical_names, pipeline_horizontal_names))

# Fill the upper left half of the matrix with mean values from df_pipeline_summary
for (row in 1:length(pipeline_vertical_names)) {
  for (col in 1:length(pipeline_horizontal_names)) {
    if (pipeline_vertical_names[row] != pipeline_horizontal_names[col]) {  # Skip self-comparisons
      pair <- paste(pipeline_vertical_names[row], pipeline_horizontal_names[col], sep = " vs ")  # Correct comparison pair
      mean_value <- df_pipeline_summary %>% filter(Pair == pair) %>% pull(mean_of_individual_means)
      if (length(mean_value) > 0) {
        mean_matrix_pipeline[row, col] <- mean_value
      }
    }
  }
}

# Convert the matrix to a long format suitable for ggplot, keeping only upper left triangle values
mean_data_pipeline <- as.data.frame(as.table(mean_matrix_pipeline)) %>%
  filter(!is.na(Freq))  # Filter to keep only the non-NA upper triangle values
colnames(mean_data_pipeline) <- c("Pipeline1", "Pipeline2", "MeanValue")

# Plot the heatmap for overall mean of individual means with x-axis labels at the top and values inside tiles
plot1 <- ggplot(mean_data_pipeline, aes(x = Pipeline1, y = Pipeline2, fill = MeanValue)) +
  geom_tile(color = "black", size = 1) +
  geom_text(aes(label = round(MeanValue, 2)), color = "black", size = 7) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey") +
  labs(
    x = "Pipeline 1",
    y = "Pipeline 2",
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


# Calculate the mean of individual variances for each pipeline pair.
# This captures how much variance exists for each pipeline comparison across individuals.

#### Visualization for Overall Variance (Mean of Individual Variances) for Pipeline Pairs ####
# Initialize a 4x4 matrix with NA values for variances
variance_matrix_pipeline <- matrix(NA, nrow = 4, ncol = 4, dimnames = list(pipeline_vertical_names, pipeline_horizontal_names))

# Fill the upper left half of the matrix with variance values from df_pipeline_summary
for (row in 1:length(pipeline_vertical_names)) {
  for (col in 1:length(pipeline_horizontal_names)) {
    if (pipeline_vertical_names[row] != pipeline_horizontal_names[col]) {  # Skip self-comparisons
      pair <- paste(pipeline_vertical_names[row], pipeline_horizontal_names[col], sep = " vs ")  # Correct comparison pair
      variance_value <- df_pipeline_summary %>% filter(Pair == pair) %>% pull(mean_of_individual_variances)
      if (length(variance_value) > 0) {
        variance_matrix_pipeline[row, col] <- variance_value
      }
    }
  }
}

# Convert the variance matrix to a long format suitable for ggplot, keeping only upper left triangle values
variance_data_pipeline <- as.data.frame(as.table(variance_matrix_pipeline)) %>%
  filter(!is.na(Freq))  # Filter to keep only the non-NA upper triangle values
colnames(variance_data_pipeline) <- c("Pipeline1", "Pipeline2", "VarianceValue")

# Plot the heatmap for mean of individual variances with x-axis labels at the top and values inside tiles
plot2 <- ggplot(variance_data_pipeline, aes(x = Pipeline1, y = Pipeline2, fill = VarianceValue)) +
  geom_tile(color = "black", size = 1) +
  geom_text(aes(label = round(VarianceValue, 2)), color = "black", size = 7) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey") +
  labs(
    x = "Pipeline 1",
    y = "Pipeline 2",
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
  plot1, plot2, 
  ncol = 2,  # Arrange the plots in two columns
  top = textGrob(
    "Pipeline Pairwise Comparison", 
    gp = gpar(fontsize = 20, fontface = "bold"),
    vjust = 10  # Adjust title vertical position (increase value to lower the title)
  )
)

# Save the combined plot
ggsave(
  filename = "Pipeline_plot.jpeg", # Output file name
  plot = combined_plot,            # The plot object to save
  width = 12,                      # Width in inches
  height = 9,                      # Height in inches
  dpi = 300                        # Resolution in dots per inch
)

# To save individual plots
ggsave(
  filename = "pipeline_tile_#mean.jpeg", # Output file name
  plot = #plot1,                         # The plot object to save
  width = 8,                             # Width in inches
  height = 6,                            # Height in inches
  dpi = 300                              # Resolution in dots per inch
)

