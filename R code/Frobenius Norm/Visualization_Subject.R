#### Visualization - Frobenius Norm - For Each Subject ####



# Read in data
df <- read.csv("R Code/Frobenius Norm/result_matrix.csv")

# Load packages
library(dplyr)
library(ggplot2)



# Calculate variance of Frobenius norms for each subject and keep only the ID and variance columns
df_within_subject_variation <- df %>%
  rowwise() %>%
  mutate(subject_variance = var(c_across(-ID), na.rm = TRUE)) %>%
  ungroup() %>%
  select(ID, subject_variance)  # Keep only the ID and variance columns

# Display the resulting data frame
print(df_within_subject_variation)

# Summarize the within-subject variance across all subjects
summary_stats <- df_within_subject_variation %>%
  summarize(mean_variance = mean(subject_variance, na.rm = TRUE),
            median_variance = median(subject_variance, na.rm = TRUE),
            sd_variance = sd(subject_variance, na.rm = TRUE),
            var_variance = var(subject_variance, na.rm = TRUE))

# Display the summary statistics
print(summary_stats)

# Histogram of subject variances (ID: 50710 potential outlier with var over 0.3)
ggplot(df_within_subject_variation, aes(x = subject_variance)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  labs(title = "Distribution of Within-Subject Variance",
       x = "Subject Variance",
       y = "Frequency") +
  theme_grey()



# Calculate the total number of IDs
total_ids <- nrow(df_within_subject_variation)

# Calculate the number of IDs with variance less than 0.15
num_below_015 <- df_within_subject_variation %>%
  filter(subject_variance < 0.15) %>%
  nrow()

# Calculate the percentage of IDs with variance less than 0.15
percentage_below_015 <- (num_below_015 / total_ids) * 100

# Display the percentage of IDs with variance less than 0.15
print(paste("Percentage of IDs with variance less than 0.15 is about:", round(percentage_below_015, 2), "%"))

