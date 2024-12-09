# Read in package
library(dplyr)

# Load data
df <- read.csv("~/R code/Frobenius Norm/all_combos_dataframe_regressed.csv")

# Apply the inverse Fisher's z-transform to relevant columns
correlation_columns <- names(df)[7:12] # Columns MPFC.LP_L to LP_R.PCC
df[correlation_columns] <- tanh(as.matrix(df[correlation_columns]))

# Get unique subject IDs
subjects <- unique(df$ID)

# Create a data frame to store the results
results <- data.frame(ID = subjects)

# Get unique combinations of pipeline, filter, and atlas
combinations <- unique(df[, c("pipeline", "filter", "atlas")])

# Generate unique pairwise combinations of the 48 combinations
combo_indices <- combn(seq_len(nrow(combinations)), 2)

combo_names <- apply(combo_indices, 2, function(idx) {
  combo1 <- combinations[idx[1], ]
  combo2 <- combinations[idx[2], ]
  paste(paste(combo1$pipeline, combo1$filter, combo1$atlas, sep = "."),
        paste(combo2$pipeline, combo2$filter, combo2$atlas, sep = "."),
        sep = "__")
})

# Initialize the results matrix with NA
results_matrix <- matrix(NA, nrow = length(subjects), ncol = length(combo_names))
colnames(results_matrix) <- combo_names

# Add ID column to the results matrix
results_matrix <- cbind(ID = subjects, results_matrix)




# Loop through Each Subject and Calculate Pairwise Frobenius Norms (takes about 12 minutes)
# Calculate the Frobenius norm for each subject
for (i in seq_along(subjects)) {
  subject <- subjects[i]
  subject_data <- subset(df, ID == subject)
  
  for (j in seq_len(ncol(results_matrix) - 1)) { # Skip the ID column
    combo_pair <- strsplit(colnames(results_matrix)[j + 1], "__")[[1]]
    idx1 <- unlist(strsplit(combo_pair[1], "\\."))
    idx2 <- unlist(strsplit(combo_pair[2], "\\."))
    
    data1 <- subject_data %>%
      filter(pipeline == idx1[1] & filter == idx1[2] & atlas == idx1[3]) %>%
      select(MPFC.LP_L:LP_R.PCC)
    
    data2 <- subject_data %>%
      filter(pipeline == idx2[1] & filter == idx2[2] & atlas == idx2[3]) %>%
      select(MPFC.LP_L:LP_R.PCC)
    
    if (nrow(data1) == 1 && nrow(data2) == 1) {
      A <- as.matrix(data1)
      B <- as.matrix(data2)
      results_matrix[i, j + 1] <- norm(A - B, type = "F")
    }
  }
}

# Convert the results_matrix to a data frame
results_df <- as.data.frame(results_matrix)

# Save the results to a CSV file
write.csv(results_df, "~/R code/Frobenius Norm/result_matrix.csv", row.names = FALSE)

# View the first few rows of the results
head(results_df)

