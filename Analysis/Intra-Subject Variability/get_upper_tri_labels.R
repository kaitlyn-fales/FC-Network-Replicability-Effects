# Packages
library(dplyr)

# Working directory
#setwd("~/FC-Network-Replicability-Effects")

# Load hierarchical clustering assignments
load("Analysis/Exploratory Analysis/hclust_cuts.RData")

# Reduce to 28 cluster options since filtering does not have pronounced effect
clust <- cut_avg[!grepl("nofilt",cut_avg$combination),]

# Base directory for correlation matrices
base_dir <- paste0(getwd(),"/Data/Correlation_Matrices")

# Full paths to the files
file_list <- list.files(path = base_dir, pattern = "\\.RData$", full.names = TRUE, ignore.case = TRUE)
file_list <- file_list[!grepl("nofilt",file_list)]

# Extract list of names of all 28 preprocessing combinations
combos <- clust$combination

# Extract the within network edge labels to use as reference later
# Pull the name of the file into generic form
load(file_list[1])
temp <- get(combos[1])
cor_matrix <- temp[[1]]

# Remove everything except cor_matrix
rm(list=setdiff(ls(), "cor_matrix"))

# Get all combinations of row and column names
row_col_names <- expand.grid(rownames(cor_matrix), colnames(cor_matrix))

# Identify the upper triangle indices (excluding the diagonal)
upper_tri_indices <- upper.tri(cor_matrix, diag = FALSE)

# Filter the row_col_names to keep only the upper triangle pairs
upper_tri_labs <- row_col_names[as.vector(upper_tri_indices), ]

# Create temporary ordering for upper_tri_labs to sort by later
upper_tri_labs$Base1 <- sub("\\..*$", "", upper_tri_labs$Var1)
upper_tri_labs$Base2 <- sub("\\..*$", "", upper_tri_labs$Var2)

upper_tri_labs$key <- ifelse(upper_tri_labs$Base1 == upper_tri_labs$Base2,
                             upper_tri_labs$Base1, 
                             paste(upper_tri_labs$Base2,upper_tri_labs$Base1, sep = "_"))

# Preserve original order
upper_tri_labs$original_order <- c(1:nrow(upper_tri_labs))

# Add edge identifier
upper_tri_labs <- upper_tri_labs %>%
  group_by(key) %>%
  mutate(id = row_number())

upper_tri_labs$key <- paste(upper_tri_labs$key,upper_tri_labs$id,sep = "_")

# Select necessary variables
upper_tri_labs <- upper_tri_labs %>% select(key,original_order,Var1,Var2)

# Save 
save(upper_tri_labs, file = "Analysis/Intra-Subject Variability/upper_triangle_labels.RData")

