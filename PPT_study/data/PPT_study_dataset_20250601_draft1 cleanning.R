install.packages("readxl")
install.packages("naniar")
install.packages("ggplot2") 

library(readxl)
library(naniar)
library(ggplot2)  

data <- read_excel(file.choose())

data <- as.data.frame(data)

data[] <- lapply(data, function(col) {
  if (is.factor(col)) {
    col <- as.character(col)
  }
  return(col)
})

data[] <- lapply(data, function(col) {
  if (is.character(col)) {
    col <- tolower(col)          
    col <- gsub("-", "_", col)  
  }
  return(col)
})

data <- data[, colSums(is.na(data)) < nrow(data)]

data[] <- lapply(data, function(col) {
  if (is.character(col)) {
    col[col == ""] <- NA
  }
  return(col)
})

data[] <- lapply(data, function(col) {
  if (is.character(col)) {
    col <- trimws(col)
    col <- tolower(col)
  }
  return(col)
})

multi_select_cols <- grep("where_healthsur_|receive_via_|motivate_healthsur_|stop_healthsur_", names(data), value = TRUE)

data[multi_select_cols] <- lapply(data[multi_select_cols], function(col) {
  as.numeric(col)
})

# Calculate the missing percentage for each column
miss_df <- miss_var_summary(data)

# Select the top 20 variables with the highest missing percentage
top_miss <- head(miss_df[order(-miss_df$pct_miss), ], 20)

# Plot (sorted by missing percentage)
ggplot(top_miss, aes(x = reorder(variable, pct_miss), y = pct_miss)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Variables with Highest Missing Percentage",
       x = "Variable",
       y = "Missing Percentage") +
  theme_minimal()

# Extract columns starting with motivate_healthsur_
pca_vars <- grep("^motivate_healthsur_", names(data), value = TRUE)
pca_data_raw <- data[pca_vars]

# Remove columns that are entirely NA or have constant values
pca_data <- pca_data_raw[, sapply(pca_data_raw, function(x) {
  !all(is.na(x)) && length(unique(x[!is.na(x)])) > 1
})]

# Check how many columns remain
ncol(pca_data)

# Remove rows with NA (PCA does not accept NA)
pca_data <- na.omit(pca_data)

# Perform Principal Component Analysis
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Plot the Scree Plot of PCA
plot(pca_result, type = "l", main = "Scree Plot of PCA")
summary(pca_result)

# Check the first few rows of the result (optional)
head(data, 5)

