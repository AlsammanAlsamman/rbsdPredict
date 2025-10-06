# Data processing functions for RBSD project
library(dplyr)
library(reshape2)

# Load and clean data
data_load <- function(filepath) {
  data <- read.table(filepath, header=TRUE, sep="\t", row.names=1)
  return(data)
}

# Check for missing values
missing_summary <- function(data) {
  list(total_na = sum(is.na(data)), col_na = colSums(is.na(data)))
}

# Remove outliers or unclustered data
remove_outliers <- function(data, rows) {
  data <- data[-rows, ]
  return(data)
}
