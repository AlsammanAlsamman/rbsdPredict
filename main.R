# Main script to build, install, and use the rbsdPredict package

# Load devtools for package development
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
library(devtools)

# Build the package
devtools::document()
devtools::build()

# Install the package (from local directory)
devtools::install_local(".")

# Load the package
library(rbsdPredict)

# Example usage (replace with actual function calls as implemented)
# result <- rbsdPredict::your_function_name(args)
# print(result)