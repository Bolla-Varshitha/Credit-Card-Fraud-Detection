# install_packages.R
required_packages <- c(
  "dplyr", "ggplot2", "caret", "randomForest", "xgboost",
  "pROC", "ROSE", "DMwR", "recipes", "corrplot", "patchwork",
  "readr", "tidyr", "reshape2", "glmnet"
)

# Install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("All required packages installed successfully!\n")
