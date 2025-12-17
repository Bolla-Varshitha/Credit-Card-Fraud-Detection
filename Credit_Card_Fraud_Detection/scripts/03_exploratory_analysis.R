#' Exploratory Data Analysis Script - Robust Version

library(ggplot2)
library(corrplot)
library(patchwork)
library(reshape2)
library(dplyr)
library(readr)

source("config/config.R")

perform_eda <- function(data, target_column = "Class") {
  cat("Performing exploratory data analysis...\n")
  cat("Target column:", target_column, "\n")
  
  # Check if target column exists
  if (!target_column %in% names(data)) {
    cat("⚠ WARNING: Target column", target_column, "not found in data.\n")
    cat("Available columns:", names(data), "\n")
    return(NULL)
  }
  
  # 1. Class distribution
  class_plot <- ggplot(data, aes(x = .data[[target_column]], fill = .data[[target_column]])) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
    labs(title = paste("Class Distribution -", target_column), 
         x = "Class", 
         y = "Count") +
    theme_minimal()
  
  print(class_plot)
  
  # Save plot
  if (!dir.exists(REPORTS_DIR)) {
    dir.create(REPORTS_DIR, recursive = TRUE)
  }
  
  png(file.path(REPORTS_DIR, "class_distribution.png"), width = 800, height = 600)
  print(class_plot)
  dev.off()
  
  # 2. Numeric features distribution
  numeric_columns <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_columns) > 0) {
    # Plot first 4 numeric features
    numeric_plots <- list()
    for (i in 1:min(4, length(numeric_columns))) {
      feature <- numeric_columns[i]
      p <- ggplot(data, aes_string(x = feature, fill = target_column)) +
        geom_density(alpha = 0.5) +
        labs(title = paste("Distribution of", feature)) +
        theme_minimal()
      numeric_plots[[feature]] <- p
      
      # Save individual plots
      png(file.path(REPORTS_DIR, paste0("feature_", feature, ".png")), 
          width = 800, height = 600)
      print(p)
      dev.off()
    }
    
    # 3. Correlation matrix (for numeric columns only)
    if (length(numeric_columns) > 1) {
      cor_matrix <- cor(data[numeric_columns], use = "complete.obs")
      png(file.path(REPORTS_DIR, "correlation_matrix.png"), width = 1000, height = 800)
      corrplot(cor_matrix, method = "color", type = "upper", 
               title = "Correlation Matrix of Numeric Features")
      dev.off()
    }
  }
  
  # 4. Statistical summary
  cat("\nStatistical summary:\n")
  print(summary(data))
  
  cat("\nClass distribution:\n")
  print(table(data[[target_column]]))
  
  cat("✓ Exploratory analysis completed. Plots saved to:", REPORTS_DIR, "\n")
}

# MAIN EXECUTION
if (sys.nframe() == 0) {
  cat("=== RUNNING EXPLORATORY ANALYSIS SCRIPT DIRECTLY ===\n")
  
  # Load preprocessed data
  train_data_path <- file.path(PROCESSED_DATA_DIR, "02_train_data.csv")
  target_info_path <- file.path(PROCESSED_DATA_DIR, "02_target_info.csv")
  
  if (file.exists(train_data_path) && file.exists(target_info_path)) {
    train_data <- read_csv(train_data_path, show_col_types = FALSE)
    target_info <- read_csv(target_info_path, show_col_types = FALSE)
    target_column <- target_info$target_column[1]
    
    cat("✓ Loaded training data\n")
    cat("Target column:", target_column, "\n")
    
    perform_eda(train_data, target_column)
  } else {
    cat("⚠ Preprocessed data not found. Running preprocessing first...\n")
    source("scripts/02_data_preprocessing.R")
    
    # Reload data after preprocessing
    train_data <- read_csv(train_data_path, show_col_types = FALSE)
    target_info <- read_csv(target_info_path, show_col_types = FALSE)
    target_column <- target_info$target_column[1]
    
    perform_eda(train_data, target_column)
  }
}
