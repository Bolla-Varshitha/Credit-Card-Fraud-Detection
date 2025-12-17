#' Model Evaluation Script - Fixed Version

library(caret)
library(pROC)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

source("config/config.R")

evaluate_models <- function(models, predictions, test_data, target_column = "Class") {
  cat("Evaluating model performance...\n")
  cat("Target column:", target_column, "\n")
  
  results <- data.frame()
  roc_curves <- list()
  confusion_matrices <- list()
  
  model_names <- names(models)
  cat("Models to evaluate:", paste(model_names, collapse = ", "), "\n")
  
  for (model_name in model_names) {
    if (is.null(models[[model_name]])) {
      cat("Skipping", model_name, "- model is NULL\n")
      next
    }
    
    cat("Evaluating", model_name, "...\n")
    
    # Get predictions
    pred_classes <- predictions[[model_name]]
    pred_probs_name <- paste0(model_name, "_probs")
    pred_probs <- predictions[[pred_probs_name]]
    
    if (is.null(pred_classes) || is.null(pred_probs)) {
      cat("Skipping", model_name, "- predictions not available\n")
      next
    }
    
    tryCatch({
      # Calculate metrics
      cm <- confusionMatrix(pred_classes, test_data[[target_column]], positive = "Fraud")
      
      # ROC curve - extract AUC as numeric
      roc_obj <- roc(as.numeric(test_data[[target_column]] == "Fraud"), pred_probs)
      auc_value <- as.numeric(auc(roc_obj))  # Convert to numeric
      roc_curves[[model_name]] <- roc_obj
      
      # Collect results - ensure all values are numeric
      model_results <- data.frame(
        Model = model_name,
        Accuracy = as.numeric(cm$overall["Accuracy"]),
        Precision = as.numeric(cm$byClass["Precision"]),
        Recall = as.numeric(cm$byClass["Recall"]),
        F1 = as.numeric(cm$byClass["F1"]),
        AUC = auc_value,  # Now this is numeric
        Sensitivity = as.numeric(cm$byClass["Sensitivity"]),
        Specificity = as.numeric(cm$byClass["Specificity"]),
        stringsAsFactors = FALSE
      )
      
      results <- rbind(results, model_results)
      confusion_matrices[[model_name]] <- cm$table
      
      cat("✓", model_name, "evaluated successfully\n")
      cat("  AUC:", round(auc_value, 4), "\n")
      cat("  Accuracy:", round(as.numeric(cm$overall["Accuracy"]), 4), "\n")
      
    }, error = function(e) {
      cat("✗ Error evaluating", model_name, ":", e$message, "\n")
    })
  }
  
  return(list(
    metrics = results,
    roc_curves = roc_curves,
    confusion_matrices = confusion_matrices
  ))
}

create_evaluation_plots <- function(evaluation_results) {
  cat("Creating evaluation plots...\n")
  
  if (nrow(evaluation_results$metrics) == 0) {
    cat("No metrics available for plotting\n")
    return(NULL)
  }
  
  # Ensure all metrics are numeric for plotting
  metrics_df <- evaluation_results$metrics %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
  
  cat("Metrics for plotting:\n")
  print(metrics_df)
  
  # 1. Metrics comparison plot
  metrics_long <- metrics_df %>%
    select(Model, Accuracy, Precision, Recall, F1, AUC) %>%
    pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")
  
  metrics_plot <- ggplot(metrics_long, aes(x = Model, y = Value, fill = Model)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ Metric, scales = "free_y") +
    labs(title = "Model Performance Comparison",
         y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = round(Value, 3)), vjust = -0.3, size = 3)
  
  print(metrics_plot)
  
  # 2. ROC curves plot
  if (length(evaluation_results$roc_curves) > 0) {
    roc_data <- data.frame()
    for (model_name in names(evaluation_results$roc_curves)) {
      roc_obj <- evaluation_results$roc_curves[[model_name]]
      roc_df <- data.frame(
        FPR = 1 - roc_obj$specificities,
        TPR = roc_obj$sensitivities,
        Model = paste0(model_name, " (AUC = ", round(auc(roc_obj), 3), ")")
      )
      roc_data <- rbind(roc_data, roc_df)
    }
    
    roc_plot <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
      geom_line(size = 1) +
      geom_abline(linetype = "dashed", color = "gray") +
      labs(title = "ROC Curves",
           x = "False Positive Rate",
           y = "True Positive Rate") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
    
    print(roc_plot)
  } else {
    roc_plot <- NULL
    cat("No ROC curves available for plotting\n")
  }
  
  # Save plots
  if (!dir.exists(REPORTS_DIR)) {
    dir.create(REPORTS_DIR, recursive = TRUE)
  }
  
  # Save metrics plot
  png(file.path(REPORTS_DIR, "model_metrics_comparison.png"), 
      width = 1200, height = 800)
  print(metrics_plot)
  dev.off()
  cat("✓ Saved metrics comparison plot\n")
  
  # Save ROC plot if available
  if (!is.null(roc_plot)) {
    png(file.path(REPORTS_DIR, "roc_curves.png"), width = 800, height = 600)
    print(roc_plot)
    dev.off()
    cat("✓ Saved ROC curves plot\n")
  }
  
  # Save confusion matrices as images
  if (length(evaluation_results$confusion_matrices) > 0) {
    for (model_name in names(evaluation_results$confusion_matrices)) {
      cm <- as.data.frame(evaluation_results$confusion_matrices[[model_name]])
      
      cm_plot <- ggplot(cm, aes(x = Reference, y = Prediction, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = Freq), color = "white", size = 6) +
        scale_fill_gradient(low = "blue", high = "red") +
        labs(title = paste("Confusion Matrix -", model_name),
             x = "Actual",
             y = "Predicted") +
        theme_minimal()
      
      png(file.path(REPORTS_DIR, paste0("confusion_matrix_", model_name, ".png")), 
          width = 600, height = 500)
      print(cm_plot)
      dev.off()
      cat("✓ Saved confusion matrix for", model_name, "\n")
    }
  } else {
    cat("No confusion matrices available for plotting\n")
  }
  
  return(list(metrics_plot = metrics_plot, roc_plot = roc_plot))
}

save_evaluation_results <- function(evaluation_results) {
  if (!dir.exists(REPORTS_DIR)) {
    dir.create(REPORTS_DIR, recursive = TRUE)
  }
  
  # Ensure metrics are numeric before saving
  metrics_to_save <- evaluation_results$metrics %>%
    mutate(across(where(is.numeric), ~ round(., 6)))
  
  write_csv(metrics_to_save, file.path(REPORTS_DIR, "06_model_metrics.csv"))
  cat("✓ Model metrics saved to:", file.path(REPORTS_DIR, "06_model_metrics.csv"), "\n")
  
  # Save ROC curves if available
  if (length(evaluation_results$roc_curves) > 0) {
    saveRDS(evaluation_results$roc_curves, 
            file.path(REPORTS_DIR, "06_roc_curves.rds"))
    cat("✓ ROC curves saved\n")
  }
  
  # Save confusion matrices if available
  if (length(evaluation_results$confusion_matrices) > 0) {
    saveRDS(evaluation_results$confusion_matrices, 
            file.path(REPORTS_DIR, "06_confusion_matrices.rds"))
    cat("✓ Confusion matrices saved\n")
  }
  
  # Create a summary report
  create_summary_report(evaluation_results)
}

create_summary_report <- function(evaluation_results) {
  if (nrow(evaluation_results$metrics) == 0) {
    return()
  }
  
  # Find best model by F1 score - using base R instead of dplyr slice
  sorted_metrics <- evaluation_results$metrics[order(-evaluation_results$metrics$F1), ]
  best_model <- sorted_metrics[1, ]
  
  # Create a simple text summary
  summary_text <- paste(
    "=== CREDIT CARD FRAUD DETECTION - MODEL EVALUATION SUMMARY ===\n",
    "\nTrained Models:", paste(evaluation_results$metrics$Model, collapse = ", "),
    "\n\nBest Model:", best_model$Model,
    "\nBest F1 Score:", round(best_model$F1, 4),
    "\nBest AUC:", round(best_model$AUC, 4),
    "\nBest Accuracy:", round(best_model$Accuracy, 4),
    "\n\nDetailed Metrics:\n"
  )
  
  # Add detailed metrics
  metrics_text <- capture.output(print(evaluation_results$metrics, row.names = FALSE))
  summary_text <- paste(summary_text, paste(metrics_text, collapse = "\n"), sep = "\n")
  
  # Write to file
  writeLines(summary_text, file.path(REPORTS_DIR, "06_evaluation_summary.txt"))
  cat("✓ Evaluation summary saved\n")
}

# MAIN EXECUTION
if (sys.nframe() == 0) {
  cat("=== RUNNING MODEL EVALUATION SCRIPT DIRECTLY ===\n")
  
  # Load models and predictions
  model_files <- list.files(MODELS_DIR, pattern = "05_.*_model\\.rds", full.names = TRUE)
  predictions_path <- file.path(PROCESSED_DATA_DIR, "05_predictions.rds")
  test_data_path <- file.path(PROCESSED_DATA_DIR, "04_test_processed.csv")
  target_info_path <- file.path(PROCESSED_DATA_DIR, "04_target_info.csv")
  
  if (length(model_files) > 0 && file.exists(predictions_path) && 
      file.exists(test_data_path) && file.exists(target_info_path)) {
    
    # Load models
    models <- list()
    for (file in model_files) {
      model_name <- gsub("05_(.*)_model\\.rds", "\\1", basename(file))
      models[[model_name]] <- readRDS(file)
      cat("✓ Loaded", model_name, "model\n")
    }
    
    # Load predictions and test data
    predictions <- readRDS(predictions_path)
    test_data <- read_csv(test_data_path, show_col_types = FALSE)
    target_info <- read_csv(target_info_path, show_col_types = FALSE)
    target_column <- target_info$target_column[1]
    
    cat("✓ Loaded predictions and test data\n")
    cat("Models loaded:", names(models), "\n")
    cat("Test data dimensions:", dim(test_data), "\n")
    
    # Convert test data target to factor
    test_data[[target_column]] <- as.factor(test_data[[target_column]])
    
    # Evaluate models
    evaluation_results <- evaluate_models(models, predictions, test_data, target_column)
    
    # Create plots
    plots <- create_evaluation_plots(evaluation_results)
    
    # Save results
    save_evaluation_results(evaluation_results)
    
    # Print results
    cat("\n")
    cat(strrep("=", 50), "\n")
    cat("MODEL PERFORMANCE SUMMARY\n")
    cat(strrep("=", 50), "\n")
    print(evaluation_results$metrics)
    
    if (nrow(evaluation_results$metrics) > 0) {
      best_model <- evaluation_results$metrics %>%
        arrange(desc(F1)) %>%
        slice(1)
      
      cat("\n")
      cat(strrep("=", 50), "\n")
      cat("BEST MODEL:", best_model$Model, "\n")
      cat("F1 Score:", round(best_model$F1, 4), "\n")
      cat("AUC:", round(best_model$AUC, 4), "\n")
      cat("Accuracy:", round(best_model$Accuracy, 4), "\n")
      cat(strrep("=", 50), "\n")
    }
    
    cat("\n✅ MODEL EVALUATION COMPLETED SUCCESSFULLY!\n")
    cat("Check the 'reports' folder for all outputs:\n")
    cat("- Model metrics: 06_model_metrics.csv\n")
    cat("- ROC curves: roc_curves.png\n")
    cat("- Performance comparison: model_metrics_comparison.png\n")
    cat("- Confusion matrices: confusion_matrix_*.png\n")
    cat("- Summary: 06_evaluation_summary.txt\n")
    
  } else {
    cat("⚠ Required files not found. Please run model training script first.\n")
    cat("Missing files:\n")
    if (length(model_files) == 0) cat("  - No model files in models directory\n")
    if (!file.exists(predictions_path)) cat("  -", predictions_path, "\n")
    if (!file.exists(test_data_path)) cat("  -", test_data_path, "\n")
    if (!file.exists(target_info_path)) cat("  -", target_info_path, "\n")
  }
}
