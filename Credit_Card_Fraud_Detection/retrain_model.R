# retrain_model.R
# Automated model retraining with new data
retrain_model <- function(new_data_path) {
  # Load new data
  new_data <- read.csv(new_data_path)
  
  # Combine with existing data
  full_data <- rbind(existing_data, new_data)
  
  # Retrain models
  updated_models <- train_models(full_data)
  
  # Validate performance
  new_performance <- evaluate_models(updated_models)
  
  # Deploy if performance improves
  if(new_performance$auc > current_performance$auc) {
    saveRDS(updated_models, "models/updated_model.rds")
    cat("✅ Model updated successfully!\n")
  }
}
