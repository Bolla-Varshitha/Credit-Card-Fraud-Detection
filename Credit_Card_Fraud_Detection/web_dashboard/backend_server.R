# backend_server.R - Simple backend to serve the frontend
library(plumber)

# Load model and functions
model <- readRDS("models/05_xgb_model.rds")

# Feature engineering function
create_features_for_prediction <- function(data) {
  # Include your feature engineering code here
  # ... (same as in predict_fraud.R)
  return(data_with_features)
}

#* @apiTitle Fraud Detection API
#* @apiDescription Backend for fraud detection web dashboard

#* @get /health
function() {
  list(status = "OK", timestamp = Sys.time())
}

#* @post /predict
#* @param transaction_data JSON with transaction details
function(transaction_data) {
  
  # Convert to data frame
  transaction_df <- as.data.frame(transaction_data)
  
  # Create features
  transaction_with_features <- create_features_for_prediction(transaction_df)
  
  # Make prediction
  prediction <- predict(model, transaction_with_features)
  probability <- predict(model, transaction_with_features, type = "prob")$Fraud
  
  return(list(
    prediction = as.character(prediction),
    fraud_probability = probability,
    risk_level = ifelse(probability > 0.7, "High", 
                       ifelse(probability > 0.5, "Medium", "Low")),
    timestamp = Sys.time()
  ))
}

#* @post /batch_predict
#* @param file_path Path to CSV file
function(file_path) {
  # Batch processing logic
  transactions <- read.csv(file_path)
  # Process and return results
  return(list(processed = nrow(transactions), fraud_count = 0))
}

#* @plumber
function(pr) {
  pr
}
