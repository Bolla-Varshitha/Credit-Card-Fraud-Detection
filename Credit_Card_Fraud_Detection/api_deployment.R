# api_deployment.R
library(plumber)
library(caret)

# Load your trained model
model <- readRDS("models/05_xgb_model.rds")

#* @apiTitle Credit Card Fraud Detection API
#* @apiDescription Real-time fraud detection for credit card transactions

#* @post /predict
#* @param transaction_data JSON transaction data
function(transaction_data) {
  
  # Convert and preprocess
  transaction_df <- as.data.frame(transaction_data)
  processed_data <- create_features_for_prediction(transaction_df)
  
  # Make prediction
  prediction <- predict(model, processed_data)
  probability <- predict(model, processed_data, type = "prob")$Fraud
  
  return(list(
    transaction_id = transaction_data$id,
    prediction = as.character(prediction),
    fraud_probability = probability,
    timestamp = Sys.time(),
    status = ifelse(probability > 0.5, "high_risk", "low_risk")
  ))
}

#* @get /health
function() {
  return(list(status = "OK", model_loaded = !is.null(model)))
}

# To run: pr("api_deployment.R") %>% pr_run(port=8000)
