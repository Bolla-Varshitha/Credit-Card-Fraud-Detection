# api_server.R
library(plumber)
library(caret)

# Load the trained model
model <- readRDS("models/05_xgb_model.rds")

#* @apiTitle Credit Card Fraud Detection API
#* @apiDescription Detect fraudulent credit card transactions

#* @post /predict
#* @param transaction_data JSON object containing transaction features
function(transaction_data) {
  
  # Convert JSON to data frame
  new_data <- as.data.frame(transaction_data)
  
  # Make prediction
  prediction <- predict(model, new_data)
  probability <- predict(model, new_data, type = "prob")$Fraud
  
  return(list(
    prediction = as.character(prediction),
    fraud_probability = probability,
    is_fraud = probability > 0.5,
    confidence = ifelse(probability > 0.5, probability, 1 - probability)
  ))
}

# Run the API
# pr <- plumber::plumb("api_server.R")
# pr$run(port = 8000)
