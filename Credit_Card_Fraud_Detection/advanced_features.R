# advanced_features.R
# Create more sophisticated features
create_advanced_features <- function(data) {
  data %>%
    mutate(
      # Time-based features
      hour_of_day = floor(Time / 3600) %% 24,
      is_weekend = ifelse(hour_of_day %in% c(0, 6), 1, 0),
      
      # Behavioral features
      transaction_frequency = calculate_frequency(card_number),
      avg_transaction_amount = calculate_avg_amount(card_number),
      
      # Anomaly detection features
      amount_zscore = (Amount - mean(Amount)) / sd(Amount),
      location_anomaly = detect_location_anomaly(card_number, merchant_location)
    )
}
