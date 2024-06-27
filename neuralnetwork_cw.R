# Load necessary libraries
library(neuralnet)
library(readxl)
library(ggplot2)
library(tidyr)

# Function to calculate statistical indices
calculate_indices <- function(actual, predicted) {
  residuals <- actual - predicted
  rmse <- sqrt(mean(residuals^2)) # Root Mean Squared Error
  mae <- mean(abs(residuals)) # Mean Absolute Error
  mape <- mean(abs(residuals / actual) * 100) # Mean Absolute Percentage Error
  smape <- mean(2 * abs(residuals) / (abs(actual) + abs(predicted))) * 100 # Symmetric Mean Absolute Percentage Error
  return(c(RMSE = rmse, MAE = mae, MAPE = mape, sMAPE = smape))
}

# -------------------------------------------------------------------------


# Load data
exchange_data <- read_excel("ExchangeUSD.xlsx")

# Extract USD/EUR exchange rate data (3rd column)
exchange_rate <- exchange_data$`USD/EUR`

# Split data into training and testing sets
train_data <- exchange_rate[1:400]
test_data <- exchange_rate[401:500]

# Function to create time-delayed input vectors
create_input_vectors <- function(data, delay) {
  input_vectors <- matrix(, nrow = length(data) - delay, ncol = delay)
  for (i in 1:delay) {
    input_vectors[,i] <- data[(delay - i + 1):(length(data) - i)]
  }
  return(input_vectors)
}

# Function to normalize data
normalize_data <- function(data) {
  normalized_data <- (data - min(data)) / (max(data) - min(data))
  return(list(data = normalized_data, min = min(data), max = max(data)))
}

# Function to denormalize data
denormalize_data <- function(data, min, max) {
  denormalized_data <- data * (max - min) + min
  return(denormalized_data)
}

# Normalize training and testing data
train_norm <- normalize_data(train_data)
test_norm <- normalize_data(test_data)


train_data_norm <- train_norm$data
test_data_norm <- test_norm$data
min_test <- test_norm$min
max_test <- test_norm$max

# Define custom MLP configurations
custom_configs <- list(
  list(delay = 1, hidden_layers = 5),
  list(delay = 2, hidden_layers = 5),
  list(delay = 3, hidden_layers = 5),
  list(delay = 4, hidden_layers = 5),
  list(delay = 4, hidden_layers = 10),
  list(delay = 4, hidden_layers = c(5,2)),
  list(delay = 4, hidden_layers = c(10, 7)),
  list(delay = 4, hidden_layers = c(7, 4)),
  list(delay = 5, hidden_layers = 5),
  list(delay = 5, hidden_layers = 10),
  list(delay = 5, hidden_layers = c(5, 2)),
  list(delay = 5, hidden_layers = c(7, 4))
)

# Initialize list to store models and performance
mlp_models <- list()
performance <- matrix(NA, nrow = length(custom_configs), ncol = 5,
                      dimnames = list(NULL, c("Model", "RMSE", "MAE", "MAPE", "sMAPE")))

# Train models for each custom configuration
for (i in 1:length(custom_configs)) {
  config <- custom_configs[[i]]
  
  # Create input/output matrices for training/testing
  train_input <- create_input_vectors(train_data_norm, config$delay)
  train_output <- train_data_norm[(config$delay + 1):length(train_data_norm)]
  test_input <- create_input_vectors(test_data_norm, config$delay)
  test_output <- test_data_norm[(config$delay + 1):length(test_data_norm)]
  
  # Define MLP model
  set.seed(123)
  model <- neuralnet(train_output ~ .,
                     data = cbind(train_input, train_output),
                     hidden = config$hidden_layers,
                     linear.output = FALSE)
  
  # Predict using the model
  predictions <- predict(model, as.data.frame(test_input))
  
  # Denormalize predictions
  predictions_denorm <- denormalize_data(predictions, min_test, max_test)
  
  # Denormalize test output
  test_output_denorm <- denormalize_data(test_output, min_test, max_test)
  
  # Calculate indices
  indices <- calculate_indices(test_output_denorm, predictions_denorm)
  
  # Store performance and model
  performance[i, ] <- c(paste("Model ", i), indices)
  mlp_models[[i]] <- list(model = model, predictions = predictions_denorm, indices = indices)
  
  # Plot predictions and neural network diagram for each model
  model_info <- mlp_models[[i]]
  model <- model_info$model
  predictions <- model_info$predictions
  indices <- model_info$indices
  
  # Plot predictions
  plot(test_output_denorm, type = "l", col = "blue", ylim = c(min_test, max_test), xlab = "Time", ylab = "Exchange Rate", main = paste("Model ", i, " Predictions", sep = ""))
  lines(predictions, type = "l", col = "red")
  legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)
  
  # Plot neural network diagram
  plot(model)
  
  # Print indices
  cat("\nPerformance Metrics for Model ", i, ":\n", sep = "")
  print(indices)
  
  # Wait for user input before proceeding to the next model
  readline(prompt = "Press Enter to continue...")
  
  # Clear the plot for the next iteration
  dev.off()
}

# Create comparison table
comparison_table <- data.frame(performance)
print("Comparison Table for Different Models:")
print(comparison_table)

# Select the best model based on RMSE
best_model_index <- which.min(performance[, "RMSE"])
best_model_info <- mlp_models[[best_model_index]]
best_model <- best_model_info$model
best_model_indices <- best_model_info$indices
best_model_predictions <- best_model_info$predictions

# Plot the predictions of the best model
plot(test_output_denorm, type = "l", col = "blue", ylim = c(min_test, max_test), xlab = "Time", ylab = "Exchange Rate", main = "USD/EUR Exchange Rate Prediction (Best Model)")
lines(best_model_predictions, type = "l", col = "red")
legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)

# Print statistical indices for the best model and plot the best model
cat("\nStatistical Indices for the Best Model and the Best model plot:\n")
print(best_model_indices)
plot(best_model)