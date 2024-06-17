# Load necessary libraries
library(tidyverse)
library(tseries)
library(quantmod)
library(zoo)

# Symbols of interest
mySymbols <- c('AMZN', 'NFLX')
from <- "2020-07-01"
to <- "2020-11-13"
getSymbols(mySymbols, from = from, to = to, src = "yahoo", adjust = TRUE)

# Get the log of the close prices
AMZN.cl <- log(Cl(AMZN))
NFLX.cl <- log(Cl(NFLX))

# Combine into one data frame
prices <- merge(AMZN.cl, NFLX.cl)
colnames(prices) <- c("AMZN", "NFLX")

# Rolling window size
window_size <- 30

# Function to perform rolling regression and calculate z-scores
rolling_regression <- function(prices, window) {
  results <- data.frame(date = index(prices), alpha = NA, beta = NA, residuals = NA, z_score = NA, cointegrated = NA)
  
  for (i in window:nrow(prices)) {
    window_data <- prices[(i - window + 1):i, ]
    
    # Perform regression (without intercept)
    model <- lm(window_data$AMZN ~ window_data$NFLX - 1)
    beta <- coef(model)
    residuals <- residuals(model)
    
    # Apply the ADF test to the residuals
    adf_pvalue <- adf.test(residuals)$p.value
    
    # Determine if the pair is cointegrated
    cointegrated <- ifelse(adf_pvalue <= 0.05, TRUE, FALSE)
    
    # Only calculate z-score if the pair is cointegrated
    if (cointegrated) {
      mean_resid <- mean(residuals)
      sd_resid <- sd(residuals)
      z_score <- (residuals[length(residuals)] - mean_resid) / sd_resid
    } else {
      z_score <- NA
    }
    
    # Store results
    results$alpha[i] <- 0  # No intercept in this model
    results$beta[i] <- beta
    results$residuals[i] <- residuals[length(residuals)]
    results$z_score[i] <- z_score
    results$cointegrated[i] <- cointegrated
  }
  
  return(results)
}

# Perform rolling regression
rolling_results <- rolling_regression(prices, window_size)

# Define thresholds for trading signals
threshold <- 1

# Initialize signal vectors
rolling_results$long_signal <- 0
rolling_results$short_signal <- 0

# Generate signals based on z-score and cointegration status
for (i in 1:nrow(rolling_results)) {
  if (!is.na(rolling_results$z_score[i])) {
    if (rolling_results$z_score[i] > threshold) {
      rolling_results$short_signal[i] <- 1  # Short signal
    } else if (rolling_results$z_score[i] < -threshold) {
      rolling_results$long_signal[i] <- 1  # Long signal
    }
  }
}

# Print the first few rows of results to check
print(head(rolling_results, 40))

# Plotting z-scores and signals for visualization
library(ggplot2)
ggplot(rolling_results, aes(x = date)) +
  geom_line(aes(y = z_score), color = "blue") +
  geom_point(aes(y = ifelse(long_signal == 1, z_score, NA)), color = "green", size = 2) +
  geom_point(aes(y = ifelse(short_signal == 1, z_score, NA)), color = "red", size = 2) +
  labs(title = "Rolling Z-Scores with Trading Signals", y = "Z-Score", x = "Date") +
  theme_minimal()


#----------------
#with no cointegration testing
# # Load necessary libraries
# library(tidyverse)
# library(tseries)
# library(quantmod)
# library(zoo)
# 
# # Symbols of interest
# mySymbols <- c('AMZN', 'NFLX')
# from <- "2020-07-01"
# to <- "2020-11-13"
# getSymbols(mySymbols, from = from, to = to, src = "yahoo", adjust = TRUE)
# 
# # Get the log of the close prices
# AMZN.cl <- log(Cl(AMZN))
# NFLX.cl <- log(Cl(NFLX))
# 
# # Combine into one data frame
# prices <- merge(AMZN.cl, NFLX.cl)
# colnames(prices) <- c("AMZN", "NFLX")
# 
# # Rolling window size
# window_size <- 30
# 
# # Function to perform rolling regression and calculate z-scores
# rolling_regression <- function(prices, window) {
#   results <- data.frame(date = index(prices), alpha = NA, beta = NA, residuals = NA, z_score = NA)
#   
#   for (i in window:nrow(prices)) {
#     window_data <- prices[(i - window + 1):i, ]
#     
#     # Perform regression (without intercept)
#     model <- lm(window_data$AMZN ~ window_data$NFLX - 1)
#     beta <- coef(model)
#     residuals <- residuals(model)
#     
#     # Calculate z-score for the most recent residual
#     mean_resid <- mean(residuals)
#     sd_resid <- sd(residuals)
#     z_score <- (residuals[length(residuals)] - mean_resid) / sd_resid
#     
#     # Store results
#     results$alpha[i] <- 0  # No intercept in this model
#     results$beta[i] <- beta
#     results$residuals[i] <- residuals[length(residuals)]
#     results$z_score[i] <- z_score
#   }
#   
#   return(results)
# }
# 
# # Perform rolling regression
# rolling_results <- rolling_regression(prices, window_size)
# 
# # Define thresholds for trading signals
# threshold <- 1
# 
# # Initialize signal vectors
# rolling_results$long_signal <- 0
# rolling_results$short_signal <- 0
# 
# # Generate signals based on z-score
# for (i in 1:nrow(rolling_results)) {
#   if (!is.na(rolling_results$z_score[i])) {
#     if (rolling_results$z_score[i] > threshold) {
#       rolling_results$short_signal[i] <- 1  # Short signal
#     } else if (rolling_results$z_score[i] < -threshold) {
#       rolling_results$long_signal[i] <- 1  # Long signal
#     }
#   }
# }
# 
# # Print the first few rows of results to check
# print(head(rolling_results, 40))
# 
# # Plotting z-scores and signals for visualization
# library(ggplot2)
# ggplot(rolling_results, aes(x = date)) +
#   geom_line(aes(y = z_score), color = "blue") +
#   geom_point(aes(y = ifelse(long_signal == 1, z_score, NA)), color = "green", size = 2) +
#   geom_point(aes(y = ifelse(short_signal == 1, z_score, NA)), color = "red", size = 2) +
#   labs(title = "Rolling Z-Scores with Trading Signals", y = "Z-Score", x = "Date") +
#   theme_minimal()
