# l <- 1000
# y <- ts(start = 1,end = l,data = rnorm(l))
# library(ggplot2)
# # Create a ggplot with the ts object
# ggplot(data = data.frame(x = time(y), y = y), aes(x = x, y = y)) +
#   geom_line() +
#   labs(x = "Time", y = "Value", title = "Time Series Plot")
# 
# #test for stationarity
# tseries::adf.test(y)
# 
# 
# ggplot(data = data.frame(x = time(y), y = y), aes(y = y)) +
#   geom_histogram()
# Load the stats package (if not already loaded)

library(ggplot2)
# Function to generate AR(1) process
generate_AR1 <- function(phi, sigma, n) {
  # Initialize an empty vector to store the generated values
  ar1_data <- numeric(n)
  
  # Generate the AR(1) process
  for (i in 2:n) {
    ar1_data[i] <- phi * ar1_data[i - 1] + rnorm(1, mean = 0, sd = sigma)
  }
  
  # Return the generated AR(1) process
  return(ar1_data)
}

# Set the parameters for the AR(1) process
phi <- 0.7  # Autoregressive coefficient
sigma <- 1  # Standard deviation of the random error
n <- 100    # Number of observations to generate

# Generate the AR(1) process using the custom function
ar1_data_custom <- generate_AR1(phi, sigma, n)

# Create a data frame for ggplot
ar1_df <- data.frame(Time = 1:n, Value = ar1_data_custom)

# Plot using ggplot
ggplot(ar1_df, aes(x = Time, y = Value)) +
  geom_line() +
  labs(title = "AR(1) Process",
       x = "Time",
       y = "Value")

