simulate_vasicek <- function(n, m, T, alpha, b, sigma) {
  library(MASS)
  library(dplyr)
  library(tidyverse)
  dt <- T / n
  
  # Initialize matrices for interest rates and random variables
  r <- matrix(0, nrow = m, ncol = n + 1)
  Z <- matrix(rnorm(m * n), nrow = m, ncol = n)
  t <- numeric(n)
  r[, 1] <- 0.0625
  t[1] <- 0
  
  # Simulate interest rate paths using Vasicek model
  for (i in 1:n) {
    t[i + 1] = t[i] + dt
    r[, i + 1] <- r[, i] + alpha * (b - r[, i]) + sigma * sqrt(dt) * Z[, i]
  }
  
  # Convert the data to a data frame for ggplot
  df_long <- as.data.frame(r) |>
    setNames(t) |>
    mutate(Path = row_number()) |>
    pivot_longer(cols = -Path, names_to = "Time", values_to = "Interest_Rate")
  
  # Create the plot using ggplot2
  p <- ggplot(df_long, aes(x = Time, y = Interest_Rate, group = Path, color = factor(Path))) +
    geom_line(alpha = 0.5) +
    labs(x = "Time", y = "Interest Rates", title = "Simulated Interest Rate Paths", color = "Path Number") +
    theme_minimal()
  
  # Return the simulated interest rate paths
  return(list(t = t, r = r, p = p, df = df_long))
}
# Example usage of the function with parameters
n <- 2
m <- 2
T <- 1
alpha <- 0.4
b <- 0.04
sigma <- 0.1

vasicek_simulation <- simulate_vasicek(n, m, T, alpha, b, sigma)
vasicek_simulation$df

