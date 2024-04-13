#----------------------
w_setup <- function(n, T, seed.num = 2024) {
  #setup
  set.seed(seed.num)
  n <- n
  T <- T
  df <- data.frame(index = seq(1, n+1))
  # 1st step
  t <- c(0, sort(runif(n, min = 0, max = T)))
  W <- numeric(n + 1)
  W[1] <- 0
  # 2nd step
  Z <- rnorm(n)
  # save to global env
  assign("df", df, envir = .GlobalEnv)
  assign("n", n, envir = .GlobalEnv)
  assign("t", t, envir = .GlobalEnv)
  assign("T", T, envir = .GlobalEnv)
  assign("W", W, envir = .GlobalEnv)
  assign("Z", Z, envir = .GlobalEnv)
  
}
#----------------------
sapply_sbm <- function(n, T, seed.num = NULL){
  #safe-check................
  if (!is.null(seed.num)) {
    set.seed(seed.num)
  }
  #setup......................
  n <- n
  T <- T
  df <- data.frame(index = seq(1, n+1))
  # 1st step
  t <- c(0, sort(runif(n, min = 0, max = T)))
  W <- numeric(n + 1)
  W[1] <- 0
  # 2nd step
  Z <- rnorm(n)
  
  #..............................
  W <- c(0, sapply(2:(n + 1), function(i) {
    W[i - 1] + sqrt(t[i] - t[i - 1]) * Z[i - 1]
  }))
  #W
  df["W"] <- W
  df["t"] <- t
  
  # 4th step
  p <- ggplot(data = df, aes(x = t, y = W)) +
    geom_step() +
    labs(
      title = "Standard Brownian motion path simulation (sapply)",
      x = "Time (years)",
      y = "Brownian motion"
    )
  
  output <- list(n = n, T = T, df = df, t = t, W = W, Z = Z, p = p)
  return(output)
}
# a <- sapply_sbm(n = 10, T = 1)
# a$p
#---------------------
sapply_bm <- function(n, T, mu, sigma, seed.num = NULL){
  
  #safe-check................
  if (!is.null(seed.num)) {
    set.seed(seed.num)
  }
  #setup......................
  n <- n
  T <- T
  df <- data.frame(index = seq(1, n+1))
  # 1st step
  t <- c(0, sort(runif(n, min = 0, max = T)))
  W <- numeric(n + 1)
  W[1] <- 0
  # 2nd step
  Z <- rnorm(n)
  
  #..............................
  W <- c(0, sapply(2:(n + 1), function(i) {
    W[i - 1] + sqrt(t[i] - t[i - 1]) * Z[i - 1]
  }))
  
  X <- mu*t + sigma*W
  
  
  df["W"] <- W
  df["t"] <- t
  df["X"] <- X
  
  # 4th step
  p <- ggplot(data = df, aes(x = t, y = X)) +
    geom_step() +
    labs(
      title = "Brownian motion path simulation (sapply)",
      x = "Time (years)",
      y = "Stock price"
    )
  
  output <- list(n = n, T = T, df = df, t = t, W = W, Z = Z, X = X, p = p)
  return(output)
  
}
# a <- sapply_bm(n = 10, T = 1,mu = 5,sigma = 2)
# a$p
#---------------------
floop_gbm <- function(n, T, r, sigma, S.1, seed.num = NULL){
  
  #safe-check................
  if (!is.null(seed.num)) {
    set.seed(seed.num)
  }
  #setup......................
  n <- n
  T <- T
  df <- data.frame(index = seq(1, n+1))
  # 1st step
  t <- c(0, sort(runif(n, min = 0, max = T)))
  W <- numeric(n + 1)
  W[1] <- 0
  S <- numeric(n + 1)
  S[1] <- S.1
  # 2nd step
  Z <- rnorm(n)
  
  #..............................
  # 3rd step
  for (i in 1:n) {
    S[i + 1] <- S[i]*exp((r - 0.5*sigma^2)*(t[i + 1] - t[i]) +
                           sigma*sqrt(t[i + 1] - t[i])*Z[i])
    
  }
  
  df["S"] <- S
  df["W"] <- W
  df["t"] <- t
  
  # 4th step
  p <- ggplot(data = df, aes(x = t, y = S)) +
    geom_step() +
    labs(
      title = "Geometric Brownian motion path simulation",
      x = "Time (years)",
      y = "Stock price"
    )
  
  output <- list(n = n, T = T, df = df, t = t, W = W, Z = Z, S = S, p = p)
  return(output)
  
}
# a$p
#-------------
sapply_gbm <- function(n, T, r, sigma, S.1, seed.num = NULL){
  
  #safe-check................
  if (!is.null(seed.num)) {
    set.seed(seed.num)
  }
  #setup......................
  n <- n
  T <- T
  df <- data.frame(index = seq(1, n+1))
  # 1st step
  t <- c(0, sort(runif(n, min = 0, max = T)))
  W <- numeric(n + 1)
  W[1] <- 0
  S <- numeric(n + 1)
  S[1] <- S.1
  # 2nd step
  Z <- rnorm(n)
  
  #..............................
  # 3rd step
  S <- c(S.1,sapply(2:(n+1), function(i){
    S[i] <<- S[i-1]*exp((r - 0.5*sigma^2)*(t[i] - t[i-1]) + 
                          sigma*sqrt(t[i] - t[i-1])*Z[i-1])
  }
  ))
  
  df["S"] <- S
  df["W"] <- W
  df["t"] <- t
  
  # 4th step
  p <- ggplot(data = df, aes(x = t, y = S)) +
    geom_step() +
    labs(
      title = "Geometric Brownian motion path simulation (sapply)",
      x = "Time (years)",
      y = "Stock price"
    )
  
  output <- list(n = n, T = T, df = df, t = t, W = W, Z = Z, S = S, p = p)
  return(output)
  
}
#-------------------
# Define a function for simulating the Vasicek model
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
# n <- 2
# m <- 2
# T <- 1
# alpha <- 0.4
# b <- 0.04
# sigma <- 0.1

# vasicek_simulation <- simulate_vasicek(n, m, T, alpha, b, sigma)
# vasicek_simulation$df

calculate_stats_vasicek <- function(df) {
  # Calculate mean, variance, and other statistics
  stats <- df %>%
    group_by(Path) %>%
    summarise(
      Mean = mean(Interest_Rate),
      Variance = var(Interest_Rate),
      SD = sd(Interest_Rate),
      Min = min(Interest_Rate),
      Max = max(Interest_Rate),
      Q1 = quantile(Interest_Rate, 0.25),
      Median = median(Interest_Rate),
      Q3 = quantile(Interest_Rate, 0.75)
    )
  
  # Create a histogram of interest rates
  hist_plot <- ggplot(df, aes(x = Interest_Rate)) +
    geom_histogram(binwidth = 0.02, fill = "skyblue", color = "black") +
    labs(x = "Interest Rate", y = "Frequency", title = "Distribution of Interest Rates")
  
  # Return the calculated statistics and histogram plot
  return(list(stats = stats, hist_plot = hist_plot))
}

# a <- calculate_stats_vasicek(vasicek_simulation$df)
# a$stats
# a$hist_plot
#------------
floop_sbm_mtx <- function(m, n, T, seed.num = NULL){
  #safe-check................
  if (!is.null(seed.num)) {
    set.seed(seed.num)
  }
  #setup......................
  n <- n
  T <- T
  m <- m
  
  # 1st step
  t <- c(0, sort(runif(n, min = 0, max = T)))
  W <- matrix(numeric(), nrow = m, ncol = n + 1)
  W[,1] <- 0
  
  # 2nd step
  Z <- matrix(rnorm(m * n), nrow = m, ncol = n)
  
  #..............................
  # Simulate Brownian motion using a for loop
  for (i in 2:(n + 1)) {
    W[, i] <- W[, i - 1] + sqrt(t[i] - t[i - 1]) * Z[, i - 1]
  }
  
  #add w to the dataframe
  df <- as.data.frame(W) |>
    setNames(t) |>
    mutate(Path = row_number()) |>
    pivot_longer(cols = -Path, names_to = "Time", values_to = "W")
  
  # 4th step
  p <- ggplot(df, aes(x = Time, y = W, group = Path, color = factor(Path))) +
    geom_line(alpha = 0.5) +
    labs(x = "Time", y = "dW", title = "Brownian Motion", color = "Path Number") +
    theme_minimal()
  
  output <- list(n = n, T = T, df = df, t = t, W = W, Z = Z, p = p)
  return(output)
}
# n = 10; T = 1; m = 2
# a <- floop_sbm_mtx(n = 10, T = 1, m = 10)
# a$p

#-------------------
sapply_sbm_mtx <- function(m, n, T, seed.num = NULL){
  #safe-check................
  if (!is.null(seed.num)) {
    set.seed(seed.num)
  }
  #setup......................
  n <- n
  T <- T
  m <- m
  
  # 1st step
  t <- c(0, sort(runif(n, min = 0, max = T)))
  W <- matrix(numeric(), nrow = m, ncol = n + 1)
  W[,1] <- 0
  
  # 2nd step
  Z <- matrix(rnorm(m * n), nrow = m, ncol = n)
  
  #..............................
  # Simulate Brownian motion using a for loop
  sapply(2:(n + 1), function(i) {
    W[,i] <<- W[, i - 1] + sqrt(t[i] - t[i - 1]) * Z[, i - 1]
  })
  
  
  #add w to the dataframe
  df <- as.data.frame(W) |>
    setNames(t) |>
    mutate(Path = row_number()) |>
    pivot_longer(cols = -Path, names_to = "Time", values_to = "W")
  
  # 4th step
  p <- ggplot(df, aes(x = Time, y = W, group = Path, color = factor(Path))) +
    geom_line(alpha = 0.5) +
    labs(x = "Time", y = "dW", title = "Brownian Motion", color = "Path Number") +
    theme_minimal()
  
  output <- list(n = n, T = T, df = df, t = t, W = W, Z = Z, p = p)
  return(output)
}
# n = 10; T = 1; m = 2; set.seed(2024)
# a <- sapply_sbm_mtx(n = 10, T = 1, m = 10)
# a$p
