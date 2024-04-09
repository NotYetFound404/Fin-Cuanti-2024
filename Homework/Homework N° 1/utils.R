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
      title = "Brownian motion path simulation",
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
# a <- floop_gbm(n = 10,T = 1,r = 0.065,sigma = 0.45,S.1 = 100)
# a$p
