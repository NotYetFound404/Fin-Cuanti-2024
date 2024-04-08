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
