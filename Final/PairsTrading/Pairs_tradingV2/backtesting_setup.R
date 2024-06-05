# Load necessary libraries
library(quantstrat)

# Function to set up the backtesting environment
setup_backtest_env <- function(initDate, from, to, initEq, symbols) {
  currency("USD")
  for (symbol in symbols) {
    stock(symbol, currency = "USD", multiplier = 1)
  }
  Sys.setenv(TZ = "UTC")
  
  strategy.st <- "pairstrading"
  portfolio.st <- "pairstrading"
  account.st <- "pairstrading"
  
  initPortf(portfolio.st, symbols = symbols, initDate = initDate)
  initAcct(account.st, portfolios = portfolio.st, initDate = initDate, initEq = initEq)
  initOrders(portfolio.st, initDate = initDate)
  strategy(strategy.st, store = TRUE)
  
  return(list(strategy.st = strategy.st, portfolio.st = portfolio.st, account.st = account.st))
}

# Setup backtesting environment
initDate <- "2020-01-02"
from <- "2020-01-03"
to <- "2021-01-03"
initEq <- 100000
mySymbols <- c('NFLX', 'AMZN')

env <- setup_backtest_env(initDate, from, to, initEq, mySymbols)
save(env, file = "backtest_env.RData")
