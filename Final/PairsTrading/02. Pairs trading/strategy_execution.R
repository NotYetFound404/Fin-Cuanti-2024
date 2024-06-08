# Load necessary libraries
library(quantstrat)
library(PerformanceAnalytics)

# Load the backtest environment
load("backtest_env.RData")

# Function to execute the strategy and analyze results
execute_strategy <- function(strategy.st, portfolio.st, account.st) {
  applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
  
  updatePortf(portfolio.st)
  updateAcct(account.st)
  updateEndEq(account.st)
  
  chart.Posn(portfolio.st, Symbol = "NFLX")
  
  tStats <- tradeStats(Portfolios = portfolio.st, use = "trades", inclZeroDays = FALSE)
  knitr::kable(t(tStats))
  
  rets <- PortfReturns(Account = account.st)
  rownames(rets) <- NULL
  charts.PerformanceSummary(rets, colorset = bluefocus)
}

# Execute and analyze the strategy
execute_strategy(env$strategy.st, env$portfolio.st, env$account.st)
