# Load necessary libraries
library(quantstrat)

# Load the backtest environment
load("backtest_env.RData")

# Load pairs data
load("mypairs.RData")

# Function to define the strategy
define_strategy <- function(strategy.st, z_threshold = 2, orderqty = 100) {
  add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)[, 1]), n = 10), label = "nFast")
  add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)[, 1]), n = 30), label = "nSlow")
  
  add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "z_score", relationship = "gt", threshold = z_threshold), label = "sell_spread")
  add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "z_score", relationship = "lt", threshold = -z_threshold), label = "buy_spread")
  
  add.rule(strategy.st, name = 'ruleSignal', arguments = list(sigcol = "buy_spread", sigval = TRUE, ordertype = "market", orderside = "long", orderqty = orderqty), type = "enter", label = "EnterLONG")
  add.rule(strategy.st, name = 'ruleSignal', arguments = list(sigcol = "sell_spread", sigval = TRUE, ordertype = "market", orderside = "short", orderqty = orderqty), type = "enter", label = "EnterSHORT")
  
  add.rule(strategy.st, name = 'ruleSignal', arguments = list(sigcol = "buy_spread", sigval = FALSE, ordertype = "market", orderside = "short", orderqty = "all"), type = "exit", label = "ExitSHORT")
  add.rule(strategy.st, name = 'ruleSignal', arguments = list(sigcol = "sell_spread", sigval = FALSE, ordertype = "market", orderside = "long", orderqty = "all"), type = "exit", label = "ExitLONG")
}

# Define the strategy
define_strategy(env$strategy.st)
save(env, file = "backtest_env.RData")
