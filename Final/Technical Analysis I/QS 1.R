# Load necessary libraries
library(quantmod)
library(TTR)
library(quantstrat)

# Ensure new environment is clean
.blotter <- new.env()
.strategy <- new.env()

# Set up the timezone
Sys.setenv(TZ = "UTC") 

# Create initdate, from, and to strings
initDate <- "1999-01-01"
from <- "2020-01-01"
to <- "2021-01-01"

# Set stocks and currency
symbols <- c("AAPL")
currency <- "USD"

# Retrieve AAPL data from Yahoo Finance
getSymbols(symbols, src = 'yahoo', from = from, to = to, auto.assign = TRUE)

# Initialize portfolio, account, and strategy
initPortf(name = "portfolio", symbols = symbols, initDate = initDate)
initAcct(name = "account", portfolios = "portfolio", initDate = initDate, initEq = 100000)
initOrders(portfolio = "portfolio", initDate = initDate)

# Define the strategy
strategy.st <- "BullishEngulfing"
add.indicator(strategy = strategy.st, name = "is.bullish.engulfing",
              arguments = list(HLC = quote(HLC(mktdata))), 
              label = "BullishEngulfingSignal")
add.signal(strategy = strategy.st, name = "is.bullish.engulfing",
           arguments = list(HLC = quote(HLC(mktdata))), 
           label = "BullishEngulfingSignal")

# Define the entry rule
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "BullishEngulfingSignal", sigval = TRUE,
                          orderqty = 100, ordertype = "market", orderside = "long"),
         type = "enter", label = "EnterLong")

# Apply the strategy
applyStrategy(strategy.st, portfolios = "portfolio")
