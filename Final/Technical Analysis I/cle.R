
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

# Define your trade size and initial equity
tradesize <- 100000
initEq <- 100000

# Retrieve AAPL data from Yahoo Finance
getSymbols(symbols, src = 'yahoo', from = from, to = to, auto.assign = TRUE)

# Use stock() to initialize AAPL and set currency to USD
stock(symbols, currency = currency, multiplier = 1)
currency(currency)  # Set the currency to USD 

# Define the strategy name
strategy.st <- portfolio.st <- account.st <- "BullishEngulfing"
rm.strat("BullishEngulfing")

# Initialize the portfolio, account, and orders
initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = currency)
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = currency, initEq = initEq)
initOrders(portfolio.st, initDate = initDate)
strategy(strategy.st, store = TRUE)

# Function to identify a bullish engulfing pattern
is_bullish_engulfing <- function(data) {
  lagOpen <- Lag(Op(data), 1)  # Lagged open prices
  lagClose <- Lag(Cl(data), 1)  # Lagged close prices
  currentOpen <- Op(data)  # Current open prices
  currentClose <- Cl(data)  # Current close prices
  
  # Condition for bullish engulfing
  return(lagClose < lagOpen & currentOpen < currentClose & currentClose > lagOpen & currentOpen < lagClose)
}

# Add the indicator to your strategy
add.indicator(strategy.st, name = "is_bullish_engulfing",
              arguments = list(data = quote(mktdata)),
              label = "BullishEngulfingSignal")

# Apply the indicators
applyIndicators(strategy.st)

# Define the parameters
stop_loss <- 0.10
stop_gain <- 0.30
capital <- 10000  # Initial capital

# Define the rule for entering long positions
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "BullishEngulfingSignal", sigval = TRUE, orderqty = 100, ordertype = "market", orderside = "long"),
         type = "enter",
         label = "EnterLong")

# Define the rule for exiting long positions based on stop loss and take profit
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "BullishEngulfingSignal", sigval = FALSE, orderqty = "all", ordertype = "market", orderside = "long"),
         type = "exit",
         label = "ExitLongStopLoss",
         replace = FALSE,
         prefer = "Open",
         execFUN = quote(
           if (isTRUE(last_sig_price() * (1 - stop_loss) >= Cl(mktdata))) {
             sigPrice <- last_sig_price() * (1 - stop_loss)
             sigTimestamp <- last_sig_timestamp()
             addTxn(Portfolio = portfolio.st, Symbol = "AAPL", TxnDate = sigTimestamp, TxnPrice = sigPrice, TxnQty = -100, TxnFees = -10)
           } else if (isTRUE(last_sig_price() * (1 + stop_gain) <= Cl(mktdata))) {
             sigPrice <- last_sig_price() * (1 + stop_gain)
             sigTimestamp <- last_sig_timestamp()
             addTxn(Portfolio = portfolio.st, Symbol = "AAPL", TxnDate = sigTimestamp, TxnPrice = sigPrice, TxnQty = -100, TxnFees = -10)
           }
         ))

# Apply the strategy
applyStrategy(strategy.st, portfolios = portfolio.st)

# Update portfolio, account, and calculate performance statistics
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

# Get trade statistics
tstats <- tradeStats(portfolio.st)
tstats$Profit.Factor
tstats$Percent.Positive

# Generate P&L graph
chart.Posn(portfolio.st, Symbol = "AAPL")

