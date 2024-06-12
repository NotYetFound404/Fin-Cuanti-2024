# Load required libraries
library(quantstrat)
library(quantmod)
#ensure new env is clean
.blotter <- new.env()
.strategy <- new.env()
#setting up the timezone

# Set initial parameters
startDate <- '2018-12-31'
endDate <- '2020-01-01'
initEq <- 100000

# Set up instruments
currency("USD")
symbols <- c("AMZN", "NFLX")
for (symbol in symbols) {
  stock(symbol, currency = "USD", multiplier = 1)
}

# Load data with quantmod
getSymbols(symbols, src = 'yahoo', from = startDate, to = endDate)

# Convert to monthly data
for (symbol in symbols) {
  x <- get(symbol)
  x <- to.monthly(x, indexAt = 'lastof', drop.time = TRUE)
  assign(symbol, x)
}

# Initialize portfolio, account, and orders
initPortf('correlationPortf', symbols = symbols, initDate = startDate)
initAcct('correlationAcct', portfolios = 'correlationPortf', initDate = startDate, initEq = initEq)
initOrders(portfolio = 'correlationPortf', initDate = startDate)

# Initialize a strategy object
strategy("correlationStrategy", store = TRUE)

# Add a custom indicator for correlation
add.indicator(strategy = "correlationStrategy", name = "runCor",
              arguments = list(x = quote(Cl(AMZN)), y = quote(Cl(NFLX)), n = 3), label = "Corr")

# Apply the indicator
tesst<-applyIndicators(strategy = "correlationStrategy", mktdata = OHLC(AMZN))
tesst2<-applyIndicators(strategy = "correlationStrategy", mktdata = OHLC(NFLX))

strat.deb <- getStrategy("correlationStrategy")
indic <- strat.deb$indicators
indic


formals(indic[[1]]$name)





# Add a signal when the correlation is above 0.95
add.signal(strategy = "correlationStrategy", name = "sigThreshold",
           arguments = list(column = "Corr", threshold = 0.95, relationship = "gt", cross = TRUE), label = "CorrAbove95")

# Add a rule to enter long when the correlation signal is triggered
add.rule(strategy = "correlationStrategy", name = "ruleSignal",
         arguments = list(sigcol = "CorrAbove95", sigval = TRUE, orderqty = 100, ordertype = "market", orderside = "long"),
         type = "enter")

# Apply the strategy
applyStrategy(strategy = "correlationStrategy", portfolios = "correlationPortf")

# Update portfolio, account, and calculate performance
updatePortf(Portfolio = "correlationPortf")
updateAcct(name = "correlationAcct")
updateEndEq(Account = "correlationAcct")

# Plot results
for (symbol in symbols) {
  chart.Posn(Portfolio = "correlationPortf", Symbol = symbol, theme = chart_theme())
}

# Print trade statistics
print(tradeStats(Portfolios = "correlationPortf"))
View(.strategy)
test_init <- applyIndicators(correlationAcct, mktdata = OHLC(AMZN))
test <- applySignals(strategy = strategy.st, mktdata = test_init)


chart.Posn(Portfolio = "correlationPortf", Symbol = "AMZN", theme = chart_theme())
chart.Posn(Portfolio = "correlationPortf", Symbol = "NFLX", theme = chart_theme())
instrets <- PortfReturns("correlationPortf")

