# Install and load necessary packages
library(quantmod)
library(blotter)
library(FinancialInstrument)

# Ensure new environment is clean
.blotter <- new.env()
.strategy <- new.env()

# Set the timezone to avoid potential issues with date/time
Sys.setenv(TZ = "UTC")

# Initialize the environment
currency("USD")

# Define instruments
stock("NFLX", currency = "USD")
stock("AMZN", currency = "USD")

rm("account.myPortfolio",pos=.blotter)
rm("portfolio.myPortfolio",pos=.blotter)

# Initialize portfolio and account
initPortf(name = "myPortfolio", symbols = c("NFLX", "AMZN"), currency = "USD")
initAcct(name = "myAccount", portfolios = "myPortfolio", initEq = 100000, currency = "USD")
initOrders(portfolio = "myPortfolio")


ls(envir=.blotter)
myAccount <- getAccount("myAccount")
myPortafolio <- getPortfolio("myPortfolio")


# Download historical stock price data for NFLX and AMZN
getSymbols(c("NFLX", "AMZN"), from = "2020-01-01", to = "2020-02-01", src = 'yahoo')

# Define transaction date and quantity
txn_date <- "2020-01-02"
nflx_qty <- 10
amzn_qty <- 10

# Get the closing price on the transaction date
nflx_price <- as.numeric(Cl(NFLX[txn_date]))
amzn_price <- as.numeric(Cl(AMZN[txn_date]))


# Add transactions to the portfolio
addTxn(Portfolio = "myPortfolio", Symbol = "NFLX", TxnDate = txn_date, TxnPrice = nflx_price, TxnQty = nflx_qty, TxnFees = 0)
addTxn(Portfolio = "myPortfolio", Symbol = "AMZN", TxnDate = txn_date, TxnPrice = amzn_price, TxnQty = amzn_qty, TxnFees = 0)


myAccount <- getAccount("myAccount")
myPortafolio <- getPortfolio("myPortfolio")


# Update portfolio and account
updatePortf(Portfolio = "myPortfolio")
updateAcct(name = "myAccount")
updateEndEq(Account = "myAccount")

myAccount <- getAccount("myAccount")
myPortafolio <- getPortfolio("myPortfolio")


# Extract portfolio equity
portf_returns <- PortfReturns(Account = "myAccount")
cum_portf_returns <- cumsum(portf_returns)


dailyEqPL(Portfolios = "myPortfolio")

portfolio.Symbols <- myPortafolio$symbols
View(portfolio.Symbols)

# Plot cumulative returns
# dev.new()
# plot(cum_portf_returns, main = "Cumulative Portfolio Returns", xlab = "Date", ylab = "Cumulative Returns", col = "blue", lwd = 2)
# 
# dev.new()
# plot(Cl(NFLX))
# 
# dev.new()
# plot(Cl(AMZN))

chart.Posn("myPortfolio", Symbol = "NFLX")
chart.Posn("myPortfolio", Symbol = "AMZN")

