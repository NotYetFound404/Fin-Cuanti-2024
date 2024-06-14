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

# Remove existing portfolio and account if they exist
rm("account.myPortfolio", pos = .blotter)
rm("portfolio.myPortfolio", pos = .blotter)

# Initialize portfolio and account
initPortf(name = "myPortfolio", symbols = c("NFLX", "AMZN"), currency = "USD")
initAcct(name = "myAccount", portfolios = "myPortfolio", initEq = 100000, currency = "USD")
initOrders(portfolio = "myPortfolio")

# Check the environment for objects
ls(envir = .blotter)

# Retrieve the account and portfolio
myAccount <- getAccount("myAccount")
myPortfolio <- getPortfolio("myPortfolio")

# Download historical stock price data for NFLX and AMZN
getSymbols(c("NFLX", "AMZN"), from = "2020-01-01", to = "2020-02-01", src = 'yahoo')

# Define transaction date and quantity for NFLX (short sell)
txn_date <- "2020-01-02"
nflx_qty <- -10  # Negative quantity for short selling

# Get the closing price on the transaction date for NFLX
nflx_price <- as.numeric(Cl(NFLX[txn_date]))

# Add the short sell transaction to the portfolio for NFLX
addTxn(Portfolio = "myPortfolio", Symbol = "NFLX", TxnDate = txn_date, TxnPrice = nflx_price, TxnQty = nflx_qty, TxnFees = 0)

# Retrieve the updated account and portfolio
myAccount <- getAccount("myAccount")
myPortfolio <- getPortfolio("myPortfolio")

# Update portfolio and account to reflect the transactions
updatePortf(Portfolio = "myPortfolio")
updateAcct(name = "myAccount")
updateEndEq(Account = "myAccount")

# Retrieve the updated account and portfolio again
myAccount <- getAccount("myAccount")
myPortfolio <- getPortfolio("myPortfolio")

# Extract portfolio equity returns and calculate cumulative returns
portf_returns <- PortfReturns(Account = "myAccount")
cum_portf_returns <- cumsum(portf_returns)

# Calculate daily equity PL
dailyEqPL(Portfolios = "myPortfolio")

# View the symbols in the portfolio
portfolio.Symbols <- myPortfolio$symbols
View(portfolio.Symbols)

# Plot cumulative returns
dev.new()
plot(cum_portf_returns, main = "Cumulative Portfolio Returns", xlab = "Date", ylab = "Cumulative Returns", col = "blue", lwd = 2)

# Plot the closing prices of NFLX
dev.new()
plot(Cl(NFLX))

# Plot the positions in the portfolio for NFLX
chart.Posn("myPortfolio", Symbol = "NFLX")