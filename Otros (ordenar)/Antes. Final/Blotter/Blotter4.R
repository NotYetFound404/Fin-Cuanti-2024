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

# Define the spread instrument (NFLX - AMZN)
spread('NFLX_AMZN', 'USD', c('NFLX', 'AMZN'), c(1, -1))


# Remove existing portfolio and account if they exist
rm("account.myPortfolio", pos = .blotter)
rm("portfolio.myPortfolio", pos = .blotter)

# Initialize portfolio and account
initPortf(name = "myPortfolio", symbols = c("NFLX", "AMZN", "NFLX_AMZN"), currency = "USD")
initAcct(name = "myAccount", portfolios = "myPortfolio", initEq = 100000, currency = "USD")
initOrders(portfolio = "myPortfolio")

# Check the environment for objects
ls(envir = .blotter)

# Retrieve the account and portfolio
myAccount <- getAccount("myAccount")
myPortfolio <- getPortfolio("myPortfolio")

myPortfolio$symbols$NFLX_AMZN
getInstrument("NFLX_AMZN")


# Download historical stock price data for NFLX and AMZN
getSymbols(c("NFLX", "AMZN"), from = "2020-01-01", to = "2020-02-01", src = 'yahoo')
NFLX_AMZN <- NFLX - AMZN

# Define transaction date and quantity for NFLX (short sell)
txn_date <- "2020-01-02"

# Get the closing prices of NFLX and AMZN on the transaction date
nflx_price <- Cl(NFLX[txn_date])
amzn_price <- Cl(AMZN[txn_date])

spread_price <- Cl(NFLX_AMZN[txn_date])


#solo el spread
addTxn(Portfolio = "myPortfolio",
       Symbol = "NFLX_AMZN",
       TxnDate = txn_date,
       TxnPrice = spread_price,
       TxnQty = 1,
       TxnFees = 0)

#por separado
addTxn(Portfolio = "myPortfolio",
       Symbol = "NFLX",
       TxnDate = txn_date,
       TxnPrice = nflx_price,
       TxnQty = 1,
       TxnFees = 0)

addTxn(Portfolio = "myPortfolio",
       Symbol = "AMZN",
       TxnDate = txn_date,
       TxnPrice = amzn_price,
       TxnQty = -1,
       TxnFees = 0)



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

dailyPL <- dailyEqPL(Portfolios = "myPortfolio")
dailyPL <- as.data.frame(dailyPL) |> dplyr::mutate(spread_sintetico = NFLX.DailyEqPL + AMZN.DailyEqPL)
dailyPL

# Extract portfolio equity returns and calculate cumulative returns
portf_returns <- PortfReturns(Account = "myAccount")
cum_portf_returns <- cumsum(portf_returns)

# Calculate daily equity PL
dailyStats(Portfolios = "myPortfolio")


# View the symbols in the portfolio
portfolio.Symbols <- myPortfolio$symbols
View(portfolio.Symbols)

# Plot cumulative returns
dev.new()
plot(cum_portf_returns, main = "Cumulative Portfolio Returns", xlab = "Date", ylab = "Cumulative Returns", col = "blue", lwd = 2)

# Plot the positions in the portfolio for NFLX
chart.Posn("myPortfolio", Symbol = "NFLX_AMZN", theme = chart_theme())

plot(Cl(NFLX_AMZN))
