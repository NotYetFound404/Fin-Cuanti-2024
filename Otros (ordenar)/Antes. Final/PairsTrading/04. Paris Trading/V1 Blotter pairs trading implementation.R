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
#initOrders(portfolio = "myPortfolio")

# Check the environment for objects
ls(envir = .blotter)

# Retrieve the account and portfolio
myAccount <- getAccount("myAccount")
myPortfolio <- getPortfolio("myPortfolio")

# Download historical stock price data for NFLX and AMZN
getSymbols(c("NFLX", "AMZN"), from = "2020-01-01", to = "2020-02-01", src = 'yahoo')

# Function to align symbols
alignSymbols <- function(symbols, env = .GlobalEnv) {
  if (length(symbols) < 2) 
    stop("Must provide at least 2 symbols")
  if (any(!is.character(symbols))) 
    stop("Symbols must be vector of character strings.")
  ff <- get(symbols[1], env = env)
  for (sym in symbols[-1]) {
    tmp.sym <- get(sym, env = env)
    ff <- merge(ff, tmp.sym, all = FALSE)
  }
  for (sym in symbols) {
    assign(sym, ff[, grep(sym, colnames(ff))], env = env)
  }
  symbols
}

# Symbols of interest
mySymbols <- c('AMZN', 'NFLX')
from <- "2020-07-01"
to <- "2020-11-13"
getSymbols(mySymbols, from = from, to = to, src = "yahoo", adjust = TRUE)

# Align symbols
alignSymbols(mySymbols)

# Calculate log prices
AMZN.cl <- log(Cl(AMZN))
NFLX.cl <- log(Cl(NFLX))

# Calculate correlation
correlation <- cor(AMZN.cl, NFLX.cl)

# Perform regression
m <- lm(AMZN.cl ~ NFLX.cl - 1) 
beta <- coef(m)[[1]]

# Calculate spread and z-score
spread <- AMZN.cl - beta * NFLX.cl
z_score <- (spread - mean(spread)) / sd(spread)

# Define thresholds for trading signals
threshold <- 1

# Initialize signal vectors
long_signal <- rep(0, length(z_score))
short_signal <- rep(0, length(z_score))

# Generate signals based on z-score
for (i in 1:length(z_score)) {
  if (z_score[i] > threshold) {
    short_signal[i] <- 1  # Short signal
    addTxn(Portfolio = "myPortfolio",
           Symbol = "NFLX_AMZN",
           TxnDate = index(NFLX)[i],
           TxnPrice = Cl(NFLX)[i] - Cl(AMZN)[i],
           TxnQty = 1,
           TxnFees = 0)
  }
  if (z_score[i] < -threshold) {
    long_signal[i] <- 1  # Long signal
    addTxn(Portfolio = "myPortfolio",
           Symbol = "NFLX_AMZN",
           TxnDate = index(NFLX)[i],
           TxnPrice = Cl(NFLX)[i] - Cl(AMZN)[i],
           TxnQty = -1,
           TxnFees = 0)
  }
}

NFLX_AMZN <- NFLX - AMZN
# Update portfolio and account to reflect the transactions
updatePortf(Portfolio = "myPortfolio")
updateAcct(name = "myAccount")
updateEndEq(Account = "myAccount")
# Retrieve the updated account and portfolio again
myAccount <- getAccount("myAccount")
myPortfolio <- getPortfolio("myPortfolio")
# Calculate daily equity PL
dailyStats(Portfolios = "myPortfolio")
dailyPL <- dailyEqPL(Portfolios = "myPortfolio")
txns <- getTxns(Portfolio = "myPortfolio", Symbol = "NFLX_AMZN")
#Chart 1
chart.Posn(Portfolio = "myPortfolio", Symbol = "NFLX_AMZN")
#Chart 2
Buys = txns$Txn.Price[which(txns$Txn.Qty > 0)]
Sells = txns$Txn.Price[which(txns$Txn.Qty < 0)]

chart_Series(Cl(NFLX_AMZN))
add_TA(Sells, pch = 6, type = "p", col = "red", on = 1, cex  = 2)
add_TA(Buys, pch = 6, type = "p", col = "green", on = 1,cex  = 2)

#chart 3
z_score_Buys = z_score[index(Buys)] #no se porque el buys debo pintarlo de rojo xd
z_score_Sells = z_score[index(Sells)]

chart_Series(z_score)
add_TA(z_score_Sells, pch = 6, type = "p", col = "green", on = 1, cex  = 2)
add_TA(z_score_Buys, pch = 6, type = "p", col = "red", on = 1,cex  = 2)
abline(h = threshold, col = "red", lty = 2)  # Line for +1 threshold
abline(h = -threshold, col = "green", lty = 2)  # Line for -1 threshold
#stats
tStats <- tradeStats(Portfolios = "myPortfolio")
t(tStats)
port.Summ <- myPortfolio$summary
#returns
rets <- PortfReturns(Account = "myAccount")
rownames(rets) <- NULL
charts.PerformanceSummary(rets, colorset = bluefocus)
pts <- perTradeStats("myPortfolio", Symbol = "NFLX_AMZN")
dailatTxpl <- dailyTxnPL(Portfolios = "myPortfolio", Symbol = "NFLX_AMZN")
getEndEq("myAccount", Date = "2020-11-13")
