#------------------
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
#-----------------------
## Install and load necessary packages
library(quantmod)
library(blotter)
library(FinancialInstrument)

library(dplyr)
library(purrr)


#----------------
# Ensure new environment is clean
.blotter <- new.env()


# Set the timezone to avoid potential issues with date/time
Sys.setenv(TZ = "UTC")
# Initialize the environment
currency("USD")

#Define which symbols we are going to use for the spread
mySymbols <- c('NFLX', 'AMZN', 'GOOGL', 'SPY')

# Set up instruments
#1. Set up currency
currency("USD")
#2. Define instruments using map
map(mySymbols, ~stock(.x, currency = "USD", multiplier = 1)) #use stock function for each symbol
ls_instruments()#check if working
#3. Define spreads
spread_names <- expand.grid(mySymbols, mySymbols) %>%
  filter(Var1 != Var2) %>%
  mutate(spread = paste(Var1, Var2, sep = "_")) %>%
  select(spread) %>%
  pull() %>%
  unique()
map(spread_names, ~spread(.x, "USD", unlist(strsplit(.x, "_")), c(1, -1))) #use spred function for each pair

#checks
#ls_instruments()#check if working
#getInstrument(ls_instruments()[1])
# all(spread_names %in% ls_instruments()) #check if all spreads are in the instruments list
# all(mySymbols %in% ls_instruments()) #check if all symbols are in the instruments list

# Remove existing portfolio and account if they exist
rm("account.myPortfolio", pos = .blotter)
rm("portfolio.myPortfolio", pos = .blotter)

# Initialize portfolio and account
initPortf(name = "myPortfolio", symbols = c("NFLX", "AMZN", "NFLX_AMZN"), currency = "USD")
initAcct(name = "myAccount", portfolios = "myPortfolio", initEq = 100000, currency = "USD")

# Check the environment for objects
ls(envir = .blotter)


# Retrieve the account and portfolio
myAccount <- getAccount("myAccount")
myPortfolio <- getPortfolio("myPortfolio")


# parameters
from <- "2020-07-01"
to <- "2020-11-13"

#Load data to global environment.
#1. Load individual stocks data
getSymbols(mySymbols, src = 'yahoo', from = from, to = to)
# Align symbols
alignSymbols(mySymbols)
#2. Load spreads data
#Using map and run quietly
map(spread_names, ~{
  pair <- unlist(strsplit(.x, "_"))
  spread <- get(pair[1]) - get(pair[2])
  assign(.x, spread, envir = .GlobalEnv)
})

library(tseries)

#Calculate the spreads beta, correlation and pvalue
pairsStatInfo <- map_df(spread_names, ~{
  x <- .x
  pairs <- unlist(strsplit(x, "_"))
  CL.log.left <- Cl(get(pairs[[1]]))
  CL.log.right <- Cl(get(pairs[[2]]))
  correlation <- cor(CL.log.left, CL.log.right)[[1]] #run correlation
  m <-  lm(CL.log.left ~ CL.log.right - 1) #run regression withouth intercept
  beta <- coef(m)[[1]] #get beta
  sprd<-residuals(m) #get residuals from the regression
  pvalue<- adf.test(sprd, alternative="stationary", k=0)$p.value #perform ADF test
  data.frame(
    pair_name = x,
    left_side = pairs[[1]],
    right_side = pairs[[2]],
    correlation = correlation,
    beta = beta,
    pvalue = pvalue
  )
})
significantPairs <- pairsStatInfo %>% filter(pvalue < 0.05) # select significant pairs
significantPairs$sorted_pair <- apply(significantPairs[, c("left_side", "right_side")], 1, function(x) paste(sort(x), collapse = "_")) #"sort" the pairs
#test eliminar luego
significantPairs <- significantPairs[2:3,]
significantPairs <- significantPairs[!duplicated(significantPairs$sorted_pair), ]# Remove duplicate pairs



#Start trading the significant pairs
#First Calculate the spread and zscore from the significant pairs
# Calculate spread and z-score

#Spread and zscore of significant pairs
pairs.Spread <- significantPairs %>% rowwise() %>% mutate(
  spread = list(Cl(get(left_side)) - beta * Cl(get(right_side))),
  z_score = list((spread - mean(spread)) / sd(spread))
)


# Define thresholds for trading signals
threshold <- 1



#Function that process the singlas
process_trading_signals <- function(x) {
  portfolio.name = "myPortfolio"
  
  pair.name <- x$pair_name
  left.pair <- get(x$left_side)
  right.pair <- get(x$right_side)
  z_score <- unlist(x$z_score)
  
  # Initialize signal vectors
  long_signal <- rep(0, length(z_score))
  short_signal <- rep(0, length(z_score))
  
  # Generate signals based on z-score
  for (i in 1:length(z_score)) {
    if (z_score[i] > threshold) {
      short_signal[i] <- 1  # Short signal
      addTxn(Portfolio = portfolio.name,
             Symbol = pair.name,
             TxnDate = index(left.pair)[i],
             TxnPrice = Cl(left.pair)[i] - Cl(right.pair)[i],
             TxnQty = -1,
             TxnFees = 0)
    }
    if (z_score[i] < -threshold) {
      long_signal[i] <- 1  # Long signal
      addTxn(Portfolio = portfolio.name,
             Symbol = pair.name,
             TxnDate = index(left.pair)[i],
             TxnPrice = Cl(left.pair)[i] - Cl(right.pair)[i],
             TxnQty = 1,
             TxnFees = 0)
    }
  }
}

#Process the signals
apply(pairs.Spread, 1, process_trading_signals)


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
#pts <- perTradeStats("myPortfolio", Symbol = "NFLX_AMZN")
#dailatTxpl <- dailyTxnPL(Portfolios = "myPortfolio", Symbol = "NFLX_AMZN")
getEndEq("myAccount", Date = "2020-11-13")

