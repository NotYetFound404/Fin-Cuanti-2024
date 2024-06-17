# Load necessary libraries
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(dplyr)
library(purrr)
library(tseries)

# Ensure new environment is clean
.blotter <- new.env()

# Set the timezone to avoid potential issues with date/time
Sys.setenv(TZ = "UTC")

# Initialize the environment
currency("USD")

# Define synthetic stock symbols
mySymbols <- c('Stock_A', 'Stock_B', 'Stock_C', 'Stock_D', 'Stock_E', 'Stock_F', 'Stock_G', 'Stock_H')

# Set up instruments
map(mySymbols, ~stock(.x, currency = "USD", multiplier = 1))

#Parameters.............
# Define parameters
recalculation_period <- 60
volatility_window <- 45
transaction_costs <- 0
max_drawdown <- 0.10
take_profit <- 0.05
initial_equity <- 100000

#load data and preparation................
# Load the synthetic data
load(file = "Final/Working/synthetic_pairs_data.RData", envir = .GlobalEnv)


#Pairs identification and spread calc...........
# Function to identify pairs and calculate statistical metrics
identify_pairs <- function(data_period, symbols) {
  
  symbols = mySymbols
  
  spread_names <- expand.grid(symbols, symbols) %>%
    filter(Var1 != Var2) %>%
    mutate(spread = paste(Var1, Var2, sep = "-")) %>%
    select(spread) %>%
    pull() %>%
    unique()
  
  pairsStatInfo <- map_df(spread_names, ~{
    x <- .x
    pairs <- unlist(strsplit(x, "-"))
    # CL.log.left <- Cl(get(pairs[[1]]))
    # CL.log.right <- Cl(get(pairs[[2]]))
    CL.log.left <- get(pairs[[1]])[data_period]
    CL.log.right <- get(pairs[[2]])[data_period]
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
  significantPairs <- pairsStatInfo %>% filter(pvalue < 0.05)
  significantPairs$sorted_pair <- apply(significantPairs[, c("left_side", "right_side")], 1, function(x) paste(sort(x), collapse = "_"))
  significantPairs <- significantPairs[!duplicated(significantPairs$sorted_pair), ]
  #debug
  print(data_period)
  print(significantPairs$pair_name)
  
  significantPairs <- significantPairs %>% rowwise() %>% mutate(
    #spread = list(Cl(get(left_side)) - beta * Cl(get(right_side))),
    spread = list(get(left_side) - beta * get(right_side)),
    z_score = list((spread - mean(spread)) / sd(spread)),
    z_scoreVolatility = sd(z_score)
  )
  return(significantPairs)
  
}

# Function to process trading signals
process_trading_signals <- function(pair, portfolio_name, threshold, data, transaction_costs) {
  pair <- first(significantPairs)
  pair_name <- pair$pair_name
  left_side <- get(pair$left_side)
  right_side <- get(pair$right_side)
  z_score <- unlist(pair$z_score)
  volatility <- pair$z_scoreVolatility
  
  
  long_signal <- rep(0, length(z_score))
  short_signal <- rep(0, length(z_score))
  
  for (i in 1:length(z_score)) {
    if (z_score[i] > threshold) {
      short_signal[i] <- 1
      addTxn(Portfolio = portfolio_name,
             Symbol = pair_name,
             TxnDate = index(left_side)[i],
             TxnPrice = Cl(left_side)[i] - Cl(right_side)[i],
             TxnQty = -1,
             TxnFees = transaction_costs)
    }
    if (z_score[i] < -threshold) {
      long_signal[i] <- 1
      addTxn(Portfolio = portfolio_name,
             Symbol = pair_name,
             TxnDate = index(left_side)[i],
             TxnPrice = Cl(left_side)[i] - Cl(right_side)[i],
             TxnQty = 1,
             TxnFees = transaction_costs)
    }
  }
}

#Backtesting Loop.............
# Initialize portfolio and account
initPortf(name = "myPortfolio", symbols = c("Stock_A", "Stock_B", "Stock_C", "Stock_D", "Stock_E", "Stock_F", "Stock_G", "Stock_H"), currency = "USD")
initAcct(name = "myAccount", portfolios = "myPortfolio", initEq = initial_equity, currency = "USD")

# Backtesting loop
start_date = "2020-01-01"
end_date = "2022-09-26"

data_period = "2020-01-01/2020-02-29"
end_Eq = getEndEq(Account = "myAccount", Date = end_date)

significantPairs <- identify_pairs(data_period, mySymbols) #prints period and selected pairs

#Max position for each pair acording to the volatility in this period
significantPairs <- significantPairs %>% mutate(
  max_position = z_scoreVolatility/sum(significantPairs$z_scoreVolatility)
)

#trading for the first pair
pair <- first(significantPairs)
pair_name <- pair$pair_name
left_side <- get(pair$left_side)
right_side <- get(pair$right_side)
z_score <- unlist(pair$z_score)
max_position <- pair$max_position
z_scoreVolatility <- pair$z_scoreVolatility


scaled_ratio_when_low_volatility = 0.7
z_score_for_high_volatility = 1
if ( z_scoreVolatility <= 1) {
  threshold = z_scoreVolatility*scaled_ratio_when_low_volatility
} else {
  threshold = z_score_for_high_volatility
}


portfolio_name = "myPortfolio"

long_signal <- rep(0, length(z_score))
short_signal <- rep(0, length(z_score))

for (i in 1:length(z_score)) {
  #price = Cl(left_side)[i] - Cl(right_side)[i]
  price = (left_side)[i] - (right_side)[i]
  quantity = round(max_position/price)
  
  if (z_score[i] > threshold) {
    short_signal[i] <- 1
    addTxn(Portfolio = portfolio_name,
           Symbol = pair_name,
           TxnDate = index(left_side)[i],
           TxnPrice = price,
           TxnQty = -quantity,
           TxnFees = transaction_costs)
  }
  if (z_score[i] < -threshold) {
    long_signal[i] <- 1
    addTxn(Portfolio = portfolio_name,
           Symbol = pair_name,
           TxnDate = index(left_side)[i],
           TxnPrice = price,
           TxnQty = quantity,
           TxnFees = transaction_costs)
  }
}
}