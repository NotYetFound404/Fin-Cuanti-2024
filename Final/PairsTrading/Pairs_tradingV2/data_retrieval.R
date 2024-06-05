# Load necessary libraries
library(quantmod)

# Function to retrieve stock data
get_stock_data <- function(symbols, from, to) {
  stock_data <- lapply(symbols, function(x) {
    getSymbols(x, from = from, to = to, periodicity = "daily", auto.assign = FALSE)
  })
  names(stock_data) <- symbols
  
  close_prices <- lapply(stock_data, Cl)
  close_prices <- do.call(merge, close_prices)
  names(close_prices) <- sub("\\.Close", "", names(close_prices))
  
  return(close_prices)
}

# Retrieve data
mySymbols <- c('NFLX', 'AMZN')
from <- "2020-01-01"
to <- "2021-01-03"
closePrices <- get_stock_data(mySymbols, from, to)

# Save the close prices to an RData file
save(closePrices, file = "closePrices.RData")
