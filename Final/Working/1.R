library(quantmod)
library(blotter)
library(FinancialInstrument)
library(dplyr)
library(purrr)
library(tseries)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)
# Generate synthetic data for 4 pairs of cointegrated series
generate_cointegrated_pair <- function(length, drift1 = 0, drift2 = 0, sd1 = 1, sd2 = 1) {
  # Generate two random walks
  x <- cumsum(rnorm(length, mean = drift1, sd = sd1))
  y <- cumsum(rnorm(length, mean = drift2, sd = sd2))
  
  # Generate a linear combination to create a spread that is stationary
  spread <- x - y
  
  # Adjust the second series to be cointegrated with the first
  y <- y + spread
  
  return(data.frame(x = x, y = y))
}
# Define length of the time series
length <- 1000
# Generate four pairs of cointegrated series
pair1 <- generate_cointegrated_pair(length)
pair2 <- generate_cointegrated_pair(length)
pair3 <- generate_cointegrated_pair(length)
pair4 <- generate_cointegrated_pair(length)
# Combine the pairs into one data frame
synthetic_data <- data.frame(
  Date = seq.Date(from = as.Date("2020-01-01"), by = "days", length.out = length),
  StockA = pair1$x,
  StockB = pair1$y,
  StockC = pair2$x,
  StockD = pair2$y,
  StockE = pair3$x,
  StockF = pair3$y,
  StockG = pair4$x,
  StockH = pair4$y
)
# Check for cointegration
adf.test(pair1$x - pair1$y)$p.value  # Should be low indicating stationary spread
adf.test(pair2$x - pair2$y)$p.value
adf.test(pair3$x - pair3$y)$p.value
adf.test(pair4$x - pair4$y)$p.value

# Plot the synthetic data
library(reshape2)

plot_data <- melt(synthetic_data, id.vars = "Date")

# ggplot(plot_data, aes(x = Date, y = value, color = variable)) +
#   geom_line() +
#   labs(title = "Synthetic Cointegrated Pairs",
#        x = "Date",
#        y = "Value") +
#   theme_minimal()

#save(synthetic_data, file = "synthetic_pairs_data.RData")

#I need to extract the synth data columns to the global emvironment

# Extract the synthetic data columns to the global environment as an xts with the date col
synthetic_xts <- xts::xts(synthetic_data[, -1], order.by = synthetic_data$Date)

#get each one to the global environment
for (i in 1:ncol(synthetic_xts)) {
  assign(names(synthetic_xts[,i]), xts::xts(synthetic_xts[, i], order.by = index(synthetic_xts)))
}

#delete all objects that dont begin with Stock
rm(list=ls()[!grepl("^Stock", ls())])

#Get instruments data



Sys.setenv(TZ = "UTC")
currency("USD")
mySymbols <- c('StockA', 'StockB', 'StockC', 'StockD', 'StockE', 'StockF', 'StockG', 'StockH')

currency("USD")

ls_instruments()
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
ls_instruments()#check if working

map(spread_names, ~{
  
  #.x <- spread_names[[1]]
  pair <- unlist(strsplit(.x, "_"))
  spread <- get(pair[1]) - get(pair[2])
  colnames(spread) = .x
  assign(.x, spread, envir = .GlobalEnv)
  
})

save.image(file = "Final/Working/synthetic_data_env.RData")
saveInstruments(file = "Final/Working/synthetic_instruments.RData")
