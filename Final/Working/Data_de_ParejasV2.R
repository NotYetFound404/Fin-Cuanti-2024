library(quantmod)
library(blotter)
library(FinancialInstrument)
library(dplyr)
library(purrr)
library(tseries)
library(ggplot2)

from <- "2020-07-01"
to <- "2020-11-13"

mySymbols <- c('GOOGL', 'TSLA', 'AMZN', 'AAPL', 'MSFT', 'VOD',  'ADBE', 'NVDA', 'CRM',
              'EBAY')
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
getSymbols(mySymbols, src = 'yahoo', from = from, to = to)
map(spread_names, ~{
  pair <- unlist(strsplit(.x, "_"))
  spread <- get(pair[1]) - get(pair[2])
  #colnames(spread) = .x
  assign(.x, spread, envir = .GlobalEnv)
  
})

save.image(file = "Final/Working/realDataEnv.Rdata")
saveInstruments(file = "Final/Working/realDataInstruments.RData")
