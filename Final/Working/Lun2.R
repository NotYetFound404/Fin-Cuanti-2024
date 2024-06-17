# Libraries
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(dplyr)
library(purrr)
library(tseries)
library(ggplot2)
library(quantstrat)

# Load data and symbols
load(file = "Final/Working/realDataEnv.Rdata")
load.instruments(file = "Final/Working/realDataInstruments.RData")

# Set timezone
.blotter <- new.env(); .strategy <- new.env(); Sys.setenv(TZ="UTC") #setting up the timezone
Sys.setenv(TZ="UTC")

# Symbols and currency
symbols = c("EBAY_AAPL", "CRM_AMZN", "AAPL_NVDA")
currency = "USD"

# Initial equity and dates
initEq <- 100000
initDate <- "1999-01-01"
from <- "2020-07-01"
to <- "2020-11-13"

# Strategy, portfolio, and account names
strategy.st <- portfolio.st <- account.st <- "firststrat"
rm.strat(strategy.st)

# Initialize portfolio, account, and orders
initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = currency)
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = currency, initEq = initEq)
initOrders(portfolio.st, initDate = initDate)
#strategy(strategy.st, store = TRUE)

# Parameters
cash_reserve_percentage <- 0.1
max_capital_per_day_percentage <- 0.5


#funciones
get_positionsAtCurrentDay <- function(symbols, portfolio.st, today){
  positions_at_day <- lapply(symbols, function(symb) {
    getPos(Portfolio = portfolio.st, Symbol = symb, Date = dias.fecha[today])
  })
  names(positions_at_day) <- symbols
  return(positions_at_day)
}

get_CashDistribuido <- function(symbols, cash_for_trading, volatilidad_al_dia_de_hoy, señales_el_dia_de_hoy){
  capital_distribuido <- lapply(symbols, function(symb) {
    if (symb %in% señales_el_dia_de_hoy$symbolos_compra) {
      cash_for_trading * volatilidad_al_dia_de_hoy[symb] / sum(volatilidad_al_dia_de_hoy)
    } else {
      0
    }
  })
  names(capital_distribuido) <- symbols
  return(capital_distribuido)
}
execute_orders <- function(symbols, capital_distribuido, today){
  for(symbol in symbols){
    if(capital_distribuido[[symbol]] > 0){
      price = Cl(get(symbol)[today])[[1]]
      order_qty <- abs(round(capital_distribuido[[symbol]]/price, 0))[[1]]
      if(order_qty > 0){
        addTxn(Portfolio = portfolio.st, Symbol = symbol, TxnDate = dias.fecha[today], TxnPrice = price, TxnQty = order_qty, TxnFees = 0)
      }
    }
  }
}
get_cashForTrading <- function(){
  equity <- getEndEq(account.st, dias.fecha[today])
  total_cash_disponible_hoy <- equity - sum(sapply(positions_at_day, function(x) x$Pos.Qty[[1]] * x$Pos.Avg.Cost[[1]]))
  cash_reserve <- total_cash_disponible_hoy * cash_reserve_percentage
  max_capital_per_day <- total_cash_disponible_hoy * max_capital_per_day_percentage
  cash_for_trading <- min(total_cash_disponible_hoy - cash_reserve, max_capital_per_day)
  cash_for_trading <- max(0, cash_for_trading)
  return(cash_for_trading)
}
# Dates and signals
dias.fecha <- index(get(symbols[1]))
dia <- c(11, 11)
symbolos_compra <- c(symbols[1], symbols[2])
señales <- data.frame(dia = dia, symbolos_compra = symbolos_compra)


# Volatility and signals for today
volatilidad_al_dia_de_hoy <- c(10, 7.8, 2)
names(volatilidad_al_dia_de_hoy) <- symbols
señales_el_dia_de_hoy <- señales[señales$dia == today,]
volatilidad_al_dia_de_hoy
señales_el_dia_de_hoy

#loop...................
# Current day (first day)
today <- 11

# Get positions and equity
# positions_at_day <- lapply(symbols, function(symb) {
#   getPos(Portfolio = portfolio.st, Symbol = symb, Date = dias.fecha[today])
# })
# names(positions_at_day) <- symbols

portfolio <- getPortfolio(Portfolio = portfolio.st)
rets <- try(PortfReturns(Account = account.st))

#1. revisar que posiciones tengo abiertas
positions_at_day <- get_positionsAtCurrentDay(symbols, portfolio.st, today)
#2. obtener el total de cash para el trading
# equity <- getEndEq(account.st, dias.fecha[today])
# total_cash_disponible_hoy <- equity - sum(sapply(positions_at_day, function(x) x$Pos.Qty[[1]] * x$Pos.Avg.Cost[[1]]))
# cash_reserve <- equity * cash_reserve_percentage; max_capital_per_day <- equity * max_capital_per_day_percentage; # Calculate cash reserve and maximum capital per day
# cash_for_trading <- min(total_cash_disponible_hoy - cash_reserve, max_capital_per_day); cash_for_trading <- max(0, cash_for_trading)# Calculate cash available for trading
cash_for_trading <- get_cashForTrading()
#3. obtener la volatilidad de hoy
volatilidad_al_dia_de_hoy
#4. distribuir el capital segun la volatilidad y capital para trading
# # Distribute capital based on signals and volatility
# capital_distribuido <- lapply(symbols, function(symb) {
#   if (symb %in% señales_el_dia_de_hoy$symbolos_compra) {
#     cash_for_trading * volatilidad_al_dia_de_hoy[symb] / sum(volatilidad_al_dia_de_hoy)
#   } else {
#     0
#   }
# })
# names(capital_distribuido) <- symbols
# capital_distribuido
capital_distribuido = get_CashDistribuido(symbols, cash_for_trading, volatilidad_al_dia_de_hoy, señales_el_dia_de_hoy)

#5. Ejecutar las ordenes segun el capital por distribuir y las señales
execute_orders(symbols, capital_distribuido, today)

# for(symbol in symbols){
#   if(capital_distribuido[[symbol]] > 0){
#     price = Cl(get(symbol)[today])[[1]]
#     order_qty <- abs(round(capital_distribuido[[symbol]]/price, 0))[[1]]
#     if(order_qty > 0){
#       addTxn(Portfolio = portfolio.st, Symbol = symbol, TxnDate = dias.fecha[today], TxnPrice = price, TxnQty = order_qty, TxnFees = 0)
#     }
#   }
# }
#6.  Update portfolio, account, and equity
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)


#loop 2
#dia siguiente se activa la liquidación de posiciones para un activo
portfolio <- getPortfolio(Portfolio = portfolio.st)
rets <- PortfReturns(Account = account.st)
today <- 12
todays_returns <- rets[today]

threshold = 0.01
take_profit <- function(today, threshold) {
  for (symbol in symbols) {
    #symbol = symbols[[2]]
    # Get today's return for the symbol
    col.name = paste0(symbol, ".DailyEqPL")
    col.pos = which(colnames(todays_returns) == col.name)
    daily_return <- sum(todays_returns[,col.pos])
    # Get the current position
    position <- getPos(Portfolio = portfolio.st, Symbol = symbol, Date = dias.fecha[today])
    if (!is.na(daily_return) && length(position$Pos.Qty) != 0 && daily_return >= threshold) {
        # Liquidate the position
        order_qty <- -sum(position$Pos.Qty) # Sell all
        price <- Cl(get(symbol)[today])[[1]]
        addTxn(Portfolio = portfolio.st, Symbol = symbol, TxnDate = dias.fecha[today], TxnPrice = price, TxnQty = order_qty, TxnFees = 0)
    }
  }
}
stop_loss <- function(today, threshold) {
  for (symbol in symbols) {
    #symbol = symbols[[2]]
    # Get today's return for the symbol
    col.name = paste0(symbol, ".DailyEqPL")
    col.pos = which(colnames(todays_returns) == col.name)
    daily_return <- sum(todays_returns[,col.pos])
    # Get the current position
    position <- getPos(Portfolio = portfolio.st, Symbol = symbol, Date = dias.fecha[today])
    if (!is.na(daily_return) && length(position$Pos.Qty) != 0 && daily_return <= threshold) {
      # Liquidate the position
      order_qty <- -sum(position$Pos.Qty) # Sell all
      price <- Cl(get(symbol)[today])[[1]]
      addTxn(Portfolio = portfolio.st, Symbol = symbol, TxnDate = dias.fecha[today], TxnPrice = price, TxnQty = order_qty, TxnFees = 0)
    }
  }
}
take_profit(today = today, threshold = threshold)
stop_loss(today = today, threshold = -0.001)

#6.  Update portfolio, account, and equity
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)



portfolio <- getPortfolio(Portfolio = portfolio.st)
rets <- try(PortfReturns(Account = account.st)) #esto esta mal?
#1. revisar que posiciones tengo abiertas
positions_at_day <- get_positionsAtCurrentDay(symbols, portfolio.st, today)
dailyEqPL(Portfolios = portfolio.st)
cash_for_trading <- get_cashForTrading()


# # Get positions and equity
# positions_at_day <- lapply(symbols, function(symb) {
#   getPos(Portfolio = portfolio.st, Symbol = symb, Date = dias.fecha[today])
# })
# names(positions_at_day) <- symbols
# positions_at_day
# equity <- getEndEq(account.st, dias.fecha[today])
#total_cash_disponible_hoy <- equity - sum(sapply(positions_at_day, function(x) x$Pos.Qty[[1]] * x$Pos.Avg.Cost[[1]]))

dailyTxnPL(Portfolios = portfolio.st)
