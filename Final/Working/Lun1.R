#--------------
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(dplyr)
library(purrr)
library(tseries)
library(ggplot2)
# Define synthetic stock symbols
#load data and symbols
load(file = "Final/Working/realDataEnv.Rdata")
load.instruments(file = "Final/Working/realDataInstruments.RData")
#--------------
# Load the quantstrat package
library(quantstrat)
#ensure new env is clean
.blotter <- new.env(); .strategy <- new.env(); Sys.setenv(TZ="UTC") #setting up the timezone

#set stocks and currecy
symbols = c("EBAY_AAPL", "CRM_AMZN", "AAPL_NVDA"); currency = "USD"


# Define your trade size and initial equity
initEq <- 100000
# Create initdate, from, and to strings
initDate <- "1999-01-01"
from <- "2020-07-01"
to <- "2020-11-13"
# Define the names of your strategy, portfolio and account
strategy.st <- portfolio.st <- account.st <- "firststrat"; rm.strat("firststrat")# Remove the existing strategy if it exists
# Initialize the portfolio, account, and orders
initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = currency); initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = currency, initEq = initEq); initOrders(portfolio.st, initDate = initDate); strategy(strategy.st, store = TRUE)

# Define los parámetros
cash_reserve_percentage = 0.1
max_capital_per_day_percentage = 0.5

# dailyPL <- dailyTxnPL(Portfolios = portfolio.st, Symbol = symbols[1], Dates = dias.fecha[today])
# dailyEQ.PL.today <- dailyEqPL(Portfolios = portfolio.st, Symbols = symbols[1], Dates = dias.fecha[today])
#getEndEq(account.st, dias.fecha[11])

dias.fecha = index(get(symbols[1]))
#señales
dia <- c(11,11)
symbolos_compra <- c(symbols[1],symbols[2])
señales <- data.frame(cbind(dia, symbolos_compra))
#dia de hoy

today <- 11

#get portfolio end equity
positions_at_day <- list()
for(symb in symbols){
  positions_at_day[[symb]] <- getPos(Portfolio = portfolio.st, Symbol = symb, Date = dias.fecha[today])
}
#positions_at_day
equity <- getEndEq(account.st, dias.fecha[today])
total_cash_disponible_hoy =  equity - sum(unlist(lapply(positions_at_day, function(x) x$pos.qty * x$pos.avg.cost)))
#portfolio <- getPortfolio(Portfolio = portfolio.st, Dates = dias.fecha[today])
portfolio <- getPortfolio(Portfolio = portfolio.st)
pl_for_symbols <- list()
for(symb in symbols){
  pl_for_symbols[[symb]] <- portfolio$symbols[[symb]]$posPL.USD$Period.Unrealized.PL
}
pl_for_symbols
# Calcula la reserva de efectivo requerida
cash_reserve <- equity * cash_reserve_percentage
max_capital_per_day <- equity * max_capital_per_day_percentage
# Calcula el efectivo disponible para trading hoy, asegurando que se mantenga la reserva de efectivo
cash_for_trading <- min(total_cash_disponible_hoy - cash_reserve, max_capital_per_day)
# Asegúrate de que no se use más efectivo del disponible después de la reserva
cash_for_trading <- max(0, cash_for_trading)

volatilidad_al_dia_de_hoy <- c(10, 7.8, 2)
names(volatilidad_al_dia_de_hoy) <- symbols
volatilidad_al_dia_de_hoy
señales_el_dia_de_hoy <- señales[señales$dia == today,]

#distribucion del capital basado en las señales del dia de hoy y la volatilidad

capital_distribuido <- list()
for(symb in symbols){
  if(symb %in% señales_el_dia_de_hoy$symbolos_compra){
    capital_distribuido[[symb]] <- cash_for_trading * volatilidad_al_dia_de_hoy[symb] / sum(volatilidad_al_dia_de_hoy)
  } else {
    capital_distribuido[[symb]] <- 0
  }
}
capital_distribuido
#mandar ordenes
for(symbol in symbols){
  #print(symbol)
  if(capital_distribuido[[symbol]] > 0){
    print(symbol)
    price = Cl(get(symb)[today])[[1]]
    print(price)
    order_qty <- capital_distribuido[[symbol]]/price
    order_qty <- abs(round(order_qty, 0))[[1]]
    print(order_qty)
    if(order_qty > 0){
      addTxn(Portfolio = portfolio.st, Symbol = symbol, TxnDate = dias.fecha[today], TxnPrice = price, TxnQty = order_qty, TxnFees = 0)
    }
  }
}

updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
