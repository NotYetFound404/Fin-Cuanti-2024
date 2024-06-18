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



dias.fecha <- index(get(symbols[1]))
today <- 11
symb = symbols[[1]]

begginingEQ <- getEndEq(account.st, dias.fecha[1])

price = Cl(get(symb))[[1]]
addTxn(Portfolio = portfolio.st, Symbol = symb, TxnDate = dias.fecha[today], TxnPrice = price, TxnQty = order_qty, TxnFees = 0)
