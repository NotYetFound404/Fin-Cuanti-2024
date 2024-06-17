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
load(file = "Final/Working/synthetic_data_env.RData")
load.instruments(file = "Final/Working/synthetic_instruments.RData")
#funtions--------------
# Función para verificar la cointegración de un par de activos en una ventana móvil de n días
cointegrationIndicator <- function(pair_name, n = 60) {
  
  #pair_name = StockA_StockC
  pair_name = deparse(substitute(StockA_StockC))
  # n = 60
  pairs <- unlist(strsplit(pair_name, "_"))
  stock1 <- pairs[[1]]
  stock2 <- pairs[[2]]
  
  # Obtener los precios de cierre ajustados de los activos
  # CL.log.left <- Cl(get(stock1))
  # CL.log.right <- Cl(get(stock2))
  CL.log.left <- get(stock1)
  CL.log.right <- get(stock2)
  
  
  # Verificar que hay suficientes datos
  if (length(CL.log.left) < n || length(CL.log.right) < n) {
    stop("No hay suficientes datos para realizar el análisis de cointegración.")
  }
  
  # Inicializar el vector de resultados
  result <- rep(NA, NROW(get(pair_name)) )
  
  for (i in seq(n,  NROW(get(pair_name)))) {
    # Obtener los datos de la ventana actual
    window_left <- CL.log.left[(i - n + 1):i]
    window_right <- CL.log.right[(i - n + 1):i]
    
    # Ejecutar la regresión lineal sin intercepto
    m <- lm(window_left ~ window_right - 1)
    
    # Obtener los residuos de la regresión
    sprd <- residuals(m)
    
    # Realizar la prueba ADF sobre los residuos
    adf_test <- adf.test(sprd, alternative = "stationary", k = 0)
    
    # Evaluar el p-valor de la prueba ADF
    pvalue <- adf_test$p.value
    
    # Determinar si el par está cointegrado (p-valor < 0.05)
    result[i] <- pvalue < 0.05
  }
  
  # Convertir el resultado a xts
  result_xts <- xts(result, order.by = index(get(pair_name)))
  colnames(result_xts) <- "Cointegrated"
  
  return(result_xts)
}
rollingZScore <- function(spread_xts, n = 60) {
  # spread_xts = StockA_StockC
  # n = 60
  
  # Inicializar el vector de resultados
  zscore <- rep(NA, NROW(spread_xts))
  
  for (i in seq(n, NROW(spread_xts))) {
    #i = 61
    
    # Obtener los datos de la ventana actual
    window_spread <- spread_xts[(i - n + 1):i]
    
    # Calcular la media y la desviación estándar del spread en la ventana actual
    mean_spread <- mean(window_spread)
    sd_spread <- sd(window_spread)
    
    # Calcular el Z-Score del spread actual
    current_spread <- spread_xts[i]
    zscore[i] <- (current_spread - mean_spread) / sd_spread
  }
  
  # Convertir el resultado a xts
  zscore_xts <- xts(zscore, order.by = index(spread_xts))
  colnames(zscore_xts) <- "Z-Score"
  
  return(zscore_xts)
}
#--------------
# Load the quantstrat package
library(quantstrat)
#ensure new env is clean
.blotter <- new.env(); .strategy <- new.env(); Sys.setenv(TZ="UTC") #setting up the timezone

#set stocks and currecy
symbols = c("StockA_StockC", "StockH_StockD"); currency = "USD"


# Define your trade size and initial equity
tradesize <- 100000
initEq <- 100000

# Create initdate, from, and to strings
initDate <- "1999-01-01"
from <- "2020-01-0"
to <- "2022-01-0"

# Define the names of your strategy, portfolio and account
strategy.st <- portfolio.st <- account.st <- "firststrat"; rm.strat("firststrat")# Remove the existing strategy if it exists


# Initialize the portfolio, account, and orders
initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = currency); initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = currency, initEq = initEq); initOrders(portfolio.st, initDate = initDate); strategy(strategy.st, store = TRUE)

#first check at what strategy holds
strategy <- getStrategy(strategy.st)
rankingFunction <- function(column_name){
  column_name = deparse(substitute(StockA_StockC))
  symbols = symbols
  mergedSymbols <- do.call(merge, lapply(symbols, function(sym) get(sym)))
  rankings <- t(apply(mergedSymbols, 1, function(x) order(x, decreasing = TRUE)))
  colnames(rankings) <- colnames(mergedSymbols)
  return(rankings[, column_name, drop = FALSE])
}

#add ranking function indicator
add.indicator(strategy = strategy.st,
              name = "rankingFunction",
              arguments = list(column_name = quote(mkdata)),
              label = "rankingFunction")

test <- applyIndicators(strategy = strategy.st, mktdata = StockA_StockC)
a <- rankingFunction(column_name = "StockA_StockC")
all(test$StockA_StockC.rankingFunction %in% a)

#ranking function based on a rolling volatility
rankingFunctionSD <- function(column_name, window){
  column_name = deparse(substitute(StockA_StockC))
  symbols = symbols
  mergedSymbols <- do.call(merge, lapply(symbols, function(sym) get(sym)))
  #apply rolling sd to mergedSymbols with a window of 30 days
  mergedSymbols <- rollapply(mergedSymbols, width = window, FUN = sd, by.column = TRUE, align = "right", fill = NA)
  rankings <- t(apply(mergedSymbols, 1, function(x) order(x, decreasing = TRUE)))
  colnames(rankings) <- colnames(mergedSymbols)
  return(rankings[, column_name, drop = FALSE])
}

#add ranking function indicator
add.indicator(strategy = strategy.st,
              name = "rankingFunctionSD",
              arguments = list(column_name = quote(mkdata), window = 30),
              label = "rankingFunctionSD")

test2 <- applyIndicators(strategy = strategy.st, mktdata = StockA_StockC)
b <- rankingFunctionSD(column_name = "StockA_StockC", window = 30)
all(test2$StockA_StockC.rankingFunctionSD %in% b)
