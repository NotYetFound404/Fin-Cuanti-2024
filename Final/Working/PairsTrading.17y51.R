#----------
# Libraries
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(dplyr)
library(purrr)
library(tseries)
library(ggplot2)
library(quantstrat)
#--------
#funtions--------------
# Función para verificar la cointegración de un par de activos en una ventana móvil de n días
cointegrationIndicator <- function(pair_name, n = 60) {
  #pair_name = "EBAY_AAPL"
  #pair_name = StockA_StockC
  #pair_name = deparse(substitute(pair_name))
  # n = 60
  pairs <- unlist(strsplit(pair_name, "_"))
  stock1 <- pairs[[1]]
  stock2 <- pairs[[2]]
  
  # Obtener los precios de cierre ajustados de los activos
  CL.log.left <- Cl(get(stock1))
  CL.log.right <- Cl(get(stock2))
  # CL.log.left <- get(stock1)
  # CL.log.right <- get(stock2)
  
  
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
rankingFunctionSD <- function(column_name, window){
  #column_name = deparse(substitute(StockA_StockC))
  symbols = symbols
  mergedSymbols <- do.call(merge, lapply(symbols, function(sym) Cl(get(sym))))
  #apply rolling sd to mergedSymbols with a window of 30 days
  mergedSymbols <- rollapply(mergedSymbols, width = window, FUN = sd, by.column = TRUE, align = "right", fill = NA)
  #rankings <- t(apply(mergedSymbols, 1, function(x) order(x, decreasing = TRUE)))
  rankings <- t(apply(mergedSymbols, 1, function(x) if (all(is.na(x))) rep(NA, length(x)) else order(x, decreasing = TRUE)))
  #colnames(rankings) <- colnames(mergedSymbols)
  return(rankings[, column_name, drop = FALSE])
}
rollingSD <- function(spread_xts, window){
  rollingSD <- rollapply(spread_xts, width = window, FUN = sd, by.column = TRUE, align = "right", fill = NA)
  colnames(rollingSD) <- colnames(spread_xts)
  return(rollingSD)
}
#------
# Load data and symbols
# load(file = "Final/Working/realDataEnv.Rdata")
# load.instruments(file = "Final/Working/realDataInstruments.RData")
window <- 30
n <- 60
zScoreThresh <- 1
topRankedSDPairsThreshold <- 1
sizing_Leverage <- 0.2
max_size_per_trade <- 0.1
txCost <- 0.005
threshold <- 1

# Set timezone
.blotter <- new.env(); .strategy <- new.env(); Sys.setenv(TZ="UTC") #setting up the timezone
Sys.setenv(TZ="UTC")

# Symbols and currency
#symbols = c("EBAY_AAPL", "CRM_AMZN", "AAPL_NVDA")
symbols = c("EBAY_AAPL")
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


# Inicializa un dataframe vacío para almacenar las señales
signals_list <- list()
indicators_list <- list()
# Itera sobre cada símbolo en symbols
for(symb in symbols){
  symb = symbols[[1]]
  # Calcula los indicadores y señales para el símbolo actual
  rollingSD_indicator <- rollingSD(spread_xts = Cl(get(symb)), window = window)
  colnames(rollingSD_indicator) <- "RollingSDIndicator"
  cointegration_indicator <- cointegrationIndicator(pair_name = symb, n = n)
  colnames(cointegration_indicator) <- "Cointegrated"
  rolling_zscore_indicator <- rollingZScore(spread_xts = Cl(get(symb)), n = n)
  colnames(rolling_zscore_indicator) <- "Zscore"
  rankingFunctionSD_indicator <- rankingFunctionSD(column_name = get(symb), window = window)[1,]
  
  rankingFunctionSD_indicator <- as.xts(rankingFunctionSD_indicator)
  colnames(rankingFunctionSD_indicator) <- "RankingFunctIndicator"
  
  # Combina los indicadores en un dataframe
  df_indicators <- merge(rollingSD_indicator, cointegration_indicator, rolling_zscore_indicator, rankingFunctionSD_indicator)
  
  # Calcula las señales finales
  df_signals <- as.data.frame(df_indicators) %>%
    mutate(
      finalBuySignal = ifelse(Zscore >= zScoreThresh & Cointegrated == 1 & RankingFunctIndicator <= topRankedSDPairsThreshold, 1, 0),
      finalSellSignal = ifelse(Zscore <= -zScoreThresh & Cointegrated == 1 & RankingFunctIndicator <= topRankedSDPairsThreshold, 1, 0)
    ) %>%
    select(finalBuySignal, finalSellSignal)
  df_signals <- timetk::tk_xts(df_signals, order.by = index(get(symb)))
  
  signals_list[[symb]] = df_signals 
  indicators_list[[symb]] = df_indicators
}
signals_list
indicators_list
