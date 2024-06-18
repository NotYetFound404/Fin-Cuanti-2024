#libraries-------------
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(dplyr)
library(purrr)
library(tseries)
library(ggplot2)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(dplyr)
library(purrr)
library(tseries)
library(ggplot2)
library(quantstrat)
#get data --------------------
from <- "2020-07-01"
to <- "2020-11-13"
# mySymbols <- c('GOOGL', 'TSLA', 'AMZN', 'AAPL', 'MSFT', 'VOD',  'ADBE', 'NVDA', 'CRM',
#                'EBAY')
mySymbols <- c('GOOGL', 'TSLA')
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
  assign(.x, spread, envir = .GlobalEnv)
})
#check1 --------------------------
from
to
mySymbols
spread_names
#parameters
window = 60
pValueTresh  = 0.35
symb <- spread_names[[1]]

zscoreThresh = 0.5
rankingSDTresh = 2
take_profit <- 0.15 
stop_loss <- 0.5

#funtions--------------
cointegrationIndicator <- function(pair_name, n = 60, pTresh) {
  # pTresh= 0.35# pair_name = symb
  pairs <- unlist(strsplit(pair_name, "_"))
  stock1 <- pairs[[1]]
  stock2 <- pairs[[2]]
  # Obtener los precios de cierre ajustados de los activos
  CL.log.left <- Cl(get(stock1))
  CL.log.right <- Cl(get(stock2))
  # Verificar que hay suficientes datos
  if (length(CL.log.left) < n || length(CL.log.right) < n) {
    stop("No hay suficientes datos para realizar el análisis de cointegración.")
  }
  # Inicializar el vector de resultados
  result <- rep(NA, NROW(Cl(get(pair_name))))
  pvalueStore <- rep(NA, NROW(Cl(get(pair_name))))
  for (i in seq(n,  NROW(Cl(get(pair_name))))) {
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
    pvalueStore[i] <- pvalue
    # Determinar si el par está cointegrado (p-valor < 0.05)
    result[i] <- pvalue < pTresh
  }
  # Convertir el resultado a xts
  result_xts <- xts(result, order.by = index(Cl(get(pair_name))))
  colnames(result_xts) <- "rollingCointegration"
  
  return(result_xts)
}
rollingZScore <- function(pair_name, n = 60) {
  # Inicializar el vector de resultados
  zscore <- rep(NA, NROW(Cl(get(pair_name))))
  for (i in seq(n, NROW(Cl(get(pair_name))))) {
    # Obtener los datos de la ventana actual
    window_spread <- Cl(get(pair_name))[(i - n + 1):i]
    # Calcular la media y la desviación estándar del spread en la ventana actual
    mean_spread <- mean(window_spread)
    sd_spread <- sd(window_spread)
    # Calcular el Z-Score del spread actual
    current_spread <- Cl(get(pair_name))[i]
    zscore[i] <- (current_spread - mean_spread) / sd_spread
  }
  
  # Convertir el resultado a xts
  zscore_xts <- xts(zscore, order.by = index(Cl(get(pair_name))))
  colnames(zscore_xts) <- "rollingZScore"
  
  return(zscore_xts)
}
rollingSD <- function(pair_name, n = 60){
  rollingSD <- rollapply(Cl(get(pair_name)), width = n, FUN = sd, by.column = TRUE, align = "right", fill = NA)
  colnames(rollingSD) <- "rollingSD"
  return(rollingSD)
}
rankingFunctionSD <- function(all_pairs = spread_names, pair_name, n){
  mergedSymbols <- do.call(merge, lapply(all_pairs, function(x) Cl(get(x))) )
  colnames(mergedSymbols) <- all_pairs
  #apply rolling sd to mergedSymbols with a window of 30 days
  mergedSymbols <- rollapply(mergedSymbols, width = n, FUN = sd, by.column = TRUE, align = "right", fill = NA)
  #rankings <- t(apply(mergedSymbols, 1, function(x) order(x, decreasing = TRUE)))
  rankings <- t(apply(mergedSymbols, 1, function(x) if (all(is.na(x))) rep(NA, length(x)) else order(x, decreasing = TRUE)))
  colnames(rankings) <- colnames(mergedSymbols)
  out <- xts(rankings[, pair_name, drop = FALSE], order.by = index(mergedSymbols))
  colnames(out) <- "rollingSDRanking"
  return(out)
}
genSymSignals <- function(symb, window, pValueTresh, zscoreThresh, rankingSDTresh, take_profit){
  cointegration <- cointegrationIndicator(symb, n = window, pTresh = pValueTresh)
  zscore <- rollingZScore(symb, n = window)
  sd <- rollingSD(symb, n = window)
  rankedSD <- rankingFunctionSD(all_pairs = spread_names, pair_name = symb, n = 60)
  indicators <- merge(cointegration, zscore, sd, rankedSD)
  signals <- as.data.frame(indicators) %>% mutate(
    longSignal = ifelse(rollingCointegration == TRUE & rollingZScore < -zscoreThresh & rollingSDRanking <= rankingSDTresh, 1, 0),
    shortSignal = ifelse(rollingCointegration == TRUE & rollingZScore > zscoreThresh & rollingSDRanking <= rankingSDTresh, -1, 0),
    ZscoreInsideZscoreThresh = ifelse(rollingZScore > -zscoreThresh & rollingZScore < zscoreThresh, 1, 0),
    ZscoreInsideTakeProfit = ifelse(rollingZScore > -(zscoreThresh/(1+take_profit)) & rollingZScore < (zscoreThresh/(1+take_profit)), 1, 0),
  )
  signals$price = coredata(Cl(get(symb)))[,1]
  
  signals <- signals %>%  mutate(
  EntryPrice = case_when(
    longSignal == 1 ~ price,
    shortSignal == -1 ~ price,
    TRUE ~ NA
  ),
  EntryType = case_when(
    longSignal == 1 ~ "compraSpreadBarato",
    shortSignal == -1 ~ "vendeSpreadCaro",
    TRUE ~ NA
  )
)

  return(signals)
}
#get signals------
all_signals <- list()
for(symb in spread_names){ all_signals[[symb]] <- genSymSignals(symb, window, pValueTresh, zscoreThresh, rankingSDTresh, take_profit)}
signals <- do.call(cbind, map(all_signals, function (x) select(x, price, EntryType,rollingSD)))#flatten wide all signals


# Función para agregar las señales diarias a un registro
add_daily_signals <- function(signals) {
  all_signals <- list()
  for (i in 1:nrow(signals)) {
    date <- rownames(signals)[i]
    for (col in seq(1, ncol(signals), by = 3)) {
      pair_name <- colnames(signals)[col]
      price <- signals[i, col]
      entry_type <- signals[i, col + 1]
      sd <- signals[i, col + 2]
      
      if (!is.na(entry_type)) {
        all_signals <- append(all_signals, list(data.frame(Date = date, Pair = pair_name, EntryType = entry_type, Price = price, SD = sd)))
      }
    }
  }
  
  all_signals <- bind_rows(all_signals)
  return(all_signals)
}

# Crear el registro de todas las señales
all_signals_df <- add_daily_signals(signals)

initEq = 1000
# Balancear las señales diarias según el riesgo (rollingSD)
balanced_signals <- all_signals_df %>%
  group_by(Date, EntryType) %>%
  mutate(
    risk_weight = 1 / SD,
    normalized_weight = risk_weight / sum(risk_weight),
    allocated_qty = case_when(
      EntryType == "compraSpreadBarato" ~ initEq * normalized_weight / Price,
      EntryType == "vendeSpreadCaro" ~ initEq * normalized_weight / Price
    )
  ) %>%
  ungroup()

# Función para ejecutar transacciones balanceadas
# execute_transactions <- function(balanced_signals, portfolio.st) {
#   for (i in 1:nrow(balanced_signals)) {
#     date <- as.Date(balanced_signals$Date[i])
#     pair <- balanced_signals$Pair[i]
#     price <- balanced_signals$Price[i]
#     qty <- balanced_signals$allocated_qty[i]
#     
#     addTxn(portfolio.st, Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = qty, TxnFees = 0)
#   }
# }
# 
# # Ejecutar las transacciones balanceadas
# execute_transactions(balanced_signals, portfolio.st)

# # Actualizar y evaluar el portafolio
# updatePortf(portfolio.st)
# updateAcct(account.st)
# updateEndEq(account.st)
# 
# # Ver el resumen de la cuenta
# tstats <- tradeStats(portfolio.st)
# print(tstats)