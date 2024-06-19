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
#Params --------------------------

# from
# to
# mySymbols
# spread_names
#parameters
window = 60
pValueTresh  = 0.35
symb <- spread_names[[1]]

zscoreThresh = 0.5
rankingSDTresh = 2
take_profit <- 0.15 
stop_loss <- 0.5

initEq = 1000

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
all_signals_df <- add_daily_signals(signals); all_signals_df$Pair <- gsub("\\.price", "", all_signals_df$Pair)#eliminar .price

# portfolio management -------------
#inicializar portafolio
currency("USD"); .blotter <- new.env(); .strategy <- new.env(); Sys.setenv(TZ="UTC");Sys.setenv(TZ="UTC"); strategy.st <- portfolio.st <- account.st <- "firststrat";rm.strat(strategy.st)
initPortf(portfolio.st, symbols = spread_names, initDate = from, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = from, currency = "USD", initEq = initEq)
initOrders(portfolio.st, initDate = from)


initial_cash <- 1000
portfolio_state <- data.frame(
  Date = as.Date('2020-01-01'),
  InvestedEquity = 0,
  Available_Cash = initial_cash,
  Margin_Used = 0,
  Total_PnL = 0,
  Pct_Invested = 0,
  Pct_Margin_Used = 0,
  Pct_Cash_Available = 1
)
portfolio_state$TotalAccountValue <- portfolio_state$InvestedEquity + portfolio_state$Margin_Used + portfolio_state$Available_Cash


update_portfolio_state <- function(date, daily_signals) {
  if(nrow(daily_signals) == 0){
    portfolio_state <<- rbind(portfolio_state, data.frame(
      Date = date,
      InvestedEquity = portfolio_state$InvestedEquity[nrow(portfolio_state)],
      Available_Cash = portfolio_state$Available_Cash[nrow(portfolio_state)],
      Margin_Used = portfolio_state$Margin_Used[nrow(portfolio_state)],
      Total_PnL = portfolio_state$Total_PnL[nrow(portfolio_state)],
      Pct_Invested = portfolio_state$Pct_Invested[nrow(portfolio_state)],
      Pct_Margin_Used = portfolio_state$Pct_Margin_Used[nrow(portfolio_state)],
      Pct_Cash_Available = portfolio_state$Pct_Cash_Available[nrow(portfolio_state)],
      TotalAccountValue = portfolio_state$TotalAccountValue[nrow(portfolio_state)]
    ))
    return(portfolio_state)
    
  }
}

short_margin_pct <- 0.50
maximo_peso_del_portafolio_para_una_sola_transaccion <- 0.25
maximo_peso_del_portafolio_para_un_solo_dia <- 0.5


dates <- seq.Date(from = as.Date(from)-1, to = as.Date(to), by = "days")
pre.dates <- dates[0:86]

for (date in pre.dates) {
  daily_signals <- subset(all_signals_df, Date == date)
  update_portfolio_state(date, daily_signals)
}

#Loop when there is a signal

today <- dates[length(pre.dates)+1]
daily_signals <- subset(all_signals_df, Date == today)

#Parte 1
available_cash <- portfolio_state$Available_Cash[nrow(portfolio_state)]
margin_used <- portfolio_state$Margin_Used[nrow(portfolio_state)]
total_account_value <- portfolio_state$TotalAccountValue[nrow(portfolio_state)]
invested_equity <- portfolio_state$InvestedEquity[nrow(portfolio_state)]

#orignial values
available_cash
margin_used
total_account_value
invested_equity

# otros valores
n.transacciones.hoy = nrow(daily_signals)
max_capital_a_invertir_hoy = total_account_value * maximo_peso_del_portafolio_para_un_solo_dia
max_capital_a_invertir_por_transaccion = total_account_value * maximo_peso_del_portafolio_para_una_sola_transaccion


#Decidir cuanto invertir
total_inverse_sd <- sum(1 / daily_signals$SD)
daily_signals$AbsoluteTradingWeight <- (1 / daily_signals$SD) / total_inverse_sd

daily_signals <- daily_signals %>% mutate(
  #posicion en neto
  net_position_type = case_when(
    (Price < 0)&(EntryType == "compraSpreadBarato") ~ "NetShortPosition",
    (Price > 0)&(EntryType == "vendeSpreadCaro") ~ "NetShortPosition",
    (Price > 0)&(EntryType == "compraSpreadBarato") ~ "NetLongPosition",
    (Price < 0)&(EntryType == "vendeSpreadCaro") ~ "NetLongPosition",
  ),
  #asignas los pesos
  max_dollar_amount_investment = case_when(
    net_position_type == "NetShortPosition" ~ min(AbsoluteTradingWeight * max_capital_a_invertir_hoy, max_capital_a_invertir_por_transaccion),
    net_position_type == "NetLongPosition" ~ min(AbsoluteTradingWeight * max_capital_a_invertir_hoy, max_capital_a_invertir_por_transaccion),
  )
)
View(daily_signals)

#parte 2
cash_outflows <- 0
new_margin_used <- 0
new_leverage <- 0

for (j in 1:nrow(daily_signals)) {
  pair <- daily_signals$Pair[j]
  entry_type <- daily_signals$EntryType[j]
  price <- daily_signals$Price[j]
  max_dollar_amount_investment <- daily_signals$max_dollar_amount_investment[j]
  quantity <- floor(max_dollar_amount_investment / abs(price))
  
  if ((price < 0 & entry_type == "compraSpreadBarato") | (price > 0 & entry_type == "vendeSpreadCaro")) {
    #leverage <- leverage + abs(price*quantity)
    new_leverage <- new_leverage + abs(price*quantity)
    new_margin_used <- new_margin_used + abs(max_dollar_amount_investment * short_margin_pct)
  } else {
    cash_outflows <- cash_outflows + max_dollar_amount_investment
  }
  
  if (entry_type == "compraSpreadBarato") {
    addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = today, TxnPrice = price, TxnQty = quantity, TxnFees = 0)
  } else if (entry_type == "vendeSpreadCaro") {
    addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = today, TxnPrice = price, TxnQty = -quantity, TxnFees = 0)
  }
}
updatePortf(portfolio.st); updateAcct(account.st); updateEndEq(account.st)

cash_outflows
margin_used <- margin_used + new_margin_used
margin_used
new_leverage


pos1 <- getPos(Portfolio = portfolio.st, Symbol = "GOOGL_TSLA", Date = today) #obtener la posición de un activo
pos2 <- getPos(Portfolio = portfolio.st, Symbol = "TSLA_GOOGL", Date = today) #obtener la posición de un activo

mkvalue1 <- pos1$Pos.Qty * pos1$Pos.Avg.Cost
mkvalue2 <- pos2$Pos.Qty * pos2$Pos.Avg.Cost

invested_equity <- mkvalue2+mkvalue1
invested_equity <- invested_equity[[1]]
invested_equity <- abs(invested_equity)
#es un valor en negativo porque es una posición corta que no me costo dinero sino que me dieron dinero
todaysPL <- dailyEqPL(account.st,incl.total = TRUE)
todaysPL <- todaysPL[today,"portfolio.PL"][[1]]

total_account_value <- available_cash + invested_equity + todaysPL + margin_used
Pct_Invested <- invested_equity / total_account_value
Pct_Margin_Used <- margin_used / total_account_value
Pct_Cash_Available <- available_cash / total_account_value

#añadir nueva fila
portfolio_state <<- rbind(portfolio_state, data.frame(
  Date = today,
  InvestedEquity = invested_equity,
  Available_Cash = available_cash,
  Margin_Used = margin_used,
  Total_PnL = todaysPL,
  Pct_Invested = Pct_Invested,
  Pct_Margin_Used = Pct_Margin_Used,
  Pct_Cash_Available = Pct_Cash_Available,
  TotalAccountValue = total_account_value
))




