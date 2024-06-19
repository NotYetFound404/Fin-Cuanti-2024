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
  
  
  # available_cash <- portfolio_state$Available_Cash[nrow(portfolio_state)]
  # margin_used <- portfolio_state$Margin_Used[nrow(portfolio_state)]
  # 
  # if (nrow(daily_signals) > 0) {
  #   total_inverse_sd <- sum(1 / daily_signals$SD)
  #   daily_signals$Weight <- (1 / daily_signals$SD) / total_inverse_sd
  #   daily_signals$Assigned_Cash <- daily_signals$Weight * available_cash
  #   
  #   cash_outflows <- 0
  #   new_margin_used <- 0
  #   
  #   for (j in 1:nrow(daily_signals)) {
  #     pair <- daily_signals$Pair[j]
  #     entry_type <- daily_signals$EntryType[j]
  #     price <- daily_signals$Price[j]
  #     assigned_cash <- daily_signals$Assigned_Cash[j]
  #     quantity <- floor(assigned_cash / abs(price))
  #     
  #     if ((price < 0 & entry_type == "compraSpreadBarato") | (price > 0 & entry_type == "vendeSpreadCaro")) {
  #       new_margin_used <- new_margin_used + abs(assigned_cash * 0.5)
  #     } else {
  #       cash_outflows <- cash_outflows + assigned_cash
  #     }
  #     
  #     if (entry_type == "compraSpreadBarato") {
  #       addTxn(Portfolio = "portfolio.st", Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = quantity, TxnFees = 0)
  #     } else if (entry_type == "vendeSpreadCaro") {
  #       addTxn(Portfolio = "portfolio.st", Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = -quantity, TxnFees = 0)
  #     }
  #   }
  #   
  #   margin_used <- margin_used + new_margin_used
  #   available_cash <- available_cash - cash_outflows
  # }
  # 
  # updatePortf("portfolio.st")
  # updateAcct("account.st")
  # updateEndEq("account.st")
  # 
  # InvestedEquity <- getEndEq(Account = "account.st", Date = date)
  # daily_pnl <- InvestedEquity - portfolio_state$InvestedEquity[nrow(portfolio_state)]
  # 
  # portfolio_state <<- rbind(portfolio_state, data.frame(
  #   Date = date,
  #   InvestedEquity = InvestedEquity,
  #   Available_Cash = available_cash,
  #   Margin_Used = margin_used,
  #   Total_PnL = portfolio_state$Total_PnL[nrow(portfolio_state)] + daily_pnl,
  #   Pct_Invested = (InvestedEquity - available_cash) / InvestedEquity,
  #   Pct_Margin_Used = margin_used / InvestedEquity,
  #   Pct_Cash_Available = available_cash / InvestedEquity
  # ))
}

short_margin_pct <- 0.50
maximo_peso_del_portafolio_para_una_sola_transaccion <- 0.25
maximo_peso_del_portafolio_para_un_solo_dia <- 0.5


dates <- seq.Date(from = as.Date(from)-1, to = as.Date(to), by = "days")
pre.dates <- dates[0:86]

for (date in pre.dates) {
  daily_signals <- subset(all_signals_df, Date == date)
  print(nrow(daily_signals))
  update_portfolio_state(date, daily_signals)
}

today <- dates[length(pre.dates)+1]
daily_signals <- subset(all_signals_df, Date == today)
#update_portfolio_state(today, daily_signals)

#logica para despues comenzar las transacciones
available_cash <- portfolio_state$Available_Cash[nrow(portfolio_state)]
margin_used <- portfolio_state$Margin_Used[nrow(portfolio_state)]
total_account_value <- portfolio_state$TotalAccountValue[nrow(portfolio_state)]
invested_equity <- portfolio_state$InvestedEquity[nrow(portfolio_state)]

available_cash
margin_used
total_account_value
invested_equity


total_inverse_sd <- sum(1 / daily_signals$SD)
daily_signals$AbsoluteTradingWeight <- (1 / daily_signals$SD) / total_inverse_sd
#posicion en neto
daily_signals <- daily_signals %>% mutate(
  net_position_type = case_when(
    (Price < 0)&(EntryType == "compraSpreadBarato") ~ "NetShortPosition",
    (Price > 0)&(EntryType == "vendeSpreadCaro") ~ "NetShortPosition",
    (Price > 0)&(EntryType == "compraSpreadBarato") ~ "NetLongPosition",
    (Price < 0)&(EntryType == "vendeSpreadCaro") ~ "NetLongPosition",
  ),
  
)

n.transacciones.hoy = nrow(daily_signals)
max_capital_a_invertir_hoy = total_account_value * maximo_peso_del_portafolio_para_un_solo_dia
max_capital_a_invertir_por_transaccion = total_account_value * maximo_peso_del_portafolio_para_una_sola_transaccion

#asignas los pesos
daily_signals <- daily_signals %>% mutate(
  monto_a_invertir = case_when(
    net_position_type == "NetShortPosition" ~ min(AbsoluteTradingWeight * max_capital_a_invertir_hoy, max_capital_a_invertir_por_transaccion),
    net_position_type == "NetLongPosition" ~ min(AbsoluteTradingWeight * max_capital_a_invertir_hoy, max_capital_a_invertir_por_transaccion),
  )
)
cash_outflows <- 0
new_margin_used <- 0
leverage <- 0

for (j in 1:nrow(daily_signals)) {
  #j = 1
  pair <- daily_signals$Pair[j]
  entry_type <- daily_signals$EntryType[j]
  price <- daily_signals$Price[j]
  monto_a_invertir <- daily_signals$monto_a_invertir[j]
  quantity <- floor(monto_a_invertir / abs(price))

  if ((price < 0 & entry_type == "compraSpreadBarato") | (price > 0 & entry_type == "vendeSpreadCaro")) {
    leverage <- leverage + abs(price*quantity)
    new_margin_used <- new_margin_used + abs(monto_a_invertir * short_margin_pct)
  } else {
    cash_outflows <- cash_outflows + monto_a_invertir
  }

  if (entry_type == "compraSpreadBarato") {
    addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = today, TxnPrice = price, TxnQty = quantity, TxnFees = 0)
  } else if (entry_type == "vendeSpreadCaro") {
    addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = today, TxnPrice = price, TxnQty = -quantity, TxnFees = 0)
  }
}
updatePortf(portfolio.st); updateAcct(account.st); updateEndEq(account.st)

updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)


pos1 <- getPos(Portfolio = portfolio.st, Symbol = "GOOGL_TSLA", Date = today) #obtener la posición de un activo
pos2 <- getPos(Portfolio = portfolio.st, Symbol = "TSLA_GOOGL", Date = today) #obtener la posición de un activo

mkvalue1 <- pos1$Pos.Qty * pos1$Pos.Avg.Cost
mkvalue2 <- pos2$Pos.Qty * pos2$Pos.Avg.Cost

invested_equity <- mkvalue2+mkvalue1
invested_equity <- invested_equity[[1]]

margin_used <- margin_used + new_margin_used

available_cash
margin_used
total_account_value
invested_equity





port <- getPortfolio(portfolio.st)
margin_used <- margin_used + new_margin_used
available_cash <- available_cash - cash_outflows
leverage



today
dailyEqPL(Account = account.st, Portfolios = portfolio.st) #obtener el P&L diario para cada pareja
extractTxns(Portfolio = portfolio.st) #obtener las transacciones)







#InvestedEquity <- getEndEq(Account = account.st, Date = today) #esto esta mal
#daily_pnl <- InvestedEquity - portfolio_state$InvestedEquity[nrow(portfolio_state)]

# portfolio_state <<- rbind(portfolio_state, data.frame(
#   Date = date,
#   InvestedEquity = InvestedEquity,
#   Available_Cash = available_cash,
#   Margin_Used = margin_used,
#   Total_PnL = portfolio_state$Total_PnL[nrow(portfolio_state)] + daily_pnl,
#   Pct_Invested = (InvestedEquity - available_cash) / InvestedEquity,
#   Pct_Margin_Used = margin_used / InvestedEquity,
#   Pct_Cash_Available = available_cash / InvestedEquity
# ))










#----------------------
# # Parámetro para el margen de posiciones cortas
# short_margin_pct <- 0.50
# 
# # Número de días totales en el periodo
# Date <- seq.Date(from = as.Date(from)-1, to = as.Date(to), by = "days")
# 
# # Dataframe para almacenar el estado diario del portafolio
# portfolio_state <- data.frame(
#   Date = Date,
#   Equity = rep(0, length(Date)),
#   Available_Cash = rep(0, length(Date)),
#   Margin_Used = rep(0, length(Date)),
#   Total_PnL = rep(0, length(Date)),
#   Pct_Invested = rep(NA, length(Date)),
#   Pct_Margin_Used = rep(NA, length(Date)),
#   Pct_Cash_Available = rep(NA, length(Date)),
#   stringsAsFactors = FALSE
# )
# portfolio_state[1, "Equity"] <- initEq
# portfolio_state[1, "Available_Cash"] <- initEq
# 
# # Lista para almacenar las posiciones abiertas
# open_positions <- list()
# 
# # Iterar sobre cada día en el dataframe de señales
# for (i in 2:nrow(portfolio_state)) {
#   date <- portfolio_state$Date[i]
#   daily_signals <- subset(all_signals_df, Date == date)
#   
#   if (nrow(daily_signals) == 0) {
#     # Si no hay señales, simplemente copiamos el estado del día anterior
#     portfolio_state[i, ] <- portfolio_state[i - 1, ]
#     portfolio_state[i, "Date"] <- date
#     next
#   }
#   
#   # Capital disponible para asignación
#   available_cash <- portfolio_state$Available_Cash[i - 1]
#   
#   # Proporción de asignación basado en SD inverso
#   total_inverse_sd <- sum(1 / daily_signals$SD)
#   daily_signals$Weight <- (1 / daily_signals$SD) / total_inverse_sd
#   
#   # Asignar capital proporcionalmente
#   daily_signals$Assigned_Cash <- daily_signals$Weight * available_cash
#   
#   # Procesar cada señal
#   for (j in 1:nrow(daily_signals)) {
#     pair <- daily_signals$Pair[j]
#     entry_type <- daily_signals$EntryType[j]
#     price <- daily_signals$Price[j]
#     assigned_cash <- daily_signals$Assigned_Cash[j]
#     quantity <- floor(assigned_cash / abs(price))
#     
#     if (entry_type == "compraSpreadBarato") {
#       addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = quantity, TxnFees = 0)
#     } else if (entry_type == "vendeSpreadCaro") {
#       addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = -quantity, TxnFees = 0)
#     }
#     
#     # Registrar la posición abierta
#     open_positions <- append(open_positions, list(list(
#       Date = date,
#       Pair = pair,
#       EntryType = entry_type,
#       EntryPrice = price,
#       Quantity = quantity
#     )))
#     
#     # Actualizar el margen utilizado si es una venta en corto
#     if (entry_type == "vendeSpreadCaro") {
#       portfolio_state$Margin_Used[i] <- portfolio_state$Margin_Used[i - 1] + assigned_cash * short_margin_pct
#     }
#   }
#   
#   # Actualizar el efectivo disponible después de las asignaciones
#   portfolio_state$Available_Cash[i] <- portfolio_state$Available_Cash[i - 1] - sum(daily_signals$Assigned_Cash)
#   
#   # Actualizar portafolio y cuenta
#   updatePortf(portfolio.st, Dates = date)
#   updateAcct(account.st, Dates = date)
#   updateEndEq(account.st, Dates = date)
#   
#   # Calcular el P&L diario
#   equity <- getEndEq(Account = account.st, Date = date)
#   daily_pnl <- equity - portfolio_state$Equity[i - 1]
#   
#   # Actualizar el equity y el efectivo disponible
#   portfolio_state$Equity[i] <- equity
#   portfolio_state$Available_Cash[i] <- portfolio_state$Available_Cash[i] + daily_pnl
#   portfolio_state$Total_PnL[i] <- portfolio_state$Total_PnL[i - 1] + daily_pnl
#   
#   # Monitorización
#   total_value <- sum(portfolio_state$Equity[i], portfolio_state$Available_Cash[i])
#   portfolio_state$Pct_Invested[i] <- (portfolio_state$Equity[i] - portfolio_state$Available_Cash[i]) / total_value
#   portfolio_state$Pct_Margin_Used[i] <- portfolio_state$Margin_Used[i] / total_value
#   portfolio_state$Pct_Cash_Available[i] <- portfolio_state$Available_Cash[i] / total_value
# }
# Parámetro para el margen de posiciones cortas
# short_margin_pct <- 0.50
# 
# # Número de días totales en el periodo
# Date <- seq.Date(from = as.Date(from)-1, to = as.Date(to), by = "days")
# 
# # Dataframe para almacenar el estado diario del portafolio
# portfolio_state <- data.frame(
#   Date = Date,
#   Equity = rep(0, length(Date)),
#   Available_Cash = rep(0, length(Date)),
#   Margin_Used = rep(0, length(Date)),
#   Total_PnL = rep(0, length(Date)),
#   Pct_Invested = rep(NA, length(Date)),
#   Pct_Margin_Used = rep(NA, length(Date)),
#   Pct_Cash_Available = rep(NA, length(Date)),
#   stringsAsFactors = FALSE
# )
# portfolio_state[1, "Equity"] <- initEq
# portfolio_state[1, "Available_Cash"] <- initEq
# 
# # Lista para almacenar las posiciones abiertas
# open_positions <- list()
# 
# # Iterar sobre cada día en el dataframe de señales
# for (i in 2:nrow(portfolio_state)) {
#   i = 87
#   date <- portfolio_state$Date[i]
#   daily_signals <- subset(all_signals_df, Date == date)
#   
#   if (nrow(daily_signals) == 0) {
#     # Si no hay señales, simplemente copiamos el estado del día anterior
#     portfolio_state[i, ] <- portfolio_state[i - 1, ]
#     portfolio_state[i, "Date"] <- date
#     next
#   }
#   
#   # Capital disponible para asignación
#   available_cash <- portfolio_state$Available_Cash[i - 1]
#   
#   # Proporción de asignación basado en SD inverso
#   total_inverse_sd <- sum(1 / daily_signals$SD)
#   daily_signals$Weight <- (1 / daily_signals$SD) / total_inverse_sd
#   
#   # Asignar capital proporcionalmente
#   daily_signals$Assigned_Cash <- daily_signals$Weight * available_cash
#   
#   # Procesar cada señal
#   for (j in 1:nrow(daily_signals)) {
#     j = 1
#     pair <- daily_signals$Pair[j]
#     entry_type <- daily_signals$EntryType[j]
#     price <- daily_signals$Price[j]
#     assigned_cash <- daily_signals$Assigned_Cash[j]
#     quantity <- floor(assigned_cash / abs(price))
#     
#     if (entry_type == "compraSpreadBarato") {
#       addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = quantity, TxnFees = 0)
#     } else if (entry_type == "vendeSpreadCaro") {
#       addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = -quantity, TxnFees = 0)
#       
#       # Ajuste del capital en la cuenta de margen para operaciones en corto
#       margin_used <- assigned_cash * short_margin_pct
#       portfolio_state$Margin_Used[i] <- portfolio_state$Margin_Used[i - 1] + margin_used
#     }
#     
#     # Registrar la posición abierta
#     open_positions <- append(open_positions, list(list(
#       Date = date,
#       Pair = pair,
#       EntryType = entry_type,
#       EntryPrice = price,
#       Quantity = quantity
#     )))
#   }
#   
#   # Actualizar el efectivo disponible después de las asignaciones
#   portfolio_state$Available_Cash[i] <- portfolio_state$Available_Cash[i - 1] - sum(daily_signals$Assigned_Cash)
#   
#   # Actualizar portafolio y cuenta
#   updatePortf(portfolio.st, Dates = date)
#   updateAcct(account.st, Dates = date)
#   updateEndEq(account.st, Dates = date)
#   
#   # Calcular el P&L diario
#   equity <- getEndEq(Account = account.st, Date = date)
#   daily_pnl <- equity - portfolio_state$Equity[i - 1]
#   
#   # Actualizar el equity y el efectivo disponible
#   portfolio_state$Equity[i] <- equity
#   portfolio_state$Available_Cash[i] <- portfolio_state$Available_Cash[i - 1] + daily_pnl
#   portfolio_state$Total_PnL[i] <- portfolio_state$Total_PnL[i - 1] + daily_pnl
#   
#   # Monitorización
#   total_value <- sum(portfolio_state$Equity[i], portfolio_state$Available_Cash[i])
#   portfolio_state$Pct_Invested[i] <- (portfolio_state$Equity[i] - portfolio_state$Available_Cash[i]) / total_value
#   portfolio_state$Pct_Margin_Used[i] <- portfolio_state$Margin_Used[i] / total_value
#   portfolio_state$Pct_Cash_Available[i] <- portfolio_state$Available_Cash[i] / total_value
# }

# # Parámetro para el margen de posiciones cortas
# short_margin_pct <- 1
# 
# # Número de días totales en el periodo
# Date <- seq.Date(from = as.Date(from) - 1, to = as.Date(to), by = "days")
# 
# # Dataframe para almacenar el estado diario del portafolio
# portfolio_state <- data.frame(
#   Date = Date,
#   Equity = rep(0, length(Date)),
#   Available_Cash = rep(0, length(Date)),
#   Margin_Used = rep(0, length(Date)),
#   Total_PnL = rep(0, length(Date)),
#   Pct_Invested = rep(NA, length(Date)),
#   Pct_Margin_Used = rep(NA, length(Date)),
#   Pct_Cash_Available = rep(NA, length(Date)),
#   stringsAsFactors = FALSE
# )
# portfolio_state[1, "Equity"] <- initEq
# portfolio_state[1, "Available_Cash"] <- initEq
# 
# # Lista para almacenar las posiciones abiertas
# open_positions <- list()
# 
# # Iterar sobre cada día en el dataframe de señales
# for (i in 2:nrow(portfolio_state)) {
#   date <- portfolio_state$Date[i]
#   daily_signals <- subset(all_signals_df, Date == date)
#   
#   if (nrow(daily_signals) == 0) {
#     # Si no hay señales, simplemente copiamos el estado del día anterior
#     portfolio_state[i, ] <- portfolio_state[i - 1, ]
#     portfolio_state[i, "Date"] <- date
#     next
#   }
#   
#   # Capital disponible para asignación
#   available_cash <- portfolio_state$Available_Cash[i - 1]
#   
#   # Proporción de asignación basado en SD inverso
#   total_inverse_sd <- sum(1 / daily_signals$SD)
#   daily_signals$Weight <- (1 / daily_signals$SD) / total_inverse_sd
#   
#   # Asignar capital proporcionalmente
#   daily_signals$Assigned_Cash <- daily_signals$Weight * available_cash
#   
#   # Procesar cada señal
#   for (j in 1:nrow(daily_signals)) {
#     pair <- daily_signals$Pair[j]
#     entry_type <- daily_signals$EntryType[j]
#     price <- daily_signals$Price[j]
#     assigned_cash <- daily_signals$Assigned_Cash[j]
#     quantity <- floor(assigned_cash / abs(price))
#     
#     if (entry_type == "compraSpreadBarato") {
#       addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = quantity, TxnFees = 0)
#     } else if (entry_type == "vendeSpreadCaro") {
#       addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = -quantity, TxnFees = 0)
#       
#       # Ajuste del capital en la cuenta de margen para operaciones en corto
#       margin_used <- assigned_cash * short_margin_pct
#       portfolio_state$Margin_Used[i] <- portfolio_state$Margin_Used[i - 1] + margin_used
#     }
#     
#     # Registrar la posición abierta
#     open_positions <- append(open_positions, list(list(
#       Date = date,
#       Pair = pair,
#       EntryType = entry_type,
#       EntryPrice = price,
#       Quantity = quantity
#     )))
#   }
#   
#   # Actualizar el efectivo disponible después de las asignaciones
#   portfolio_state$Available_Cash[i] <- portfolio_state$Available_Cash[i - 1] - sum(daily_signals$Assigned_Cash)
#   
#   # Actualizar portafolio y cuenta
#   updatePortf(portfolio.st, Dates = date)
#   updateAcct(account.st, Dates = date)
#   updateEndEq(account.st, Dates = date)
#   
#   # Calcular el P&L diario
#   equity <- getEndEq(Account = account.st, Date = date)
#   daily_pnl <- equity - portfolio_state$Equity[i - 1]
#   
#   # Actualizar el equity y el efectivo disponible
#   portfolio_state$Equity[i] <- equity
#   portfolio_state$Available_Cash[i] <- portfolio_state$Available_Cash[i - 1] + daily_pnl
#   portfolio_state$Total_PnL[i] <- portfolio_state$Total_PnL[i - 1] + daily_pnl
#   
#   # Monitorización
#   total_value <- sum(portfolio_state$Equity[i], portfolio_state$Available_Cash[i])
#   portfolio_state$Pct_Invested[i] <- (portfolio_state$Equity[i] - portfolio_state$Available_Cash[i]) / total_value
#   portfolio_state$Pct_Margin_Used[i] <- portfolio_state$Margin_Used[i] / total_value
#   portfolio_state$Pct_Cash_Available[i] <- portfolio_state$Available_Cash[i] / total_value
# }
#----------------------
short_margin_pct <- 0.5
Date <- seq.Date(from = as.Date(from) - 1, to = as.Date(to), by = "days")
# Dataframe para almacenar el estado diario del portafolio
portfolio_state <- data.frame(
  Date = Date,
  Equity = rep(0, length(Date)),
  Available_Cash = rep(0, length(Date)),
  Margin_Used = rep(0, length(Date)),
  Total_PnL = rep(0, length(Date)),
  Pct_Invested = rep(NA, length(Date)),
  Pct_Margin_Used = rep(NA, length(Date)),
  Pct_Cash_Available = rep(NA, length(Date)),
  stringsAsFactors = FALSE
)
portfolio_state[1, "Available_Cash"] <- initEq
# Lista para almacenar las posiciones abiertas
open_positions <- list()
# Iterar sobre cada día en el dataframe de señales
for (i in 2:nrow(portfolio_state)) {
  date <- portfolio_state$Date[i]
  daily_signals <- subset(all_signals_df, Date == date)
  if (nrow(daily_signals) == 0) {
    # Si no hay señales, simplemente copiamos el estado del día anterior
    portfolio_state[i, ] <- portfolio_state[i - 1, ]
    portfolio_state[i, "Date"] <- date
    next
  }
  # Capital disponible para asignación
  available_cash <- portfolio_state$Available_Cash[i - 1]
  # Proporción de asignación basado en SD inverso
  total_inverse_sd <- sum(1 / daily_signals$SD)
  daily_signals$Weight <- (1 / daily_signals$SD) / total_inverse_sd
  # Asignar capital proporcionalmente
  daily_signals$Assigned_Cash <- daily_signals$Weight * available_cash
  margin_used = 0
  cash_outflows = 0
  # Procesar cada señal
  for (j in 1:nrow(daily_signals)) {
    pair <- daily_signals$Pair[j]
    entry_type <- daily_signals$EntryType[j]
    price <- daily_signals$Price[j]
    assigned_cash <- daily_signals$Assigned_Cash[j]
    quantity <- floor(assigned_cash / abs(price))
    if( (price < 0) & (entry_type == "compraSpreadBarato")  | (price > 0) & (entry_type == "vendeSpreadCaro") ){
      # Ajuste del capital en la cuenta de margen para operaciones en corto
      margin_used <- margin_used + abs(assigned_cash * short_margin_pct)
    }
    if( (price > 0) & (entry_type == "compraSpreadBarato")  | (price < 0) & (entry_type == "vendeSpreadCaro") ){
      # Ajuste del capital en la cuenta de margen para operaciones en corto
      cash_outflows <- cash_outflows + assigned_cash
    }
    if (entry_type == "compraSpreadBarato") {
      addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = quantity, TxnFees = 0)
    } else if (entry_type == "vendeSpreadCaro") {
      addTxn(Portfolio = portfolio.st, Symbol = pair, TxnDate = date, TxnPrice = price, TxnQty = -quantity, TxnFees = 0)
    }
    # Registrar la posición abierta
    open_positions <- append(open_positions, list(list(
      Date = date,
      Pair = pair,
      EntryType = entry_type,
      EntryPrice = price,
      Quantity = quantity
    )))
  }
  portfolio_state$Margin_Used[i] <- portfolio_state$Margin_Used[i-1] + margin_used
  # Actualizar el efectivo disponible después de las asignaciones
  portfolio_state$Available_Cash[i] <- portfolio_state$Available_Cash[i - 1] - cash_outflows
  # Actualizar portafolio y cuenta
  updatePortf(portfolio.st)
  updateAcct(account.st)
  updateEndEq(account.st)
  # Calcular el P&L diario
  if(date < daily_signals[1,]$Date){
    equity = 0
  } else {
    equity <- getEndEq(Account = account.st, Date = date)
  }
  daily_pnl <- equity - portfolio_state$Equity[i - 1]
  # Actualizar el equity y el efectivo disponible
  portfolio_state$Equity[i] <- equity
  #portfolio_state$Available_Cash[i] <- portfolio_state$Available_Cash[i - 1] + daily_pnl
  portfolio_state$Total_PnL[i] <- portfolio_state$Total_PnL[i - 1] + daily_pnl
  # Monitorización
  total_value <- sum(portfolio_state$Equity[i], portfolio_state$Available_Cash[i])
  portfolio_state$Pct_Invested[i] <- (portfolio_state$Equity[i] - portfolio_state$Available_Cash[i]) / total_value
  portfolio_state$Pct_Margin_Used[i] <- portfolio_state$Margin_Used[i] / total_value
  portfolio_state$Pct_Cash_Available[i] <- portfolio_state$Available_Cash[i] / total_value
}
View(portfolio_state)
port <- getPortfolio(portfolio.st)
dailyTxnPL(Portfolios = portfolio.st)
getTxns(Portfolio = portfolio.st,Symbol = "GOOGL_TSLA")
getTxns(Portfolio = portfolio.st,Symbol = "TSLA_GOOGL")
head(portfolio_state)
View(portfolio_state)
