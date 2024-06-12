# Carga de paquetes necesarios
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)

# Función para identificar un patrón bullish engulfing
is_bullish_engulfing <- function(open1, close1, open2, close2) {
  return(close1 < open1 & open2 < close2 & close2 > open1 & open2 < close1)
}

# Obtener datos históricos de una acción (ejemplo: Apple Inc.)
stock_symbol <- "AAPL"
start_date <- "2020-01-01"
end_date <- Sys.Date()

# Obtener los datos de la acción
stock_data <- getSymbols(stock_symbol, src = 'yahoo', from = start_date, to = end_date, auto.assign = FALSE)

# Convertir a data frame para facilitar el manejo
stock_df <- data.frame(Date = index(stock_data), coredata(stock_data))

# Identificar patrones bullish engulfing
bullish_engulfing_indices <- c()
for (i in 2:NROW(stock_df)) {
  if (is_bullish_engulfing(stock_df$AAPL.Open[i-1], stock_df$AAPL.Close[i-1], stock_df$AAPL.Open[i], stock_df$AAPL.Close[i])) {
    bullish_engulfing_indices <- c(bullish_engulfing_indices, i)
  }
}

# Parámetros de la estrategia
stop_loss <- 0.10
stop_gain <- 0.30
capital <- 10000  # Capital inicial
positions <- data.frame(Date = as.Date(character()), BuyPrice = numeric(), SellPrice = numeric(), Profit = numeric())

# Ejecutar la estrategia
for (index in bullish_engulfing_indices) {
  buy_price <- stock_df$AAPL.Close[index]
  max_price <- buy_price * (1 + stop_gain)
  min_price <- buy_price * (1 - stop_loss)
  
  for (j in (index + 1):NROW(stock_df)) {
    if (!is.na(stock_df$AAPL.Close[j])) {
      if (stock_df$AAPL.Close[j] >= max_price) {
        sell_price <- max_price
        break
      } else if (stock_df$AAPL.Close[j] <= min_price) {
        sell_price <- min_price
        break
      }
    }
  }
  
  if (exists("sell_price")) {
    profit <- (sell_price - buy_price) / buy_price
    positions <- rbind(positions, data.frame(Date = stock_df$Date[index], BuyPrice = buy_price, SellPrice = sell_price, Profit = profit))
    rm(sell_price)
  }
}

# Calcular el retorno total de la estrategia
total_return <- prod(1 + positions$Profit) - 1
total_return_percentage <- total_return * 100

# Visualización
ggplot(stock_df, aes(x = Date)) +
  geom_segment(aes(xend = Date, y = AAPL.Low, yend = AAPL.High), color = "black") +
  geom_rect(aes(xmin = Date - 0.5, xmax = Date + 0.5, ymin = pmin(AAPL.Open, AAPL.Close), ymax = pmax(AAPL.Open, AAPL.Close), fill = AAPL.Close > AAPL.Open), color = "black") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), guide = "none") +
  geom_point(data = stock_df[bullish_engulfing_indices,], aes(y = AAPL.Close), color = "blue", size = 3) +
  labs(title = paste("Bullish Engulfing Pattern in", stock_symbol),
       subtitle = paste("From", start_date, "to", end_date),
       x = "Date",
       y = "Price") +
  theme_minimal() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 month")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Imprimir las transacciones y el retorno total
print(positions)
cat("Total Return: ", total_return_percentage, "%\n")

#####

# Obtener datos históricos del S&P 500
sp500_data <- getSymbols("^GSPC", src = 'yahoo', from = start_date, to = end_date, auto.assign = FALSE)
sp500_df <- data.frame(Date = index(sp500_data), coredata(sp500_data))

# Calcular el retorno del S&P 500
sp500_return <- (last(sp500_df$GSPC.Close) - first(sp500_df$GSPC.Close)) / first(sp500_df$GSPC.Close)
sp500_return_percentage <- sp500_return * 100

# Imprimir las transacciones y el retorno total
print(positions)
cat("Total Return of the Strategy: ", total_return_percentage, "%\n")
cat("Total Return of the S&P 500: ", sp500_return_percentage, "%\n")
