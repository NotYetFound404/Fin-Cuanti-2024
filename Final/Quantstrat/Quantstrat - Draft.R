# Load Libraries
library(quantmod)
library(TTR)

# Load Data
from <- "2017-01-03"
to <- "2018-03-18"
getSymbols("SPY", from = from, to = to)

# Visualize Raw Data
chartSeries(SPY, name = "SPY Closing Prices")

# Define Parameters
nFast <- 10
nSlow <- 30

# Calculate Indicators
smaFast <- SMA(Cl(SPY), n = nFast)
smaSlow <- SMA(Cl(SPY), n = nSlow)

# Plot Closing Prices with SMA Indicators
chartSeries(SPY, name = "SPY with SMA Indicators")
addTA(smaFast, col = "blue", on = 1, lwd = 2, legend = paste("SMA", nFast))
addTA(smaSlow, col = "red", on = 1, lwd = 2, legend = paste("SMA", nSlow))

# Create Signals
longSignal <- ifelse(smaFast >= smaSlow, 1, 0)
shortSignal <- ifelse(smaFast < smaSlow, -1, 0)
signals <- longSignal + shortSignal

# Plot Signals on Price Chart
chartSeries(SPY, name = "SPY with Trading Signals")
addTA(smaFast, col = "blue", on = 1, lwd = 2, legend = paste("SMA", nFast))
addTA(smaSlow, col = "red", on = 1, lwd = 2, legend = paste("SMA", nSlow))
addTA(signals, col = "purple", type = "h", on = 1, lwd = 2, legend = "Signals")

# Define Custom Indicator
customSMA <- function(x, n, factor) {
  sma <- SMA(x, n)
  custom_sma <- sma * factor
  return(custom_sma)
}

# Calculate Custom Indicator
customSMA_10 <- customSMA(Cl(SPY), n = 10, factor = 1.1)

# Plot Custom Indicator on Price Chart
chartSeries(SPY, name = "SPY with Custom SMA Indicator")
addTA(smaFast, col = "blue", on = 1, lwd = 2, legend = paste("SMA", nFast))
addTA(smaSlow, col = "red", on = 1, lwd = 2, legend = paste("SMA", nSlow))
addTA(customSMA_10, col = "green", on = 1, lwd = 2, legend = "Custom SMA 10")

zoomFrom <- "2018-03-01"
zoomTo <- "2018-03-18"
SPY_zoom <- window(SPY, start = zoomFrom, end = zoomTo)
smaFast_zoom <- window(smaFast, start = zoomFrom, end = zoomTo)
smaSlow_zoom <- window(smaSlow, start = zoomFrom, end = zoomTo)
signals_zoom <- window(signals, start = zoomFrom, end = zoomTo)

# Plot Closing Prices with SMA Indicators (Zoomed In)
chartSeries(SPY_zoom, name = "SPY with SMA Indicators (Zoomed In)")
addTA(signals_zoom, col = "purple", type = "h", on = 1, lwd = 2, legend = "Signals")


# Define a layout with two panels
layout(matrix(1:2, ncol = 1), heights = c(2, 1))

# Plot the main chart with SMA indicators
chartSeries(SPY_zoom, name = "SPY with SMA Indicators (Zoomed In)", TA = NULL)
addTA(smaFast_zoom, col = "blue", on = 1, lwd = 2, legend = paste("SMA", nFast))
addTA(smaSlow_zoom, col = "red", on = 1, lwd = 2, legend = paste("SMA", nSlow))

# Plot the signals on a separate chart below
barplot(signals_zoom, col = ifelse(signals_zoom > 0, "green", ifelse(signals_zoom < 0, "red", "white")),
        border = NA, space = 0, main = "Trading Signals", ylab = "Signal", xlab = "Date", 
        names.arg = index(signals_zoom), las = 2, cex.names = 0.7)


barplot(signals, col = ifelse(signals > 0, "green", ifelse(signals < 0, "red", "white")),
        border = NA, space = 0, main = "Trading Signals", ylab = "Signal", xlab = "Date", 
        names.arg = index(signals), las = 2, cex.names = 0.7)
