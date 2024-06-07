# Load Libraries
library(quantmod)
library(TTR)

# Load Data
from <- "2018-11-01"
to <- "2018-12-31"
getSymbols("SPY", from = from, to = to)

# Define Parameters
nFast <- 5
nSlow <- 15

# Calculate Indicators
smaFast <- SMA(Cl(SPY), n = nFast)
smaSlow <- SMA(Cl(SPY), n = nSlow)

# Plot Closing Prices with SMA Indicators
chartSeries(SPY, name = "SPY with SMA Indicators", TA = NULL) #plot with no volume
addTA(smaFast, col = "blue", on = 1, lwd = 2, legend = paste("SMA", nFast))
addTA(smaSlow, col = "red", on = 1, lwd = 2, legend = paste("SMA", nSlow))



# Create Signals
longSignal <- ifelse(smaFast >= smaSlow, 1, 0)
shortSignal <- ifelse(smaFast < smaSlow, -1, 0)
signals <- longSignal + shortSignal

#get the buys and sells prices give the signals
Buys <- Cl(SPY)[signals == 1]
Sells <- Cl(SPY)[signals == -1]

# Plot Signals on Price Chart
chartSeries(SPY, name = "SPY with Trading Signals", TA = NULL)
addTA(smaFast, col = "blue", on = 1, lwd = 2, legend = paste("SMA", nFast))
addTA(smaSlow, col = "red", on = 1, lwd = 2, legend = paste("SMA", nSlow))
addTA(Buys,pch = 6, type = "p", col = "green", on = 1,cex = 4)
addTA(Sells,pch = 6, type = "p", col = "red", on = 1,cex = 4)




# # Define Custom Indicator
# customSMA <- function(x, n, factor) {
#   sma <- SMA(x, n)
#   custom_sma <- sma * factor
#   return(custom_sma)
# }
# 
# # Calculate Custom Indicator
# customSMA_10 <- customSMA(Cl(SPY), n = 10, factor = 1.1)
# 
# # Plot Custom Indicator on Price Chart
# chartSeries(SPY, name = "SPY with Custom SMA Indicator")
# addTA(smaFast, col = "blue", on = 1, lwd = 2, legend = paste("SMA", nFast))
# addTA(smaSlow, col = "red", on = 1, lwd = 2, legend = paste("SMA", nSlow))
# addTA(customSMA_10, col = "green", on = 1, lwd = 2, legend = "Custom SMA 10")

