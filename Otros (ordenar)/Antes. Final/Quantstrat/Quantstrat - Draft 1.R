# Load required libraries
library(quantmod)  # For financial data analysis and visualization
library(TTR)       # For technical trading rules and indicators

# Define the date range for the data
from <- "2018-11-01"
to <- "2018-12-31"

# Retrieve the historical data for the SPY ETF (tracks the S&P 500) from Yahoo Finance
getSymbols("SPY", from = from, to = to)

# Define the lookback periods for the Simple Moving Averages (SMAs)
nFast <- 5   # Fast SMA period
nSlow <- 15  # Slow SMA period

# Calculate the Simple Moving Averages for the closing prices
smaFast <- SMA(Cl(SPY), n = nFast)  # Calculate the 5-day SMA for the closing prices
smaSlow <- SMA(Cl(SPY), n = nSlow)  # Calculate the 15-day SMA for the closing prices

# Plot the closing prices of SPY without volume
chartSeries(SPY, name = "SPY with SMA Indicators", TA = NULL)

# Overlay the fast SMA (5-day) on the price chart in blue
addTA(smaFast, col = "blue", on = 1, lwd = 2, legend = paste("SMA", nFast))

# Overlay the slow SMA (15-day) on the price chart in red
addTA(smaSlow, col = "red", on = 1, lwd = 2, legend = paste("SMA", nSlow))

# Generate trading signals based on the crossover of the fast and slow SMAs
# A long signal (1) is generated when the fast SMA is above or equal to the slow SMA
# A short signal (-1) is generated when the fast SMA is below the slow SMA
longSignal <- ifelse(smaFast >= smaSlow, 1, 0)  # Long signal when fast SMA >= slow SMA
shortSignal <- ifelse(smaFast < smaSlow, -1, 0) # Short signal when fast SMA < slow SMA
signals <- longSignal + shortSignal             # Combine long and short signals into one vector

# Extract the closing prices where buy (long) and sell (short) signals occur
Buys <- Cl(SPY)[signals == 1]   # Closing prices for buy signals
Sells <- Cl(SPY)[signals == -1] # Closing prices for sell signals

# Plot the closing prices of SPY with the trading signals
chartSeries(SPY, name = "SPY with Trading Signals", TA = NULL)

# Overlay the fast SMA (5-day) on the price chart in blue
addTA(smaFast, col = "blue", on = 1, lwd = 2, legend = paste("SMA", nFast))

# Overlay the slow SMA (15-day) on the price chart in red
addTA(smaSlow, col = "red", on = 1, lwd = 2, legend = paste("SMA", nSlow))

# Add green triangles at the buy signal points
addTA(Buys, pch = 6, type = "p", col = "green", on = 1, cex = 4)

# Add red triangles at the sell signal points
addTA(Sells, pch = 6, type = "p", col = "red", on = 1, cex = 4)
