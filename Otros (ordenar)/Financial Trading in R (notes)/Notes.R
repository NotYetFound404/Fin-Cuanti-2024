#Use getSymbols() to obtain adjusted data for SPY from January 1, 2000 through June 30, 2016 from Yahoo! Finance.
#Use Cl() to plot the closing price of SPY.
library(quantmod)
getSymbols("SPY", from = "2000-01-01", to = "2016-06-30", src = "yahoo", adjust = TRUE)
plot(Cl(SPY), main = "SPY Closing Price", ylab = "Price")

# Plot the closing prices of SPY
plot(Cl(SPY))
# Add a 200-day SMA using lines()
lines(SMA(Cl(SPY), n =200), col = "red")


# Use the library() command to load the quantstrat package.
# Set initdate as January 1, 1999, from as January 1, 2003, and to as December 31, 2015.
# Set a timezone of "UTC" using Sys.setenv().
# Set a currency of "USD" using currency().

library(quantstrat)
# Create initdate, from, and to strings
initDate <- "1999-01-01"
from <- "2003-01-01"
to <- "2015-12-31"
# Set the timezone to UTC
Sys.setenv(TZ = "UTC")
# Set the currency to USD 
currency("USD")


# Load the quantmod package
library(quantstrat)

# Retrieve SPY from yahoo
getSymbols("SPY", from = from, to = to, src = "yahoo", adjust = TRUE)

# Use stock() to initialize SPY and set currency to USD
stock("SPY", currency = "USD")

#boiler plate for quanstrat
# Define your trade size and initial equity
tradesize <- 100000
initeq <- 100000

# Define the names of your strategy, portfolio and account
strategy.st <- portfolio.st <- account.st <- "firststrat"
.blotter <- new.env()
.strategy <- new.env()
# Remove the existing strategy if it exists
rm.strat("firststrat")


#Use initPortf() to initialize the portfolio called portfolio.st with "SPY", initdate, and "USD" as the arguments.
#Use initAcct() to initialize the account called account.st with portfolio.st, initdate, "USD", and initeq as the arguments.
#Use initOrders() to initialize orders called portfolio.st and initdate as the arguments.
#Use strategy() to store a strategy called strategy.st with store = TRUE as the arguments.

# Initialize the portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initDate, currency = "USD")
# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = "USD", initEq = initeq)
# Initialize orders
initOrders(portfolio.st, initDate = initDate)
# Store the strategy
strategy(strategy.st, store = TRUE)

# Create a 200-day SMA of the closing price of SPY. Call this spy_sma.
# Create an RSI with a lookback period n of 3 days using the closing price of SPY. Call this spy_rsi.

# Create a 200-day SMA
spy_sma <- SMA(Cl(SPY), n = 200)

# Create an RSI with a 3-day lookback period
spy_rsi <- RSI(Cl(SPY), n = 3)

# Plot the closing prices of SPY
plot(Cl(SPY), main = "SPY Closing Price", ylab = "Price")

# Overlay a 200-day SMA
lines(spy_sma, col = "red")


# Plot the closing price of SPY
plot(Cl(SPY), main = "SPY Closing Price", ylab = "Price")

# Plot the RSI 2
plot(spy_rsi, main = "RSI of SPY", ylab = "RSI")


#Use add.indicator() on your existing strategy strategy.st. Follow the example code closely.
# Provide the SMA function as the name argument.
# Specify the desired arguments of SMA, using the closing price of mktdata and a lookback period n of 200 days.
# Label your indicator "SMA200".

add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 200), label = "SMA200")


# Add a 50-day SMA indicator to strategy.st
add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 50), label = "SMA50")



# Add an RSI 3 indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the RSI 3 function
              name = "RSI", 
              
              # Create a lookback period
              arguments = list(price = quote(Cl(mktdata)), n = 3), 
              
              # Label your indicator RSI_3
              label = "RSI_3")

#check if the indicator has been applied correctly
#coding your own indicator
# Create and name a function calc_RSI_avg with three arguments price, n1, and n2, in that order.
# Compute an RSI of lookback n1 named RSI_1.
# Compute an RSI of lookback n2 named RSI_2.
# Calculate the average of RSI_1 and RSI_2. Call this RSI_avg.
# Set the column name of RSI_avg to RSI_avg using colnames(), and return RSI_avg.
# Add this indicator to your strategy using inputs of n1 = 3 and n2 = 4. Label this indicator RSI_3_4.

# Write the calc_RSI_avg function
calc_RSI_avg <- function(price, n1, n2) {
  
  # RSI 1 takes an input of the price and n1
  RSI_1 <- RSI(price = price, n = n1)
  
  # RSI 2 takes an input of the price and n2
  RSI_2 <- RSI(price = price, n = n2)
  
  # RSI_avg is the average of RSI_1 and RSI_2
  RSI_avg <- (RSI_1 + RSI_2)/2
  
  # Your output of RSI_avg needs a column name of RSI_avg
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}

# Add this function as RSI_3_4 to your strategy with n1 = 3 and n2 = 4
add.indicator(strategy.st, name = calc_RSI_avg, arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = "RSI_3_4")

test <- applyIndicators(strategy.st, mktdata = SPY)
head(test,n = 10)

test2 <- applyIndicators(strategy.st, mktdata = OHLC(SPY))
head(test2,n = 10)

test3 <- applyIndicators(strategy.st, mktdata = OHLC(SPY["2003-01-01/2003-01-10"]))
test3


#Code your own indicator - II
# Create and name a function, DVO, for the indicator described above. The three arguments for your function will be HLC, navg (default to 2), and percentlookback (default to 126).
# The ratio of the close (Cl()) of HLC divided by the average of the high (Hi()) and low (Lo()) prices is computed for you.
# Use SMA() to implement a moving average of this ratio, parameterized by the navg argument. Save this as avgratio.
# Use runPercentRank() to implement a percentage ranking system for avgratio.


# Declare the DVO function
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# Knowing how to use the applyIndicators() command will help you identify your errors. Furthermore, sometimes you may want to look at a small chunk of time in your strategy. This exercise will help train you to do this as well.
# In order to subset time series data, use brackets with the start date, forward slash symbol, and end date. Both dates are in the same format as the from and to arguments for getSymbols() that you used in the first chapter. The quantstrat, TTR, and quantmod packages have once again been loaded for you.


# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))

# Subset your data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-01/2013-09-05"]



#Intro to signals
#market data + indicators or indicators + indicators

#types of signals
#sig comparison: Relationship between 2 indicators. Ex 1 if relationship is true is true while the relation holds
#sigCrossover: Cross over of 2 indicators. Ex 1 if indicator 1 crosses over indicator 2. Only true in the day of the crossing
#sig threshold. compares range bound indicator to a static quantity. Compares via comparison or treshold a given value (dynamic or static)
#sig formula. flexible signal funciton


#Subset test between September 10th, 2010, and October 10th, 2010, using
test["2010-09-10/2010-10-10"]
#Is SMA50(indicator) or RSI (another example) greater than or less than SMA200 on September 20?

#             SMA.SMA200 SMA.SMA50 DVO.DVO_2_126
#2010-09-20   99.41035  98.07610      67.46032
#sma50 < sma200


#Subset test between September 10th, 2010, and October 10th, 2010, using
test["2010-09-10/2010-10-10"]
#     SMA.SMA200 SMA.SMA50 DVO.DVO_2_126
#2010-09-30   99.60760  98.98035      19.84127
#Is DVO greater or smaller than 20 on September 30?
#dv0 < 20

#sig comparison vs sigcrossover (trend indicators)
#example shorter lookback ma crosses over longer lookback ma (50d vs 200d SMA)
#usualmente sigcrossover te dice cuando comprar o vender
#usualmente un sigcomparison te permite ver si las condiciones son favorables y te ayuda a tomar una decision de compra/venta


add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 50), label = "SMA50")

add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 200), label = "SMA200")

test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
tail(test,n=10)

# Add a signal to strategy.st that compares SMA50 to SMA200
# you will use sigComparison() to generate a signal comparison that specifies that the 50-day simple moving average (SMA) must be above the 200-day simple moving average (SMA). You will label this signal longfilter, because it signals that the short-term average is above the long-term averag

add.signal(strategy.st, name = "sigComparison", 
           arguments = list(columns = c("SMA50", "SMA200"), relationship = "gt"), 
           label = "longfilter")
#While having a long filter is necessary, it is not sufficient to put on a trade for this strategy. However, the moment the condition does not hold, the strategy should not hold any position whatsoever. For this exercise, you will implement the opposite of the rule specified above using the sigCrossover() function.
# As opposed to sigComparison(), which will always state whether or not a condition holds, sigCrossover() only gives a positive the moment the signal first occurs, and then not again. This is useful for a signal that will be used to initiate a transaction, as you only want one transaction in most cases, rather than having transactions fire again and again.
#As opposed to sigComparison(), which will always state whether or not a condition holds, sigCrossover() only gives a positive the moment the signal first occurs, and then not again. This is useful for a signal that will be used to initiate a transaction, as you only want one transaction in most cases, rather than having transactions fire again and again.


# Add a sigCrossover which specifies that the SMA50 is less than the SMA200 and label it filterexit
add.signal(strategy.st, name = "sigCrossover",
           
           # We're interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"),
                            
                            # The relationship is that the SMA50 crosses under the SMA200
                            relationship = "lt"),
           
           # Label it filterexit
           label = "filterexit")

#sig treshold
#cross = true only in the event of a signal crossover (mimics sigCrossover) only in that day
#cross = false mimics sig comparison is maintained as many days as long it is true (has passed the treshold)
#only takes one column at a time and one static threshold
#The sigThreshold signal is mainly used for comparing an indicator to a fixed number, which usually has applications for bounded oscillators, or perhaps rolling statistical scores (for example, for a trading strategy that might choose to go long when a ratio of mean to standard deviation is at -2, or vice versa). Whereas sigComparison and sigCrossover deal with quantities that are usually based off of an indicator that takes values in the same general area as prices, sigThreshold exists specifically to cover those situations outside the bounds of indicators that take values similar to prices
# Furthermore, the sigThreshold() function takes the cross argument, which specifies whether it will function similarly to sigComparison (cross = FALSE) or sigCrossover (cross = TRUE), respectively. In this exercise, you will implement a variant of sigThreshold that functions similarly to sigComparison.
# Your job will be to implement a sigThreshold that checks whether or not DVO_2_126 is under 20. This signal will serve as one of the two switches that need to be "on" in order to enter into a long position in the strategy.

# Implement a sigThreshold which specifies that DVO_2_126 must be less than 20, label it longthreshold
#relationship: c("gt", "lt", "eq", "gte", "lte"
add.signal(strategy.st, name = "sigThreshold", 
           
           # Use the DVO_2_126 column
           arguments = list(column = "DVO_2_126", 
                            
                            # The threshold is 20
                            threshold = 20, 
                            
                            # We want the oscillator to be under this value
                            relationship = "lt", 
                            
                            # We're interested in every instance that the oscillator is less than 20
                            cross = FALSE), 
           
           # Label it longthreshold
           label = "longthreshold")

?sigThreshold

#you will again use sigThreshold(), this time counting when the DVO_2_126 crosses above a threshold of 80. To mimic a sigCrossover signal, set cross equal to TRUE Label this signal thresholdexit.

# Add a sigThreshold signal to your strategy that specifies that DVO_2_126 must cross above 80 and label it thresholdexit
add.signal(strategy.st, name = "sigThreshold", 
           
           # Reference the column of DVO_2_126
           arguments = list(column = "DVO_2_126", 
                            
                            # Set a threshold of 80
                            threshold = 80, 
                            
                            # The oscillator must be greater than 80
                            relationship = "gt", 
                            
                            # We are interested only in the cross
                            cross = TRUE), 
           
           # Label it thresholdexit
           label = "thresholdexit")

#sig formula
#usualmente es mejor para combinar señales en vez de indicadores
#ej, cuando dos señales son verdad: longreshold (mi oscilador temporal supera una barrera establecida) & long filter (estoy en bullish market) quiero entrar en esta fecha. Mientras estoy bullish y la señal de oscilador temporal me dice que esta bien. Me estoy cubriendo de hacer una mala entrada si no me asegurar de un bullish market

#test the indicators and the SIGNALSS
test_init <- applyIndicators(strategy.st, mktdata = OHLC(SPY))
head(test_init)

test <- applySignals(strategy = strategy.st, mktdata = test_init) #needs to pass the already calculated indicators
head(test)

colnames(test_init)
strategy <- try(getStrategy(strategy.st))

#n the previous exercise, you approximated a sigFormula signal by comparing the value of two other signals. In this final exercise, you will take this one step further by using the sigFormula() function to generate a sigFormula signal.

# The goal of this exercise is simple. You want to enter into a position when both longfilter and longthreshold become true at the same time. The idea is this: You don't want to keep entering into a position for as long as conditions hold true, but you do want to hold a position when there's a pullback in an uptrending environment.
# 
# Writing a sigFormula function is as simple as writing the argument of an "if statement" in base R inside the formula() function. In this case, you want to create a signal labeled longentry, which is true when both longfilter and longthreshold cross over to true at the same time.
# 
# Once you complete this exercise, you will have a complete survey of how signals work in quantstrat!
# Add a sigFormula signal to your code specifying that both longfilter and longthreshold must be TRUE, label it longentry
add.signal(strategy.st, name = "sigFormula",
           
           # Specify that longfilter and longthreshold must be TRUE
           arguments = list(formula = "longfilter & longthreshold", 
                            
                            # Specify that cross must be TRUE
                            cross = TRUE),
           
           # Label it longentry
           label = "longentry")


#rules
# should take an enter/exit position given a well defined signal (that only returns true, this is referenced in the values the rule will take)








