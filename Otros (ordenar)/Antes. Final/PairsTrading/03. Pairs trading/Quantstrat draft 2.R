# Load the quantstrat package
library(quantstrat)

#ensure new env is clean
.blotter <- new.env()
.strategy <- new.env()
#setting up the timezone
Sys.setenv(TZ="UTC") 


# Create initdate, from, and to strings
initDate <- "1999-01-01"
from <- "2020-7-01"
to <- "2020-11-13"

#set stocks and currecy
symbols <- c("AMZN", "NFLX")

# Define your trade size and initial equity
tradesize <- 100000
initEq <- 100000


# Retrieve SPY from yahoo
getSymbols(symbols, from = from, to = to, src = "yahoo", adjust = TRUE)

currency = "USD"
#Use stock() to initialize SPY and set currency to USD
currency(currency) # Set the currency to USD 
for (symbol in symbols) {
  stock(symbol, currency = "USD", multiplier = 1)
}
# Define the names of your strategy, portfolio and account
strategy.st <- portfolio.st <- account.st <- "firststrat"
# Remove the existing strategy if it exists
rm.strat("firststrat")


# Initialize the portfolio, account, and orders
# Initialize the portfolio
initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = currency)
# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = currency, initEq = initEq)
# Initialize the orders
initOrders(portfolio.st, initDate = initDate)
# Store the strategy
strategy(strategy.st, store = TRUE)

#first check at what strategy holds
strategy <- getStrategy(strategy.st)
#View(strategy)

#get the log of the close prices
AMZN.cl <- log(Cl(AMZN))
NFLX.cl <- log(Cl(NFLX))

# Define the log indicator function
log_indicator <- function(x) {
  log(x)
}

log_corr <- function(log_AMZN, log_NFLX) {
  # Calculate the difference between log_AMZN and log_NFLX
  corr <- cor(log_AMZN,log_NFLX)
  
  # Return the difference value as the custom indicator
  return(corr)
}




# Add a Log of amazon
strategy.st <- add.indicator(strategy = strategy.st, 
                             name = "log_indicator", 
                             arguments = list(x = quote(Cl(AMZN))), 
                             label = "log_AMZN")
# Add a log of nflx
add.indicator(strategy = strategy.st,

              # Add the log function
              name = "log_indicator",

              # pass the closing prices of amzn
              arguments = list(x = quote(Cl(NFLX))),

              # Label your indicator
              label = "log_NFLX")
#Add the correlation
add.indicator(strategy = strategy.st, name = "runCor",
              arguments = list(x = quote(Cl(AMZN)), y = quote(Cl(NFLX)), n = 3),
              label = "runCorrelation_3")

#Add the log correlation
strategy.st <- add.indicator(strategy = strategy.st, 
                             name = "log_corr",
                             arguments = list(log_AMZN = quote(log_AMZN), log_NFLX = quote(log_NFLX)),
                             label = "custom_diff")




strategy <- getStrategy(strategy.st)
#View(strategy)
strategy$indicators
combined_data <- merge(Cl(AMZN), Cl(NFLX))
test1<-applyIndicators(strategy = strategy.st, mktdata =combined_data)




# Add a 50-day SMA indicator to strategy.st



# Add an RSI 3 indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the RSI 3 function
              name = "RSI", 
              
              # Create a lookback period
              arguments = list(price = quote(Cl(mktdata)), n = 3), 
              
              # Label your indicator RSI_3
              label = "RSI_3")

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

# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))

# Subset your data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-01/2013-09-05"]
test_subset



# Add a sigComparison which specifies that SMA50 must be greater than SMA200, call it longfilter
add.signal(strategy.st, name = "sigComparison", 
           
           # We are interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"), 
                            
                            # Particularly, we are interested when the SMA50 is greater than the SMA200
                            relationship = "gt"),
           
           # Label this signal longfilter
           label = "longfilter")


# Add a sigCrossover which specifies that the SMA50 is less than the SMA200 and label it filterexit
add.signal(strategy.st, name = "sigCrossover",
           
           # We're interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"),
                            
                            # The relationship is that the SMA50 crosses under the SMA200
                            relationship = "lt"),
           
           # Label it filterexit
           label = "filterexit")


# Implement a sigThreshold which specifies that DVO_2_126 must be less than 20, label it longthreshold
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





# Add a sigFormula signal to your code specifying that both longfilter and longthreshold must be TRUE, label it longentry
add.signal(strategy.st, name = "sigFormula",
           
           # Specify that longfilter and longthreshold must be TRUE
           arguments = list(formula = "longfilter & longthreshold", 
                            
                            # Specify that cross must be TRUE
                            cross = TRUE),
           
           # Label it longentry
           label = "longentry")

# Create your dataset: test
test_init <- applyIndicators(strategy.st, mktdata = OHLC(SPY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)
head(test)
tail(test)



#check strategy second time
strategy <- getStrategy(strategy.st)
# View(strategy)


colnames(test)
#sigCol uses the column names of the signals from above
#test$filterexit[test$filterexit == TRUE]
#todas las señales deberían ser 1 o 0 (true or false) y eso se pone para sigval


# Fill in the rule's type as exit
add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")
#si un rule es de tipo exit, puedes "cerrar la posicion" usando orderqty = all
#replace true= si es que hay una señal solo se ejecuta 1 señal y se cierran todas las otras señales (no se ejeutan). false, se mantienen las señales aun se pueden ejecutar si hay mas de 1 señal en el dia (casi siempre se prefiere false)
#prefer = "Open" es para que se ejecute en el precio de apertura del siguiente dia. Es decir ni bien detecte la señal deseo ejecutarla lo mas pronto posible. Otras opciones son open, high, low, close

# Fill in the replace argument in add.rule()
add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "thresholdexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")
# Create an entry rule of 1 share when all conditions line up to enter into a position

add.rule(strategy.st, name = "ruleSignal", 
         
         # Use the longentry column as the sigcol
         arguments=list(sigcol = "longentry", 
                        
                        # Set sigval to TRUE
                        sigval = TRUE, 
                        
                        # Set orderqty to 1
                        orderqty = 1,
                        
                        # Use a market type of order
                        ordertype = "market",
                        
                        # Take the long orderside
                        orderside = "long",
                        
                        # Do not replace other signals
                        replace = FALSE, 
                        
                        # Buy at the next day's opening price
                        prefer = "Open"),
         
         # This is an enter type rule, not an exit
         type = "enter")

#order sizing functions ??

#run strategy
# Use applyStrategy() to apply your strategy. Save this to out
strategy <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update your portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update your account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)

tstats <- tradeStats(Portfolios = portfolio.st)
tstats$Profit.Factor
tstats$Percent.Positive

#viz strategy

chart.Posn(portfolio.st, Symbol = "SPY")

#add the indicators to the plot
sma50 <- SMA(Cl(SPY), n = 50)
sma200 <- SMA(Cl(SPY), n = 200)
dvo <- DVO(HLC(SPY), navg = 2, percentlookback = 126)

chart.Posn(portfolio.st, Symbol = "SPY")
add_TA(sma50, on = 1, col = "blue")
add_TA(sma200, on = 1, col = "red")
add_TA(dvo)

zoom_Chart("2007-08/2007-12")
schart.ME(portfolio.st, Symbol = "SPY")


#View(.blotter)
portpl <- .blotter$portfolio.firststrat$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric=FALSE)

# Get instrument returns
instrets <- PortfReturns(Portfolios = portfolio.st, Account = account.st)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)
