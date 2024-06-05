#----------
#https://blog.quantinsti.com/pair-trading-strategy-backtesting-using-quantstrat/
#https://timtrice.github.io/backtesting-strategies/index.html
#https://medium.com/@boniface.yogi/algotrading-with-r-quantstrat-763a44c9c3ab
#-------------
library(quantstrat)
#Initialization ----------

#%%%%%%%%%#
## Init. Step 1 ##
#%%%%%%%%%#

#set the currency and the stock we are interested
currency("USD")
stock("SPY",currency="USD",multiplier=1)
Sys.setenv(TZ="UTC") #setting up the timezone

#%%%%%%%%%#
## Init. Step 2 ##  
#%%%%%%%%%#

#Set the parameters
initDate <- "2007-01-02" #Date of initiation
from <- "2007-01-03" #Start date of the data set
to <- "2018-03-18" #End date of the data set
initEq <- 1000 #Initial equity
nFast <- 10
nSlow <- 30
orderqty <- 100

#Portfolio data  ----------

#%%%%%%%%%#
## Port. Step 1 ##  
#%%%%%%%%%#

# read in data 
getSymbols("SPY", from = from, to = to)

#%%%%%%%%%#
## Port. Step 2 ##  
#%%%%%%%%%#

#Initiate portfolio and account
strategy.st <- "luxor" #Name the strategy
account.st <- "luxor"
portfolio.st <- "luxor"

#%%%%%%%%%#
## Port. Step 3 ##  
#%%%%%%%%%#

#Initiate portfolio
initPortf(strategy.st,symbols = "SPY", initDate = initDate) #Initiate portfolio
#Initiate account
initAcct(strategy.st, portfolios = strategy.st, initDate = initDate, initEq = initEq) 
#Initiate account
initOrders(portfolio = strategy.st, initDate = initDate) 
#Store all the events in the strategy
strategy(strategy.st, store = TRUE)


#Define a strategy-----------
### indicators
add.indicator(strategy.st, name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = nFast
              ),
              label="nFast"
)

add.indicator(strategy.st, name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n = nSlow
              ),
              label="nSlow"
)
### signals

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="gte"
           ),
           label='long'
)

add.signal(strategy.st, name='sigCrossover',
           arguments = list(
             columns=c("nFast","nSlow"),
             relationship="lt"
           ),
           label='short'
)

### rules
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)
add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='market', 
                        orderqty=orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)
#...................
applyStrategy(strategy.st, portfolio.st)

#update
updatePortf(strategy.st)
updateAcct(strategy.st)
updateEndEq(strategy.st)

#Plot chart
chart.Posn(portfolio.st, "SPY")

#Performance
tStats <- tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=FALSE)

knitr::kable(t(tStats))


rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
charts.PerformanceSummary(rets, colorset = bluefocus)

#Revisar analysing returns 
#https://timtrice.github.io/backtesting-strategies/analyzing-results.html
