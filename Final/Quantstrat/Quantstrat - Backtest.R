library(quantstrat)
.blotter <- new.env()
.strategy <- new.env()
currency("USD")
stock("SPY",currency="USD",multiplier=1)
Sys.setenv(TZ="UTC") #setting up the timezone
initDate <- "2007-01-02" #Date of initiation
from <- "2007-01-03" #Start date of the data set
to <- "2018-03-18" #End date of the data set
initEq <- 1000 #Initial equity
nFast <- 10
nSlow <- 30
orderqty <- 100
getSymbols("SPY", from = from, to = to)
strategy.st <- "luxor" #Name the strategy
account.st <- "luxor"
portfolio.st <- "luxor"
quantstrat::rm.strat(strategy.st)
initPortf(strategy.st,symbols = "SPY", initDate = initDate) #Initiate portfolio
initAcct(strategy.st, portfolios = strategy.st, initDate = initDate, initEq = initEq) 
initOrders(portfolio = strategy.st, initDate = initDate) 
strategy(strategy.st, store = TRUE)

sma_values <- SMA(Cl(get("SPY")), n = 10) 

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
applyStrategy(strategy.st, portfolio.st)
updatePortf(strategy.st)
updateAcct(strategy.st)
updateEndEq(strategy.st)
chart.Posn(portfolio.st, "SPY")
tStats <- tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=FALSE)
knitr::kable(t(tStats))
rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
charts.PerformanceSummary(rets, colorset = bluefocus)

