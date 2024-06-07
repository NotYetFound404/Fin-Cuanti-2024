library(quantstrat)
.blotter <- new.env()
.strategy <- new.env()
currency("USD")
stock("SPY",currency="USD",multiplier=1)
Sys.setenv(TZ="UTC") #setting up the timezone
initDate <- "2018-11-01" #Date of initiation
from <- "2018-11-01" #Start date of the data set
to <- "2018-12-31" #End date of the data set
initEq <- 1000 #Initial equity
nFast <- 5
nSlow <- 15
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

#Prueba para ver como funciona el chart.Posn para sacar los signals
# test.Port <- getPortfolio(portfolio.st)
# Trades = test.Port$symbols[["SPY"]]$txn$Txn.Qty
# Buys = test.Port$symbols[["SPY"]]$txn$Txn.Price[which(Trades > 0)]
# Sells = test.Port$symbols[["SPY"]]$txn$Txn.Price[which(Trades <0)]
# chart_Series(SPY, name = "SPY", TA = NULL)
# add_TA(Buys, pch = 2, type = "p", col = "green", on = 1,cex = 4)
# add_TA(Sells, pch = 6, type = "p", col = "red", on = 1, cex = 4)






tStats <- tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=FALSE)
knitr::kable(t(tStats))
rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
charts.PerformanceSummary(rets, colorset = bluefocus)

