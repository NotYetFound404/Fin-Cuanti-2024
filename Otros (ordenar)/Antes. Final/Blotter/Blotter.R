# ls(envir=.blotter)
# View(.blotter$portfolio.buyHold)
# .blotter$account.buyHold

library(blotter)
library(quantstrat)
options("getSymbols.warning4.0"=FALSE)
from ="2008-01-01"
to ="2012-12-31"
symbols = c("AAPL", "IBM")
currency("USD")
getSymbols(symbols, from=from, to=to, 
           adjust=TRUE)
stock(symbols, currency="USD", multiplier=1)
initEq=10^6

# Ensure new environment is clean
.blotter <- new.env()
.strategy <- new.env()

rm("account.buyHold",pos=.blotter)
rm("portfolio.buyHold",pos=.blotter)


initPortf("buyHold", symbol=symbols)
initAcct("buyHold", portfolios = "buyHold",
         initEq = initEq)


Apple.Buy.Date <- first(time(AAPL))
Apple.Buy.Price <- as.numeric(Cl(AAPL[Apple.Buy.Date,]))
Apple.Sell.Date <- last(time(AAPL))
Apple.Sell.Price <- as.numeric(Cl(AAPL[Apple.Sell.Date,]))
Apple.Qty <- trunc(initEq/(2*Apple.Buy.Price))

IBM.Buy.Date <- first(time(IBM))
IBM.Buy.Price <- as.numeric(Cl(IBM[IBM.Buy.Date,]))
IBM.Sell.Date <- last(time(IBM))
IBM.Sell.Price <- as.numeric(Cl(IBM[IBM.Sell.Date,]))
IBM.Qty <- trunc(initEq/(2*IBM.Buy.Price))


addTxn(Portfolio = "buyHold", 
       Symbol = "AAPL", 
       TxnDate = Apple.Buy.Date, 
       TxnQty = Apple.Qty,
       TxnPrice = Apple.Buy.Price,
       TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Buy.Date, 
       TxnQty = IBM.Qty,
       TxnPrice = IBM.Buy.Price,
       TxnFees = 0)


addTxn(Portfolio = "buyHold", 
       Symbol = "AAPL", 
       TxnDate = Apple.Sell.Date, 
       TxnQty = -Apple.Qty,
       TxnPrice = Apple.Sell.Price,
       TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Sell.Date, 
       TxnQty = -IBM.Qty,
       TxnPrice = IBM.Sell.Price,
       TxnFees = 0)

updatePortf(Portfolio = "buyHold")
updateAcct(name = "buyHold")
updateEndEq(Account = "buyHold")


chart.Posn("buyHold", Symbol = "AAPL")
chart.Posn("buyHold", Symbol = "IBM")


