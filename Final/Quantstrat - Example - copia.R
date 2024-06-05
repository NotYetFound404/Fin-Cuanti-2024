require("quantstrat")
.blotter <- new.env()
.strategy <- new.env()

startDate <- "2008-01-01"
endDate <- "2019-12-31"
symbols <- c("EEM", "SPY", "TLT")

Sys.setenv(TZ = "UTC")

getSymbols(
  symbols,
  index.class = c("POSIXct", "POSIXt"),
  from = startDate,
  to = endDate,
  adjust = TRUE,
  src = "yahoo",
  env = .blotter
  )


initDate <- "2008-01-01"
initEq <- 2500
currency("USD")
stock(symbols, currency = "USD", multiplier = 1)

quantstrat::rm.strat("BBandStrat")
initPortf(
  name = "BBandStrat",
  symbols = symbols,
  initDate = initDate,
  )
initAcct(
  name = "BBandStrat",
  portfolios = "BBandStrat",
  initDate = initDate,
  initEq = initEq
  )
initOrders(
  portfolio = "BBandStrat",
  symbols = symbols, 
  initDate = initDate
  )
strategy("bbands", store = TRUE)
add.indicator(
  strategy = "bbands",
  name = "BBands",
  arguments = list(HLC = quote(HLC(mktdata)),
                     maType = "SMA"),
  label = "BBands"
  )

add.signal(
  strategy = "bbands",
  name = "sigCrossover",
  arguments = list(columns = c("Close", "up"),
                          relationship = "gt"),
  label = "Cl.gt.UpperBand"
  )
add.signal(
  strategy = "bbands",
  name = "sigCrossover",
  arguments = list(columns = c("Close", "dn"),
                   relationship = "lt"),
  label = "Cl.lt.UpperBand"
)



