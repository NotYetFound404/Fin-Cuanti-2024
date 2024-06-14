#-----------------
alignSymbols <- function(symbols, env=.GlobalEnv) {
  # This is a simplified version of qmao::alignSymbols()
  if (length(symbols) < 2) 
    stop("Must provide at least 2 symbols")
  if (any(!is.character(symbols))) 
    stop("Symbols must be vector of character strings.")
  ff <- get(symbols[1],env=env)
  for (sym in symbols[-1]) {
    tmp.sym <- get(sym,env=env)
    ff <- merge(ff, tmp.sym, all=FALSE)
  }
  for (sym in symbols) {
    assign(sym,ff[,grep(sym, colnames(ff))], env=env)
  }
  symbols
}
#-----------------
#https://www.r-bloggers.com/2021/01/example-of-pairs-trading/
library(tidyverse)
library(tseries)
library(quantmod)
# Symbols of interest
mySymbols <- c('AMZN', 'NFLX')
from <- "2020-7-01"
to <- "2020-11-13"
getSymbols(mySymbols, from = from, to = to, src = "yahoo", adjust = TRUE)
#alignSymbols(mySymbols)

#get the log of the close prices
AMZN.cl <- log(Cl(AMZN))
NFLX.cl <- log(Cl(NFLX))

#left is  amazn and right is  nflx !!
correlation <-cor(AMZN.cl, NFLX.cl)

#make a regresion of netflix and amazon without intercept
m<-lm(AMZN.cl ~ NFLX.cl- 1) 

beta<-coef(m)[[1]]
reg.residuals <-residuals(m)

pvalue<-adf.test(reg.residuals)$p.value

if((pvalue<=0.05)*(correlation>0.95)){
  print("The pair is cointegrated (ALPHA < 0.5)&(CORR > 0.95)")
}


spread = AMZN.cl-beta*NFLX.cl
z_score = (spread-mean(spread))/sd(spread)
#plot(z_score)
# Define thresholds for trading signals
threshold <- 1

# Initialize signal vectors
long_signal <- rep(0, length(z_score))
short_signal <- rep(0, length(z_score))

# Generate signals based on z-score
#Compramos el spread cuando esta bajo. Long cuando este por debajo del threshold
#vendemos el spread cuando esta alto. Short cuando este por encima del threshold

for (i in 1:length(z_score)) {
  if (z_score[i] > threshold) {
    print(z_score[i])
    short_signal[i] <- 1  #  Short signal
    #Amazon > netflix
    #shorteo amazon y compro netflix
    
    #we sell amazon and we buy netflix
  }
}
# short_signal
# sum(short_signal)


for (i in 1:length(z_score)) {
   if (z_score[i] < -threshold) {
     print(z_score[i])
     long_signal[i] <- 1  # Long signal
     #Amazon < netflix
     #shorteo netflix y compro amazon
     
     #we sell netflix and buy amazon
     }
  
}
# long_signal
# sum(sum(short_signal))



# Create a data frame for plotting
spread_data <- data.frame(
  AMZN = Cl(AMZN),
  NFLX = Cl(NFLX),
  Date = index(spread),
  log.Spread = as.numeric(spread),
  log.Z_Score = as.numeric(z_score),
  Long_Signal = long_signal,
  Short_Signal = short_signal
)

#si long signal es 1 -> compramos amzn y vendemos nflx
#si short signal es 1 -> vendemos amzn y compramos nflx


#add the signals the actual data
spread_data <- spread_data %>% mutate(
  actual.spread = NFLX.Close-AMZN.Close,
  AMZN.signal = case_when(
    Long_Signal == 1 ~ "Buy",
    Short_Signal == 1 ~ "Sell",
    TRUE ~ "Hold"
  ),
  NFLX.signal = case_when(
    Long_Signal == 1 ~ "Sell",
    Short_Signal == 1 ~ "Buy",
    TRUE ~ "Hold"
  )
)

# Plot the log zscore with the 1 and - 1 threshold
ggplot(spread_data, aes(x = Date, y = log.Z_Score)) +
  geom_line() +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -threshold, linetype = "dashed", color = "red") +
  labs(x = "Date", y = "Log Z-Score", title = "Log Z-Score of Amazon and Netflix") +
  theme_minimal()

# Plot the log spread with trading signals
ggplot(spread_data, aes(x = Date)) +
  geom_line(aes(y = log.Spread, color = "Spread")) +
  geom_point(aes(y = log.Spread, color = "Long", shape = "Long"), data = subset(spread_data, Long_Signal == 1), size = 2) +
  geom_point(aes(y = log.Spread, color = "Short", shape = "Short"), data = subset(spread_data, Short_Signal == 1), size = 2) +
  scale_color_manual(values = c("Spread" = "blue", "Long" = "green", "Short" = "red")) +
  scale_shape_manual(values = c("Long" = 16, "Short" = 17)) +
  labs(title = "Log Spread Amazon - Netflix with Trading Signals", y = "Log Spread", x = "Date") +
  theme_minimal()

# Plot the actual spread with trading signals
ggplot(spread_data, aes(x = Date)) +
  geom_line(aes(y = actual.spread, color = "Spread")) +
  geom_point(aes(y = actual.spread, color = "Long", shape = "Long"), data = subset(spread_data, Long_Signal == 1), size = 2) +
  geom_point(aes(y = actual.spread, color = "Short", shape = "Short"), data = subset(spread_data, Short_Signal == 1), size = 2) +
  scale_color_manual(values = c("Spread" = "blue", "Long" = "green", "Short" = "red")) +
  scale_shape_manual(values = c("Long" = 16, "Short" = 17)) +
  labs(title = "Actual Spread Amazon - Netflix with Trading Signals", y = "Actual Spread", x = "Date") +
  theme_minimal()


# Plot the prices signals for amazon
ggplot(spread_data, aes(x = Date)) +
  geom_line(aes(y = AMZN.cl, color = "AMZN")) +
  geom_point(aes(y = AMZN.cl, color = AMZN.signal, shape = AMZN.signal), size = 2) +
  scale_color_manual(values = c("AMZN" = "blue", "Buy" = "green", "Sell" = "red", "Hold" = "black")) +
  scale_shape_manual(values = c("Buy" = 16, "Sell" = 17, "Hold" = 19)) +
  labs(title = "Amazon Prices with Trading Signals", y = "Amazon Price", x = "Date") +
  theme_minimal()

#plot the prices for netflix
ggplot(spread_data, aes(x = Date)) +
  geom_line(aes(y = NFLX.cl, color = "NFLX")) +
  geom_point(aes(y = NFLX.cl, color = NFLX.signal, shape = NFLX.signal), size = 2) +
  scale_color_manual(values = c("NFLX" = "blue", "Buy" = "green", "Sell" = "red", "Hold" = "black")) +
  scale_shape_manual(values = c("Buy" = 16, "Sell" = 17, "Hold" = 19)) +
  labs(title = "Netflix Prices with Trading Signals", y = "Netflix Price", x = "Date") +
  theme_minimal()


