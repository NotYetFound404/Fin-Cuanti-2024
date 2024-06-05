#https://www.r-bloggers.com/2021/01/example-of-pairs-trading/
library(tidyverse)
library(tseries)
library(quantmod)
# Symbols of interest
mySymbols <- c('NFLX', 'AMZN')

# Retrieve stock data
myStocks <- lapply(mySymbols, function(x) {
  getSymbols(x, 
             from = "2020-01-01", 
             to = "2021-01-03",
             periodicity = "daily",
             auto.assign = FALSE)
})

# Assign names to the stocks
names(myStocks) <- mySymbols

# Extract close prices
closePrices <- lapply(myStocks, Cl)
closePrices <- do.call(merge, closePrices)
names(closePrices) <- sub("\\.Close", "", names(closePrices))

# Display first few rows of the close prices
head(closePrices)

# Train and test data
train <- log(closePrices[1:220])
test <- log(closePrices[221:252])
left_side<-NULL
right_side<-NULL
correlation<-NULL
beta<-NULL
pvalue<-NULL
for (i in 1:length(mySymbols)) {
  for (j in 1:length(mySymbols)) {
    if (i>j) {
      left_side<-c(left_side, mySymbols[i])
      right_side<-c(right_side, mySymbols[j])
      correlation<-c(correlation, cor(train[,mySymbols[i]], train[,mySymbols[j]]))
      
      # linear regression withoout intercept
      m<-lm(train[,mySymbols[i]]~train[,mySymbols[j]]-1)
      beta<-c(beta, as.numeric(coef(m)[1]))
      
      # get the mispricings of the spread
      sprd<-residuals(m)
      
      # adf test
      pvalue<-c(pvalue, adf.test(sprd, alternative="stationary", k=0)$p.value)
      
    }
  }
  
}
df<-data.frame(left_side, right_side, correlation, beta, pvalue)
mypairs<-df%>%filter(pvalue<=0.05, correlation>0.95)%>%arrange(-correlation)
mypairs

#trading on my pairs
mypairs_reg <- mypairs %>%
  rowwise() %>%
  mutate(
    spread = list(train[,left_side]-beta*train[,right_side]),
    z_score = list((spread-mean(spread))/sd(spread))
  )
mypairs_reg
plot(mypairs_reg$z_score[[1]])
