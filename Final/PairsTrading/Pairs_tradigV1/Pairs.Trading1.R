#https://www.r-bloggers.com/2021/01/example-of-pairs-trading/
library(tidyverse)
library(tseries)
library(quantmod)
# Symbols of interest
mySymbols <- c('NFLX', 'AMZN', 'GOOGL')

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


#Plot of the training data of netflix and amazon
plot(train$NFLX, type="l", col="blue", ylab="Log Prices", xlab="Time", main="Training Data of Netflix and Amazon")
lines(train$AMZN, col="red")
legend("topleft", legend=c("Netflix", "Amazon"), col=c("blue", "red"), lty=1)

#Focusing only on the amazon-netflix spread
myspread<-train[,"AMZN"]-0.7988209*train[,"NFLX"]
plot(myspread, main = "NFLX vs AMZN")

#plot with ggplot my spread and add 1 sd above and below 0


ggplot(data.frame(myspread), aes(y=AMZN, x = index(myspread)))+
  geom_line()+
  geom_hline(yintercept = sd(myspread), linetype = "dashed", color = "red") +
  geom_hline(yintercept = -sd(myspread), linetype = "dashed", color = "blue") +
  labs(title="Spread between Netflix and Amazon", x="Time", y="Spread")+
  theme_minimal()

#si esta por encima de la linea roja -> "vendemos el spread" = Vendemos netflix y compramos amazon
#si esta por debajo de la linea azul -> "compramos el spread" = compramos netflix y vendemos amazon

#Calculating the z-score
zscore<-(myspread-mean(myspread))/sd(myspread)
plot(zscore)


ggplot(data.frame(zscore), aes(y=AMZN, x = index(myspread)))+
  geom_line()+
  geom_hline(yintercept = sd(zscore), linetype = "dashed", color = "red") +
  geom_hline(yintercept = -sd(zscore), linetype = "dashed", color = "blue") +
  labs(title="zscore", x="Time", y="Spread")+
  theme_minimal()
