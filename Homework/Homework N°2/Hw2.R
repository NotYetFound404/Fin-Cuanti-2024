#------------
#previous work
#5 day grouping (using mean)
# periodicity <- 5
# df_weekly_df <- xts(df$TC, order.by = df$Date) %>% # Convert df to xts object
#   period.apply(., endpoints(., on = "days", k = periodicity), FUN = colMeans, na.rm = TRUE) %>% # Define a custom period for aggregation (5 days) %>%
#   data.frame(date = index(.), TC = coredata(.))
# #plot 5 day grouping with ggplot
# ggplot(df_weekly_df, aes(x=date, y=TC)) + geom_line() + labs(title = "Tipo de cambio interbancario (agrupado por 5 días)", x = "Fecha", y = "Tipo de cambio") + theme_minimal()

#Manual OHLC weekly
# weekly_data <- df %>%
#   group_by(week = format(Date, "%Y-%U")) 
# weekly_ohlc <- weekly_data %>%
#   summarise(
#     open = first(TC, na.rm = TRUE),
#     high = max(TC, na.rm = TRUE),
#     low = min(TC, na.rm = TRUE),
#     close = last(TC, na.rm = TRUE)
#   )
#------------
#libraries
library(lubridate)
library(tidyverse)
library(quantmod)
library(ggplot2)

library(tseries)
library(forecast)

library(rugarch)
library(tidyquant)
#0--------------
#support functions
# Define the function to translate the date and convert it to a Date type
convert_date <- function(date_string) {
  # Translate the Spanish month abbreviations to English
  date_string <- gsub("Ene", "Jan", date_string)
  date_string <- gsub("Feb", "Feb", date_string)
  date_string <- gsub("Mar", "Mar", date_string)
  date_string <- gsub("Abr", "Apr", date_string)
  date_string <- gsub("May", "May", date_string)
  date_string <- gsub("Jun", "Jun", date_string)
  date_string <- gsub("Jul", "Jul", date_string)
  date_string <- gsub("Ago", "Aug", date_string)
  date_string <- gsub("Set", "Sep", date_string)
  date_string <- gsub("Oct", "Oct", date_string)
  date_string <- gsub("Nov", "Nov", date_string)
  date_string <- gsub("Dic", "Dec", date_string)
  
  # Parse the modified date string into a Date object
  return(date_string)
}
#1------------------
#Load data + Data cleaning

#read tc interbancario
df <- read.csv("Homework/Homework N°2/TC_interbancarioSBS_venta_BCRP.csv", header = TRUE)
#delete first row of df
df <- df[-1,]
#rename colums to TC and date
colnames(df) <- c("date", "TC")
# Apply the function to the 'date' column
df$date <- sapply(df$date, convert_date)
# convert to date, turn numeric, filter date from 2007 onwards
df <- df %>% 
  mutate(Date = dmy(date),
         TC = ifelse(TC == "n.d", NA, as.numeric(TC))) %>%
  filter(Date >= "2007-01-01", complete.cases(TC))
#glimpse(df)

#plot with ggplot
# ggplot(df, aes(x=Date, y=TC)) + geom_line() + labs(title = "Tipo de cambio interbancario", x = "Fecha", y = "Tipo de cambio") + theme_minimal()
df.xts <- xts(df$TC, order.by = df$Date)
#plot with quantmod
chart_Series(df.xts)

daily.Ret <- df %>%
  tq_transmute(
    select = TC,
    mutate_fun = periodReturn,
    period = "daily",
    type = "log"
  )
daily.Ret.xts <- xts(daily.Ret$daily.returns, order.by = df$Date)
#----------------
#Check stationarity for daily returns xts
plot(daily.Ret.xts)
adf.test(daily.Ret.xts)

#Asumiendo que el mejor garch es 1,1
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)))
garch_fit <- ugarchfit(spec = garch_spec, data = daily.Ret.xts)

simulations <- ugarchsim(garch_fit, n.sim = 10, m.sim = 2)
#nsim is the steps of the simulation, m.sim is the number of simulations

test <- simulations@simulation$seriesSim
#each col is a mc simulation
#each row is the time step

# Extract positive returns
positive_returns <- test[test > 0]
# Compute the mean of the positive returns
mean_positive <- mean(positive_returns)
mean_positive
# Compute the 40th quantile of the positive returns
quantile_40 <- quantile(positive_returns, probs = c(0.5, 0.75, 0.9, 0.95, 0.99))
quantile_40


#x------------------
# #turning data to specified periodicity
# #turn and plot using OHLC data
# weekly.ohlc <- to.period(df.xts, period = "weeks", OHLC = TRUE)
# monthly.ohlc <- to.period(df.xts, period = "months", OHLC = TRUE)
# quarterly.ohlc <- to.period(df.xts, period = "quarters", OHLC = TRUE)
# Semester.ohlc <- to.period(df.xts, period = "months", OHLC = TRUE, k = 6)
# #pltoing
# chart_Series(weekly.ohlc)
# chart_Series(monthly.ohlc)
# chart_Series(quarterly.ohlc)
# chart_Series(Semester.ohlc)
# 
# #using days specified and returning the average price in said period
# weekly.5days.mean <- period.apply(df.xts, endpoints(df.xts, on = "days", k = 5), FUN = colMeans)
# monthly.21days.mean <- period.apply(df.xts, endpoints(df.xts, on = "days", k = 21), FUN = colMeans)
# quarterly.63days.mean <- period.apply(df.xts, endpoints(df.xts, on = "days", k = 63), FUN = colMeans)
# semesterly.126days.mean <- period.apply(df.xts, endpoints(df.xts, on = "days", k =126), FUN = colMeans)
# #plotting
# chart_Series(weekly.5days.mean)
# chart_Series(monthly.21days.mean)
# chart_Series(quarterly.63days.mean)
# chart_Series(semesterly.126days.mean)
# 
# #garch fit and simulation (1000)
# 
# #1. Calculate log returns from mean prices of weekly.5days.mean
# # quarterly.63days.mean.ret <- diff(log(quarterly.63days.mean))
# # quarterly.63days.mean.ret <- quarterly.63days.mean.ret[-1,]
# 
# # stationarity check
# # plot(quarterly.63days.mean.ret)
# # adf.test(quarterly.63days.mean.ret)
# 
# # ACF plot of log returns
# #acf(quarterly.63days.mean.ret, main = "Autocorrelation Function (ACF) of Log Returns")
# 
# # PACF plot of log returns
# #pacf(quarterly.63days.mean.ret, main = "Partial Autocorrelation Function (PACF) of Log Returns")
# 
# 
# # Fit GARCH(1,1) model (example)
# # garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
# #                          mean.model = list(armaOrder = c(0, 0)))
# # garch_fit <- ugarchfit(spec = garch_spec, data = quarterly.63days.mean.ret)
# # # Volatility clustering plot
# # plot(garch_fit, which = "all")
# 
# 
# #Model Selection Criteria
# # infocriteria(garch_fit)
# # residuals(garch_fit)
# # ?infocriteria
# 
# ## Sensitivity analysis (varying GARCH orders) #necesita correr el modelo con grid search para tener mejores fits
# # garch_orders <- c(1:3)
# # for (p in garch_orders) {
# #   for (q in garch_orders) {
# #     garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
# #                              mean.model = list(armaOrder = c(0, 0)))
# #     garch_fit <- ugarchfit(spec = garch_spec, data = quarterly.63days.mean.ret)
# #     print(paste("AIC for GARCH(", p, ",", q, "):", infocriteria(garch_fit)[1]))
# #   }
# # }
# 
# #foward sim
# # Fit GARCH(1,1) model (asumming best model)
# # garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
# #                          mean.model = list(armaOrder = c(0, 0)))
# # garch_fit <- ugarchfit(spec = garch_spec, data = log_returns)
# # 
# # # Perform forward simulation
# # n_steps <- 50  # Number of steps for simulation
# # simulated_returns <- ugarchsim(garch_fit, n.sim = n_steps)
# 
# 
# # str(simulated_returns)
# # # Convert the simulated returns to prices
# # simulated_prices <- exp(cumsum(simulated_returns))
# # str(simulated_prices)
# # #plot
# # plot(simulated_prices, type = "l", col = "blue", lwd = 2, main = "Simulated Prices from GARCH(1,1) Model", xlab = "Time", ylab = "Price")
# 
# 
# #Historic data summary
# #para el quarterly data
# # appreciation <- quarterly.63days.mean.ret[quarterly.63days.mean.ret>0]
# # 
# # hist(appreciation)
# # mean(appreciation)
# # quantile(appreciation, probs = c(0.5, 0.75, 0.9, 0.95, 0.99))
# # 
# # depreciation <- quarterly.63days.mean.ret[quarterly.63days.mean.ret<0]
# # hist(depreciation)
# # mean(depreciation)
# # quantile(depreciation, probs = c(1-0.5, 1- 0.75, 1- 0.9, 1- 0.95, 1-0.99))
# 
