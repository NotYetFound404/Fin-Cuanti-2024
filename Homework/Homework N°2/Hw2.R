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
#2------------------
#turning data to specified periodicity
#turn and plot using OHLC data
weekly.ohlc <- to.period(df.xts, period = "weeks", OHLC = TRUE)
monthly.ohlc <- to.period(df.xts, period = "months", OHLC = TRUE)
quarterly.ohlc <- to.period(df.xts, period = "quarters", OHLC = TRUE)
Semester.ohlc <- to.period(df.xts, period = "months", OHLC = TRUE, k = 6)
#pltoing
chart_Series(weekly.ohlc)
chart_Series(monthly.ohlc)
chart_Series(quarterly.ohlc)
chart_Series(Semester.ohlc)

#using days specified and returning the average price in said period
weekly.5days.mean <- period.apply(df.xts, endpoints(df.xts, on = "days", k = 5), FUN = colMeans)
monthly.21days.mean <- period.apply(df.xts, endpoints(df.xts, on = "days", k = 21), FUN = colMeans)
quarterly.63days.mean <- period.apply(df.xts, endpoints(df.xts, on = "days", k = 63), FUN = colMeans)
semesterly.126days.mean <- period.apply(df.xts, endpoints(df.xts, on = "days", k =126), FUN = colMeans)
#plotting
chart_Series(weekly.5days.mean)
chart_Series(monthly.21days.mean)
chart_Series(quarterly.63days.mean)
chart_Series(semesterly.126days.mean)

#garch fit and simulation (1000)

