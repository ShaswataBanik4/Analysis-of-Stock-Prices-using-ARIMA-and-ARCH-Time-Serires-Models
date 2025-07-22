rm(list=ls())

amazon_data<- read.csv("C:/Users/User/OneDrive/Documents/Dissertation/MSFT(2000-2023).csv",header=T)

colnames(amazon_data)

adj=amazon_data$Adj.Close
adj
adf.test(((log(adj))))
adf.test((diff(log(adj))))
adf.test(diff(diff(log(adj))))
library(forecast)
plot(log(adj),type="l")
pacf(log(adj))
ma_series=na.omit(filter(log(adj),filter=c(1/1000,rep(1/500,each=499,times=1),1/1000)))
ma_series
acf((diff(diff(log(adj)))))
library(tseries)

adf.test((log(adj)))

auto.arima(log(adj))