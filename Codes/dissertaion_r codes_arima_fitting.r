rm(list=ls())

data1<- read.csv("C:/Users/User/OneDrive/Documents/Dissertation/AMZN_data_1999_2022.csv",header=T)
data2<-read.csv("C:/Users/User/OneDrive/Documents/Dissertation/MSFT(2000-2023).csv",header=T)
data3<-read.csv("C:/Users/User/OneDrive/Documents/Dissertation/NFLX.csv",header=T)

colnames(data1)

closing_price_amazon=log(data1$Close)
opening_price_amazon=log(data1$Open)
closing_price_microsoft=log(data2$Close)
opening_price_microsoft=log(data2$Open)
closing_price_netflix=log(data3$Close)
opening_price_netflix=log(data3$Open)

n1=length(closing_price_amazon)
n1
n2=length(closing_price_microsoft)
n2
n3=length(closing_price_netflix)
n3

tl_amazon=round(n1*0.8)
tl_microsoft=round(n2*0.8)
tl_netflix=round(n3*0.8)

tsl_amazon=n1-tl_amazon
tsl_microsoft=n2-tl_microsoft
tsl_netflix=n3-tl_netflix


library(tseries)
library(forecast)

####Arima Fitting

###Amazon
##analysis on closing prices

adf.test((closing_price_amazon[1:tl_amazon]))
adf.test(diff(closing_price_amazon[1:tl_amazon]))

plot(((closing_price_amazon[1:tl_amazon])),type="l")
acf(diff(closing_price_amazon[1:tl_amazon]),main="ACF plotting on stationary series")
pacf((closing_price_amazon[1:tl_amazon]),main="PACF plotting on non-stationary series")
fit1=arima((closing_price_amazon[1:tl_amazon]),order=c(1,1,2))
fit1
plot(
	((closing_price_amazon[1:tl_amazon])),
	type="l",
	main="Graph of Log returns of Amazon Closing Stock Prices",
	xlab="Stock Trading Days in Training Data",
	ylab="Log returns of Stock Prices"
	)
lines(fitted(fit1),col="red")
legend(
	"topleft",
	legend=c("True Values","Fitted Values"),
	col=c("black","red"),
	lty=1
	)
summary(fit1)
fcast_ca=data.frame(forecast(fit1,h=tsl_amazon))
matplot(
	c(1:length(fcast_ca$Point.Forecast)),
	cbind(closing_price_amazon[(tl_amazon+1):n1],fcast_ca$Point.Forecast,fcast_ca$Lo.95,fcast_ca$Hi.95),
	col=c("black","red","blue","green"),
	main="Amazon Closing & forecasted Prices' Log returns' Graph(based on Testing data)",
	xlab="Stock Trading Days in Testing data",
	ylab="Log returns of Stock Prices",
	ylim=c(2,10)
	)
legend(
	"topleft",
	legend=c("True Values","Forecast Values","Upper 95% CLs","Lower 95% CLs"),
	col=c("black","red","green","blue"),
	lty=1
	)
e_ca=closing_price_amazon[(tl_amazon+1):n1]-fcast_ca$Point.Forecast
MAD_ca=mean(abs(e_ca));MAD_ca
MPE_ca=mean((e_ca/closing_price_amazon[(tl_amazon+1):n1])*100);MPE_ca
MAPE_ca=mean(abs(e_ca/closing_price_amazon[(tl_amazon+1):n1])*100);MAPE_ca


##analysis on opening prices

adf.test((opening_price_amazon[1:tl_amazon]))
adf.test(diff(opening_price_amazon[1:tl_amazon]))

plot(((opening_price_amazon[1:tl_amazon])),type="l")
acf(diff(opening_price_amazon[1:tl_amazon]),main="ACF plotting on stationary series")
pacf((opening_price_amazon[1:tl_amazon]),main="PACF plotting on non-stationary series")
fit1_o=arima((opening_price_amazon[1:tl_amazon]),order=c(1,1,1))
fit1_o
plot(
	((opening_price_amazon[1:tl_amazon])),
	type="l",
	main="Graph of Log returns of Amazon Opening Stock Prices",
	xlab="Stock Trading Days in Training Data",
	ylab="Log returns of Stock Prices"
	)
lines(fitted(fit1_o),col="red")
legend(
	"topleft",
	legend=c("True Values","Fitted Values"),
	col=c("black","red"),
	lty=1
	)
summary(fit1_o)
fcast_oa=data.frame(forecast(fit1_o,h=tsl_amazon))
matplot(
	c(1:length(fcast_oa$Point.Forecast)),
	cbind(opening_price_amazon[(tl_amazon+1):n1],fcast_oa$Point.Forecast,fcast_oa$Lo.95,fcast_oa$Hi.95),
	col=c("black","red","blue","green"),
	main="Amazon Opening & forecasted Prices' Log returns' Graph(based on Testing data)",
	xlab="Stock Trading Days in Testing data",
	ylab="Log returns of Stock Prices",
	)
legend(
	"topleft",
	legend=c("True Values","Forecast Values","Upper 95% CLs","Lower 95% CLs"),
	col=c("black","red","green","blue"),
	lty=1
	)
e_oa=opening_price_amazon[(tl_amazon+1):n1]-fcast_oa$Point.Forecast
MAD_oa=mean(abs(e_oa));MAD_oa
MPE_oa=mean((e_oa/opening_price_amazon[(tl_amazon+1):n1])*100);MPE_oa
MAPE_oa=mean(abs(e_oa/opening_price_amazon[(tl_amazon+1):n1])*100);MAPE_oa


###Microsoft
##analysis on closing prices

adf.test((closing_price_microsoft[1:tl_microsoft]))
adf.test(diff(closing_price_microsoft[1:tl_microsoft]))

plot(((closing_price_microsoft[1:tl_microsoft])),type="l")
acf(diff(closing_price_microsoft[1:tl_microsoft]),main="ACF plotting on stationary series")
pacf((closing_price_microsoft[1:tl_microsoft]),main="PACF plotting on non-stationary series")
fit2=arima(closing_price_microsoft[1:tl_microsoft],order=c(1,1,1))
fit2
plot(
	((closing_price_microsoft[1:tl_microsoft])),
	type="l",
	main="Graph of Log returns of Microsoft Closing Stock Prices",
	xlab="Stock Trading Days in Training Data",
	ylab="Log returns of Stock Prices"
	)
lines(fitted(fit2),col="red")
legend(
	"topleft",
	legend=c("True Values","Fitted Values"),
	col=c("black","red"),
	lty=1
	)
summary(fit2)
fcast_cm=data.frame(forecast(fit2,h=tsl_microsoft))
matplot(
	c(1:length(fcast_cm$Point.Forecast)),
	cbind(closing_price_microsoft[(tl_microsoft+1):n2],fcast_cm$Point.Forecast,fcast_cm$Lo.95,fcast_cm$Hi.95),
	col=c("black","red","blue","green"),
	main="Microsoft Closing & forecasted Prices' Log returns' Graph(based on Testing data)",
	xlab="Stock Trading Days in Testing data",
	ylab="Log returns of Stock Prices",
	ylim=c(2,8)
	)
legend(
	"topleft",
	legend=c("True Values","Forecast Values","Upper 95% CLs","Lower 95% CLs"),
	col=c("black","red","green","blue"),
	lty=1
	)
e_cm=closing_price_microsoft[(tl_microsoft+1):n2]-fcast_cm$Point.Forecast
MAD_cm=mean(abs(e_cm));MAD_cm
MPE_cm=mean((e_cm/closing_price_microsoft[(tl_microsoft+1):n2])*100);MPE_cm
MAPE_cm=mean(abs(e_cm/closing_price_microsoft[(tl_microsoft+1):n2])*100);MAPE_cm


##analysis on opening prices

adf.test((opening_price_microsoft[1:tl_microsoft]))
adf.test(diff(opening_price_microsoft[1:tl_microsoft]))

plot(((opening_price_microsoft[1:tl_microsoft])),type="l")
acf(diff(opening_price_microsoft[1:tl_microsoft]),main="ACF plotting on stationary series")
pacf((opening_price_microsoft[1:tl_microsoft]),main="PACF plotting on non-stationary series")
fit2_o=arima(closing_price_microsoft[1:tl_microsoft],order=c(1,1,4))
fit2_o
plot(
	((opening_price_microsoft[1:tl_microsoft])),
	type="l",
	main="Graph of Log returns of Microsoft Opening Stock Prices",
	xlab="Stock Trading Days in Training Data",
	ylab="Log returns of Stock Prices"
	)
lines(fitted(fit2_o),col="red")
legend(
	"topleft",
	legend=c("True Values","Fitted Values"),
	col=c("black","red"),
	lty=1
	)
summary(fit2_o)
fcast_om=data.frame(forecast(fit2_o,h=tsl_microsoft))
matplot(
	c(1:length(fcast_om$Point.Forecast)),
	cbind(opening_price_microsoft[(tl_microsoft+1):n2],fcast_om$Point.Forecast,fcast_om$Lo.95,fcast_om$Hi.95),
	col=c("black","red","blue","green"),
	main="Microsoft Opening & forecasted Prices' Log returns' Graph(based on Testing data)",
	xlab="Stock Trading Days in Testing data",
	ylab="Log returns of Stock Prices",
	ylim=c(3,8)
	)
legend(
	"topleft",
	legend=c("True Values","Forecast Values","Upper 95% CLs","Lower 95% CLs"),
	col=c("black","red","green","blue"),
	lty=1
	)
e_om=opening_price_microsoft[(tl_microsoft+1):n2]-fcast_om$Point.Forecast
MAD_om=mean(abs(e_om));MAD_om
MPE_om=mean((e_om/opening_price_microsoft[(tl_microsoft+1):n2])*100);MPE_om
MAPE_om=mean(abs(e_om/opening_price_microsoft[(tl_microsoft+1):n2])*100);MAPE_om



###Netflix
##analysis on closing prices

adf.test((closing_price_netflix[1:tl_netflix]))
adf.test(diff(closing_price_netflix[1:tl_netflix]))

plot(((closing_price_netflix[1:tl_netflix])),type="l")
acf(diff(closing_price_netflix[1:tl_netflix]),main="ACF plotting on stationary series")
pacf(closing_price_netflix[1:tl_netflix],main="PACF plotting on non-stationary series")
fit3=arima(closing_price_netflix[1:tl_netflix],order=c(1,1,0))
fit3
plot(
	((closing_price_netflix[1:tl_netflix])),
	type="l",
	main="Graph of Log returns of Netflix Closing Stock Prices",
	xlab="Stock Trading Days in Training Data",
	ylab="Log returns of Stock Prices"
	)
lines(fitted(fit3),col="red")
legend(
	"topleft",
	legend=c("True Values","Fitted Values"),
	col=c("black","red"),
	lty=1
	)
summary(fit3)
fcast_cn=data.frame(forecast(fit3,h=tsl_netflix))
matplot(
	c(1:length(fcast_cn$Point.Forecast)),
	cbind(closing_price_netflix[(tl_netflix+1):n3],fcast_cn$Point.Forecast,fcast_cn$Lo.95,fcast_cn$Hi.95),
	col=c("black","red","blue","green"),
	main="Netflix Closing & forecasted Prices' Log returns' Graph(based on Testing data)",
	xlab="Stock Trading Days in Testing data",
	ylab="Log returns of Stock Prices",
	ylim=c(2,9)
	)
legend(
	"topleft",
	legend=c("True Values","Forecast Values","Upper 95% CLs","Lower 95% CLs"),
	col=c("black","red","green","blue"),
	lty=1
	)
e_cn=closing_price_netflix[(tl_netflix+1):n3]-fcast_cn$Point.Forecast
MAD_cn=mean(abs(e_cn));MAD_cn
MPE_cn=mean((e_cn/closing_price_netflix[(tl_netflix+1):n3])*100);MPE_cn
MAPE_cn=mean(abs(e_cn/closing_price_netflix[(tl_netflix+1):n3])*100);MAPE_cn


##analysis on opening prices

adf.test((opening_price_netflix[1:tl_netflix]))
adf.test(diff(opening_price_netflix[1:tl_netflix]))

plot(((opening_price_netflix[1:tl_netflix])),type="l")
acf(diff(opening_price_netflix[1:tl_netflix]),main="ACF plotting on stationary series")
pacf(opening_price_netflix[1:tl_netflix],main="PACF plotting on non-stationary series")
fit3_o=arima(opening_price_netflix[1:tl_netflix],order=c(1,1,2))
fit3_o
plot(
	((opening_price_netflix[1:tl_netflix])),
	type="l",
	main="Graph of Log returns of Netflix Opening Stock Prices",
	xlab="Stock Trading Days in Training Data",
	ylab="Log returns of Stock Prices"
	)
lines(fitted(fit3_o),col="red")
legend(
	"topleft",
	legend=c("True Values","Fitted Values"),
	col=c("black","red"),
	lty=1
	)
summary(fit3_o)
fcast_on=data.frame(forecast(fit3_o,h=tsl_netflix))
matplot(
	c(1:length(fcast_on$Point.Forecast)),
	cbind(opening_price_netflix[(tl_netflix+1):n3],fcast_on$Point.Forecast,fcast_on$Lo.95,fcast_on$Hi.95),
	col=c("black","red","blue","green"),
	main="Netflix Opening & forecasted Prices' Log returns' Graph(based on Testing data)",
	xlab="Stock Trading Days in Testing data",
	ylab="Log returns of Stock Prices",
	ylim=c(1,10)
	)
legend(
	"topleft",
	legend=c("True Values","Forecast Values","Upper 95% CLs","Lower 95% CLs"),
	col=c("black","red","green","blue"),
	lty=1
	)
e_on=opening_price_netflix[(tl_netflix+1):n3]-fcast_on$Point.Forecast
MAD_on=mean(abs(e_on));MAD_on
MPE_on=mean((e_on/opening_price_netflix[(tl_netflix+1):n3])*100);MPE_on
MAPE_on=mean(abs(e_on/opening_price_netflix[(tl_netflix+1):n3])*100);MAPE_on
