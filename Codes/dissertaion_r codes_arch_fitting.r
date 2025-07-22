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
library(rugarch)
library(FinTS)
library(e1071)
library(zoo)

####ARCH Fitting

###Amazon
##analysis on closing prices

adf.test((closing_price_amazon[1:tl_amazon]))
adf.test(diff(closing_price_amazon[1:tl_amazon]))

plot((diff(closing_price_amazon[1:tl_amazon])),type="l",main="Plot of Stationary time series",xlab="Stock Trading Days",ylab="Returns")
acf(diff(closing_price_amazon[1:tl_amazon]))
pacf((closing_price_amazon[1:tl_amazon]))
mean_eq_ca=arma((closing_price_amazon[1:tl_amazon]),order=c(1,2));mean_eq_ca
resi_mean_eq_ca=na.omit(residuals(mean_eq_ca))
n_ca=length(resi_mean_eq_ca)
resi_1st_ca=resi_mean_eq_ca[1:(n_ca-1)]
resi_2nd_ca=resi_mean_eq_ca[2:n_ca]
lm((resi_2nd_ca^2)~(resi_1st_ca^2))
summary(lm((resi_2nd_ca^2)~(resi_1st_ca^2)))
et_ca=residuals(lm((resi_2nd_ca^2)~(resi_1st_ca^2)))
w_bar_ca=mean(resi_mean_eq_ca^2);w_bar_ca
SSR0_ca=sum((resi_mean_eq_ca^2 - w_bar_ca)^2);SSR0_ca
SSR1_ca=sum(et_ca^2);SSR1_ca
F_obs_ca=(((SSR0_ca - SSR1_ca)/1)/(SSR1_ca/(tl_amazon-(2*1)-1)));F_obs_ca
qchisq(0.95,1)
ArchTest(diff(closing_price_amazon[1:tl_amazon]),lag=1)  ##by ArchTest function
garch(diff(closing_price_amazon[1:tl_amazon]),c(0,1),grad="numerical",trace=F)

ca_arch=ugarchspec(variance.model=list(garchOrder=c(1,0)),mean.model=list(armaOrder=c(1,2)))
ca_arch_fit=ugarchfit(ca_arch,data=diff(closing_price_amazon[1:tl_amazon]))
ca_arch_fit
plot(ca_arch_fit,xlim=c(1:tl_amazon))

ca_arch_f=(ugarchforecast(ca_arch_fit,n.ahead=tsl_amazon))
ca_arch_f
plot(sigma(ca_arch_f),main="Volality Series over Time points",xlab="Stock Trading Days",ylab="Volatility Values")

#analysis on opening prices

adf.test((opening_price_amazon[1:tl_amazon]))
adf.test(diff(opening_price_amazon[1:tl_amazon]))

plot((diff(opening_price_amazon[1:tl_amazon])),type="l",main="Plot of Stationary time series",xlab="Stock Trading Days",ylab="Returns")
acf(diff(opening_price_amazon[1:tl_amazon]))
pacf((opening_price_amazon[1:tl_amazon]))
mean_eq_oa=arma((opening_price_amazon[1:tl_amazon]),order=c(1,0));mean_eq_oa
resi_mean_eq_oa=na.omit(residuals(mean_eq_oa))
n_oa=length(resi_mean_eq_oa)
resi_1st_oa=resi_mean_eq_oa[1:(n_oa-1)]
resi_2nd_oa=resi_mean_eq_oa[2:n_oa]
lm((resi_2nd_oa^2)~(resi_1st_oa^2))
summary(lm((resi_2nd_oa^2)~(resi_1st_oa^2)))
et_oa=residuals(lm((resi_2nd_oa^2)~(resi_1st_oa^2)))
w_bar_oa=mean(resi_mean_eq_oa^2);w_bar_oa
SSR0_oa=sum((resi_mean_eq_oa^2 - w_bar_oa)^2);SSR0_oa
SSR1_oa=sum(et_oa^2);SSR1_oa
F_obs_oa=(((SSR0_oa - SSR1_oa)/1)/(SSR1_oa/(tl_amazon-(2*1)-1)));F_obs_oa
qchisq(0.95,1)
ArchTest(diff(opening_price_amazon[1:tl_amazon]),lag=1)
garch(diff(opening_price_amazon[1:tl_amazon]),c(0,1),grad="numerical",trace=F)

oa_arch=ugarchspec(variance.model=list(garchOrder=c(1,0)),mean.model=list(armaOrder=c(1,0)))
oa_arch_fit=ugarchfit(oa_arch,data=diff(opening_price_amazon[1:tl_amazon]))
oa_arch_fit
plot(oa_arch_fit)

oa_arch_f=(ugarchforecast(oa_arch_fit,n.ahead=tsl_amazon))
oa_arch_f
plot(sigma(oa_arch_f),main="Volality Series over Time points",xlab="Stock Trading Days",ylab="Volatility Values")



##Microsoft
#analysis on closing prices

adf.test((closing_price_microsoft[1:tl_microsoft]))
adf.test(diff(closing_price_microsoft[1:tl_microsoft]))

plot((diff(closing_price_microsoft[1:tl_microsoft])),type="l",main="Plot of Stationary time series",xlab="Stock Trading Days",ylab="Returns")
acf(diff(closing_price_microsoft[1:tl_microsoft]))
pacf((closing_price_microsoft[1:tl_microsoft]))
mean_eq_cm=arma((closing_price_microsoft[1:tl_microsoft]),order=c(1,0));mean_eq_cm
resi_mean_eq_cm=na.omit(residuals(mean_eq_cm))
n_cm=length(resi_mean_eq_cm)
resi_1st_cm=resi_mean_eq_cm[1:(n_cm-1)]
resi_2nd_cm=resi_mean_eq_cm[2:n_cm]
lm((resi_2nd_cm^2)~(resi_1st_cm^2))
summary(lm((resi_2nd_cm^2)~(resi_1st_cm^2)))
et_cm=residuals(lm((resi_2nd_cm^2)~(resi_1st_cm^2)))
w_bar_cm=mean(resi_mean_eq_cm^2);w_bar_cm
SSR0_cm=sum((resi_mean_eq_cm^2 - w_bar_cm)^2);SSR0_cm
SSR1_cm=sum(et_cm^2);SSR1_cm
F_obs_cm=(((SSR0_cm - SSR1_cm)/1)/(SSR1_cm/(tl_microsoft-(2*1)-1)));F_obs_cm
qchisq(0.95,1)



#analysis on opening prices

adf.test((opening_price_microsoft[1:tl_microsoft]))
adf.test(diff(opening_price_microsoft[1:tl_microsoft]))

plot((diff(opening_price_microsoft[1:tl_microsoft])),type="l",main="Plot of Stationary time series",xlab="Stock Trading Days",ylab="Returns")
acf(diff(opening_price_microsoft[1:tl_microsoft]))
pacf((opening_price_microsoft[1:tl_microsoft]))
mean_eq_om=arma((opening_price_microsoft[1:tl_microsoft]),order=c(1,0));mean_eq_om
resi_mean_eq_om=na.omit(residuals(mean_eq_om))
n_om=length(resi_mean_eq_om)
resi_1st_om=resi_mean_eq_om[1:(n_om-1)]
resi_2nd_om=resi_mean_eq_om[2:n_om]
lm((resi_2nd_om^2)~(resi_1st_om^2))
summary(lm((resi_2nd_om^2)~(resi_1st_om^2)))
et_om=residuals(lm((resi_2nd_om^2)~(resi_1st_om^2)))
w_bar_om=mean(resi_mean_eq_om^2);w_bar_om
SSR0_om=sum((resi_mean_eq_om^2 - w_bar_om)^2);SSR0_om
SSR1_om=sum(et_om^2);SSR1_om
F_obs_om=(((SSR0_om - SSR1_om)/1)/(SSR1_om/(tl_microsoft-(2*1)-1)));F_obs_om
qchisq(0.95,1)
ArchTest(diff(opening_price_microsoft[1:tl_microsoft]),lag=1)
garch(diff(opening_price_microsoft[1:tl_microsoft]),c(0,1),grad="numerical",trace=F)

om_arch=ugarchspec(variance.model=list(garchOrder=c(1,0)),mean.model=list(armaOrder=c(1,0)))
om_arch_fit=ugarchfit(om_arch,data=diff(opening_price_microsoft[1:tl_microsoft]))
om_arch_fit
plot(om_arch_fit)

om_arch_f=(ugarchforecast(om_arch_fit,n.ahead=tsl_microsoft))
om_arch_f
plot(sigma(om_arch_f),main="Volality Series over Time points",xlab="Stock Trading Days",ylab="Volatility Values")



##Netflix
#analysis on closing prices

adf.test((closing_price_netflix[1:tl_netflix]))
adf.test(diff(closing_price_netflix[1:tl_netflix]))

plot((diff(closing_price_netflix[1:tl_netflix])),type="l",main="Plot of Stationary time series",xlab="Stock Trading Days",ylab="Returns")
acf(diff(closing_price_netflix[1:tl_netflix]))
pacf((closing_price_netflix[1:tl_netflix]))
mean_eq_cn=arma((closing_price_netflix[1:tl_netflix]),order=c(1,0));mean_eq_cn
resi_mean_eq_cn=na.omit(residuals(mean_eq_cn))
n_cn=length(resi_mean_eq_cn)
resi_1st_cn=resi_mean_eq_cn[1:(n_cn-1)]
resi_2nd_cn=resi_mean_eq_cn[2:n_cn]
lm((resi_2nd_cn^2)~(resi_1st_cn^2))
summary(lm((resi_2nd_cn^2)~(resi_1st_cn^2)))
et_cn=residuals(lm((resi_2nd_cn^2)~(resi_1st_cn^2)))
w_bar_cn=mean(resi_mean_eq_cn^2);w_bar_cn
SSR0_cn=sum((resi_mean_eq_cn^2 - w_bar_cn)^2);SSR0_cn
SSR1_cn=sum(et_cn^2);SSR1_cn
F_obs_cn=(((SSR0_cn - SSR1_cn)/1)/(SSR1_cn/(tl_netflix-(2*1)-1)));F_obs_cn
qchisq(0.95,1)


#analysis on opening prices

adf.test((opening_price_netflix[1:tl_netflix]))
adf.test(diff(opening_price_netflix[1:tl_netflix]))

plot((diff(opening_price_netflix[1:tl_netflix])),type="l",main="Plot of Stationary time series",xlab="Stock Trading Days",ylab="Returns")
acf(diff(opening_price_netflix[1:tl_netflix]))
pacf((opening_price_netflix[1:tl_netflix]))
mean_eq_on=arma((opening_price_netflix[1:tl_netflix]),order=c(1,0));mean_eq_on
resi_mean_eq_on=na.omit(residuals(mean_eq_on))
n_on=length(resi_mean_eq_on)
resi_1st_on=resi_mean_eq_on[1:(n_on-1)]
resi_2nd_on=resi_mean_eq_on[2:n_on]
lm((resi_2nd_on^2)~(resi_1st_on^2))
summary(lm((resi_2nd_on^2)~(resi_1st_on^2)))
et_on=residuals(lm((resi_2nd_on^2)~(resi_1st_on^2)))
w_bar_on=mean(resi_mean_eq_on^2);w_bar_on
SSR0_on=sum((resi_mean_eq_on^2 - w_bar_on)^2);SSR0_on
SSR1_on=sum(et_on^2);SSR1_on
F_obs_on=(((SSR0_on - SSR1_on)/1)/(SSR1_on/(tl_netflix-(2*1)-1)));F_obs_on
qchisq(0.95,1)
