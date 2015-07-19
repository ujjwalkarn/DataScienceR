#http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings
kingstimeseries <- ts(kings)
kingstimeseries

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries

plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)

plot(kings)
plot(births)
plot(souvenir)

logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

#DECOMPOSING THE TIME SERIES
kingstimeseriescomponents <- decompose(kingstimeseries)
#cannot be decomposed using classical decomposition of stl method, since there is no seasonal component
#use simple moving average to get the trend component
library(forecast)
kingstimeseriescomponents <- ma(kingstimeseries,3)
plot.ts(kingstimeseries)
plot.ts(kingstimeseriescomponents)

birthstimeseriescomponents <- decompose(birthstimeseries)
plot.ts(birthstimeseriescomponents$seasonal)
plot.ts(birthstimeseriescomponents$trend)
plot.ts(birthstimeseriescomponents$random)
plot(birthstimeseriescomponents)

souvenirtimeseriescomponents <- decompose(souvenirtimeseries)
plot.ts(souvenirtimeseriescomponents$seasonal)
plot.ts(souvenirtimeseriescomponents$trend)
plot.ts(souvenirtimeseriescomponents$random)
plot(souvenirtimeseriescomponents)

logsouvenirtimeseriescomponents <- decompose(logsouvenirtimeseries)
plot.ts(logsouvenirtimeseriescomponents$seasonal)
plot.ts(logsouvenirtimeseriescomponents$trend)
plot.ts(logsouvenirtimeseriescomponents$random)
plot(logsouvenirtimeseriescomponents)

#Seasonally Adjusting
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

library(forecast)
?decompose
?stl
?stlf


#EXPONENTIAL SMOOTHING
#Simple Exponential Smoothing

##1.using ses() fro forecast
library(fpp)
data(oil)

oildata <- window(oil,start=1996,end=2007)
plot(oildata, ylab="Oil (millions of tonnes)",xlab="Year")

fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3)

plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)", xlab="Year", main="", fcol="white", type="o")

lines(fitted(fit1), col="blue", type="o")
lines(fit1$mean, col="blue", type="o")

fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)
lines(fitted(fit2), col="red", type="o")
lines(fit2$mean, col="red", type="o")
forecast(fit2,h=3)
fitted(fit2)

fitted(fit2)[length(fitted(fit2))]

0.2*oildata[length(oildata)]+0.8*fitted(fit1)[length(fitted(fit1))]      #=fit1$mean=484.8025
0.6*oildata[length(oildata)]+0.4*fitted(fit2)[length(fitted(fit2))]      #=fit2$mean=501.8375

?ses

##2.using HoltWinters() from stats package
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts

plot(rainseriesforecasts)
rainseriesforecasts$SSE

HoltWinters(rainseries) #error

rainseriesforecasts2 <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)
plot(rainseriesforecasts2)
rainseriesforecasts2$SSE

library("forecast")
rainseriesforecasts3 <- forecast.HoltWinters(rainseriesforecasts, h=8)
plot.forecast(rainseriesforecasts3)

acf(rainseriesforecasts3$residuals, lag.max=20)

#dickey-fuller test
library(tseries)
adf.test(rainseriesforecasts3$residuals,alternative="stationary")

kpss.test(rainseriesforecasts3$residuals)

plot.ts(rainseriesforecasts3$residuals)

?HoltWinters






