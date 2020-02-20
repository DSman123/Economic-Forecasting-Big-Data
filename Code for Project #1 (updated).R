# Joel Cabrera
# Economic Forecasting and Big Data (01:220:423)
# Professor Berisha
# September 24, 2019

#Loading data
data=read.csv(file.choose(), header=TRUE)
summary(data)

#libraries MUST BE RAN FIRST
library(TSA)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

#variables becoming time series
#1
LFPR=ts(data$LBSSA34, frequency=12, start = c(1976,1))
UR=ts(data$NJURN, frequency=12, start = c(1976,1))
autoplot(LFPR)+xlab("Year") + ylab("Labor Force Participation Rate in NJ From 1976-2019")
autoplot(UR)+xlab("Year") + ylab("Unemployment Rate in NJ From 1976-2019")
#Note: can also use plot(), has different style
acf(LFPR)
acf(UR)
#ggAcf(LFPR)
#ggAcf(UR)
#Note: can also use ggAcf(), has different style

#2: Forecast Methods - for each variable
#2a. Average
plot(meanf(LFPR, h=1)) #forecastss next period #Note: blue dot = mean/most likely will be; 80% CI is dark blue; 95% CI is lighter bar
meanf(LFPR, h=1) #h = 1 -> forecasts one period forward

plot(meanf(UR, h=1)) #forecastss next period
meanf(UR, h=1) #h = 1 -> forecasts one period forward

#2b. Naive
LFPR1=naive(LFPR, h=1) #LFPR1 = renaming for naive
LFPR1
autoplot(naive(LFPR, h=1)) #can do plot, too, to check

UR1=naive(UR, h=1) #UR1 = renaming for naive
UR1
autoplot(naive(UR, h=1)) #can do plot, too, to check

#2c. Drift
LFPR2=rwf(LFPR, h=1, drift = TRUE)
LFPR2 #different name and gives dif. forecast #
autoplot(LFPR2)

UR2=rwf(UR, h=1, drift = TRUE)
UR2 #different name and gives dif. forecast #
autoplot(UR2)

#2d. Seasonal Naive
LFPR3=snaive(LFPR, h=1)
LFPR3
autoplot(LFPR3)

UR3=snaive(UR, h=1)
UR3
autoplot(UR3)

#3.Which model is most reliable?
#Average
checkresiduals(meanf(LFPR))
checkresiduals(meanf(UR))
#Naive
checkresiduals(naive(LFPR))
checkresiduals(naive(UR))
#Drift
checkresiduals(rwf(LFPR, drift = TRUE))
checkresiduals(rwf(UR, drift = TRUE))
#Seasonal Naive
checkresiduals(snaive(LFPR))
checkresiduals(snaive(UR))