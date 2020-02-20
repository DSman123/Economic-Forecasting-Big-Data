# Joel Cabrera
# Economic Forecasting and Big Data (01:220:423)
# Professor Berisha
# October 21, 2019

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
#autoplot(LFPR)+xlab("Year") + ylab("Labor Force Participation Rate in NJ From 1976-2019")
#autoplot(UR)+xlab("Year") + ylab("Unemployment Rate in NJ From 1976-2019")

#Time plots with MAs
# LFPR
autoplot(LFPR, series="NJ Labor Force Participation Rate")+
  autolayer(ma(LFPR,order=4, centre=TRUE), series="4-MA")
# UR
autoplot(UR, series="NJ Unemployment Rate")+
  autolayer(ma(UR,order=4, centre=TRUE), series="4-MA")

#2.
#Classical additive decomposition for LFPR
deLFPR1 = decompose(LFPR, type="additive")
autoplot(deLFPR1) #gives 4 graphs
s1=deLFPR1$seasonal #seasonal comp
r1=deLFPR1$random #remainder comp
t1=deLFPR1$trend #trend comp
plot(s1)
plot(r1)
plot(t1)
s1
r1
t1
#Classical multiplicative decomposition for LFPR
deLFPR2 = decompose(LFPR, type="multiplicative")
autoplot(deLFPR2) #gives 4 graphs
s2=deLFPR2$seasonal #seasonal comp
r2=deLFPR2$random #remainder comp
t2=deLFPR2$trend #trend comp
plot(s2)
plot(r2)
plot(t2)
s2
r2
t2
#Classical additive decomposition for UR
deUR1 = decompose(UR, type="additive")
autoplot(deUR1) #gives 4 graphs
s3=deUR1$seasonal #seasonal comp
r3=deUR1$random #remainder comp
t3=deUR1$trend #trend comp
plot(s3)
plot(r3)
plot(t3)
s3
r3
t3
#Classical multiplicative decomposition for UR
deUR2 = decompose(UR, type="multiplicative")
autoplot(deUR2) #gives 4 graphs
s4=deUR2$seasonal #seasonal comp
r4=deUR2$random #remainder comp
t4=deUR2$trend #trend comp
plot(s4)
plot(r4)
plot(t4)
s4
r4
t4

#3. Determining additive or multiplicative decomposed series

# decomposed LFPR
# additive deLFPR1
ggAcf(r1)
Box.test(r1)
# multiplicative deLFPR2
ggAcf(r2)
Box.test(r2)

# decomposed UR
# additive deUR1
ggAcf(r3)
Box.test(r3)
# multiplicative deUR2
ggAcf(r4)
Box.test(r4)