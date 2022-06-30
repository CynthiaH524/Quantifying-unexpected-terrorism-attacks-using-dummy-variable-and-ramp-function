# ECON 599 Research Paper

# Libraries
library("writexl")
library(tidyverse)
library(forecast)
library(rvest)
library(tidyr)

# Set working directory
setwd("~/Desktop/")

####################################################################################

# Importing Data

####################################################################################
# Importing data for Maldives from 1999 to 2012
maldives= scan()
maldives
maldives = ts(maldives, start = c(1999,1), frequency = 12)
autoplot(maldives)+autolayer(BaliArrival)
BaliArrival = ts(bali, start = c(1999,1), frequency = 12)
autoplot(window(maldives, start = c(2002,1),end=c(2006,1)))

# Importing data for Bali from 1998 to 2012
bali = scan()
Bali = ts(bali, start = c(1998,1), frequency = 12)
autoplot(window(bali, end=c(2005,10)))
autoplot(bali)

# Importing global GDP
wGDP = scan()
wGDP = ts(wGDP, start=c(1998,1), frequency = 12)
autoplot(wGDP)

# Importing International Tourism Receipt
IntTourismR = scan()
IntTourismR = rep(IntTourismR, each = 12)
IntTourismR = ts(IntTourismR, start=c(1998,1), frequency = 12)
autoplot(IntTourismR)

# Importing Indonesia Exchange Rate
IndoER = scan()

# Importing Australian Exchange Rate
AusER = scan()

# Importing Japanese Exchange Rate
JanER = scan()

# Importing Indonesia Trade Balance
IndoTB = scan()

# setting up monthly dummy
month = c('Jan', 'Feb','Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' )
monthd = rep(month, 15)
Trend = 1:180

# dummy for affected period
dummyT = c(rep(0, length(window(Bali, end = c(2002, 9)))),
           rep(1, length(window(Bali, start = c(2002,10), end = c(2004, 1)))),
           rep(0, length(window(Bali, start = c(2004, 2), end = c(2005, 9)))),
           rep(1, length(window(Bali, start = c(2005, 10), end = c(2007, 6)))),
           rep(0, length(window(Bali, start = c(2007, 7)))))

# ramp function for affected period
rampT = c(rep(0, length(window(Bali, end = c(2002, 9)))),
           (1:length(window(Bali, start = c(2002,10), end = c(2004, 1)))),
           rep(0, length(window(Bali, start = c(2004, 2), end = c(2005, 9)))),
           (1:length(window(Bali, start = c(2005, 10), end = c(2007, 6)))),
           rep(0, length(window(Bali, start = c(2007, 7)))))

# Inversed ramp
rampTr = c(rep(0, length(window(Bali, end = c(2002, 9)))),
          (length(window(Bali, start = c(2002,10), end = c(2004, 1))):1),
          rep(0, length(window(Bali, start = c(2004, 2), end = c(2005, 9)))),
          (length(window(Bali, start = c(2005, 10), end = c(2007, 6))):1),
          rep(0, length(window(Bali, start = c(2007, 7)))))

# Constructing the dataset
dataset = data.frame(monthd, Trend, bali, wGDP, IntTourismR, IndoER, IndoTB, 
                     dummyT,rampT, rampTr, AusER, JanER)
dataset$monthd = factor(dataset$monthd)

dataset$logBali = log(dataset$bali)
dataset$lagBali = lag(dataset$bali,1)
dataset$laglogBali = lag(dataset$logBali,1)

ggplot(data = dataset, aes(y = dummyT, x = Trend)) + geom_point()+geom_line()

ggplot(data = dataset, aes(y = rampT, x = Trend)) + geom_point()+geom_line()

ggplot(data = dataset, aes(y = rampTr, x = Trend)) + geom_point()+geom_line()

####################################################################################

# Arima

####################################################################################
train1 = window(Bali, end = c(2002, 9))
length(train1)
test1 = window(Bali, start = c(2002, 10), end=c(2005, 12))
test2 = window(Bali, start = c(2002, 10), end=c(2012, 12))
test3 = window(Bali, start = c(2002, 10), end=c(2008, 1))
test4 = window(Bali, start = c(2004, 10), end=c(2010, 12))

M1 = auto.arima(train1, lambda = "auto") # ARIMA(0,0,0)(1,1,0)[12] with drift 
summary(M1)
M1F = forecast(M1, h=length(test1), level = FALSE)
M2F = forecast(M1, h=length(test2), level = FALSE)
M3F = forecast(M1, h=length(test3), level = FALSE)

#Visualization
autoplot(window(Bali, end=c(2005,12)))+autolayer(M1F$mean) +autolayer(M1F$fitted)+theme_bw()
accuracy(M1F, test1)

autoplot(Bali)+autolayer(M2F$mean) +autolayer(M2F$fitted)+theme_bw()

autoplot(window(Bali, end=c(2008,1)))+autolayer(M3F$mean) +autolayer(M2F$fitted)+theme_bw()


autoplot(Bali)+autolayer(M1F$mean) +autolayer(M1F$fitted)+theme_bw()

dif = window(Bali, start = c(2002, 10), end=c(2004, 1))-M3F$mean

diff = window(Bali, start = c(2005, 10), end=c(2007, 6))-M3F$mean

# calculating loss using ARIMA
sum(dif)+sum(diff)

autoplot(dif)
length(dif)

####################################################################################

# Hedonic Regression Models

####################################################################################
# 1st model - lm
M = lm(data = dataset, logBali~monthd+wGDP+IntTourismR+IndoER+AusER+JanER)
summary(M)
length(M$fitted.values)
ggplot(data = dataset, aes(y = M$fitted.values, x = Trend)) + geom_point()+
  geom_line() +geom_line(aes(y = dataset$logBali, x = dataset$Trend), color = 'red') + 
  geom_point(aes(y = dataset$logBali, x = dataset$Trend), color = 'red')+ theme_bw() 

ggtsdisplay(M$residuals)

# MAPE for 1st model
mean(abs((exp(dataset$logBali)-exp(M$fitted.values))/exp(dataset$logBali))) * 100

# 2nd model - lm with dummy
Mdu = lm(data = dataset, logBali~monthd+wGDP+IntTourismR+IndoER +dummyT+AusER+JanER)
summary(Mdu)
length(M$fitted.values)
ggplot(data = dataset, aes(y = Mdu$fitted.values, x = Trend)) + geom_point()+
  geom_line() +geom_line(aes(y = dataset$logBali, x = dataset$Trend), color = 'red') + 
  geom_line(aes(y = dataset$logBali, x = dataset$Trend), color = 'red') + 
  geom_point(aes(y = dataset$logBali, x = dataset$Trend), color = 'red')+ theme_bw() 


# 3rd model - lm with ramp
Mra = lm(data = dataset, logBali~monthd+wGDP+IntTourismR+IndoER + rampT+AusER+JanER)
summary(Mra)
ggplot(data = dataset, aes(y = Mra$fitted.values, x = Trend)) + geom_point()+
  geom_line() +geom_line(aes(y = dataset$logBali, x = dataset$Trend), color = 'red') + 
  geom_line(aes(y = dataset$logBali, x = dataset$Trend), color = 'red') + theme_bw()

# 4th model - lm with inverse ramp
Mrr = lm(data = dataset, logBali~monthd+wGDP+IntTourismR+IndoER + rampTr+AusER+JanER)
summary(Mrr)
ggplot(data = dataset, aes(y = Mrr$fitted.values, x = Trend)) + geom_point()+
  geom_line() +geom_line(aes(y = dataset$logBali, x = dataset$Trend), color = 'red') + 
  geom_line(aes(y = dataset$logBali, x = dataset$Trend), color = 'red') +
  geom_point(aes(y = dataset$logBali, x = dataset$Trend), color = 'red') + theme_bw()

ggtsdisplay(Mrr$residuals)

# Adding one lag log value in the 4th hedonic regression model

M5 = lm(data = dataset, logBali~monthd+wGDP+IntTourismR+IndoER + rampTr+AusER+JanER+ laglogBali)
summary(M5)
ggtsdisplay(M5$residuals)

# calculating loss using hedonic regression
mfitted = ts(M$fitted.values, start = c(1998,1), frequency = 12)
mfitted = exp(mfitted)
dif2002m = window(Bali, start = c(2002, 10), end=c(2003, 12))- mfitted 

dif2005m = window(Bali, start = c(2005, 10), end=c(2006, 10))-mfitted 

sum(dif2002m)+sum(dif2005m)

# Exporting dataset to excel file
maldives=data.frame(maldives)
write_xlsx(dataset,"~/Desktop/CynthiaHuangResearchDataBali.xlsx")
write_xlsx(maldives,"~/Desktop/CynthiaHuangResearchDataMaldives.xlsx")
