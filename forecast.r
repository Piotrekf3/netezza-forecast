library(timetk)
library(tidyverse)
library(forecast)
library(zoo)

dataPath = "C:\\Users\\Piotrek\\Documents\\netezza-forecast\\data\\"
data<-read.table(file=paste(dataPath, "aggregatedData2.csv", sep="") ,sep=";",row.names=1,header=TRUE)
data.ts<- ts(data, frequency=365)

fit <-auto.arima(data.ts, D=1)
fcast <- forecast(fit, h = 100)
plot(fcast)