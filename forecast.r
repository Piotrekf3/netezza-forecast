library(timetk)
library(tidyverse)
library(forecast)
library(zoo)

dataPath = "C:\\Users\\Piotrek\\Documents\\netezza-forecast\\data\\"
data<-read.table(file=paste(dataPath, "aggregatedData.csv", sep="") ,sep=";",row.names=1,header=TRUE)
data.ts<- as.ts(data)
plot(data.ts)

fit <-auto.arima(data.ts[,2])
fcast <- forecast(fit, h = 3000)
plot(fcast)