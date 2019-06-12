library(timetk)
library(tidyverse)
library(forecast)
library(zoo)

nz.fun <- function() {
  data <- list()
  while(getNext()) {
    input <- getInputColumn(0)
    data <- list(input)
  }
  data.ts<- ts(data, frequency=365)
  
  fit <- auto.arima(data.ts, D=1)
  fcast <- forecast(fit, h = 100)

}
