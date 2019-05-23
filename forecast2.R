library(timetk)
library(tidyverse)
library(forecast)
library(zoo)

plot(AirPassengers)
plot(decompose(AirPassengers, "multiplicative")) #ploting components of time series (trend, seasonality, random)

###Forecast based on whole time series 1949-1960
fit <-auto.arima(AirPassengers)
fcast <- forecast(fit, h = 24)
plot(fcast)
# ### Forecast using train/test split for validation
# train_1949_1958 <- ts(AirPassengers[1:120], start = 1949, frequency = 12)
# test_1959_1960 <- ts(AirPassengers[121:144], start = 1959, frequency = 12)
# fit <- auto.arima(train_1949_1958)
# fcast <- forecast(fit, h = 24)
#
# ###change locale to help formatting dates
# Sys.setlocale("LC_TIME", "us") #Sys.getlocale("LC_TIME")
# results <- tk_tbl(fcast) # throwing results into a nicer timetk format for table
# results$Date <- as.Date(as.yearmon(results$index, "%b %Y"))
# results <- results %>% select(Date, 'Point Forecast') %>% rename(value = 'Point Forecast')
# AirPassengers_tk <- tk_tbl(AirPassengers) #convertion to timetk time series format
# AirPassengers_tk$Date <- AirPassengers_tk %>% tk_index(timetk_idx = TRUE)
# AirPassengers_tk$Date <- as.Date(as.yearmon(tk_index(AirPassengers_tk, timetk_idx = TRUE), "%b %Y"))
# AirPassengers_tk <- AirPassengers_tk %>%   select(Date, value)
# results <- bind_rows("actual" = AirPassengers_tk, "forecast" = results, .id="groups")
#
# ### Ploting actual sales with forecasted values on one plot
# ggplot(results, aes(x = Date, y = value)) +
#   geom_line(aes(color = groups), size = 1) +
#   scale_color_manual(values = c("#C0C0C0", "#0000FF")) +
#   theme_minimal()
#
# ### Accuracy measures
# accuracy(fcast, test_1959_1960)
# accuracy(fcast)