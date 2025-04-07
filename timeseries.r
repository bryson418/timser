library(stats)
library(lubridate)
library(forecast)
library(tseries)
#reads in stock data of multiple indexes over many decades
stocks <- read.csv("C:\\Users\\bryso\\Downloads\\archive\\indexProcessed.csv")
#reads in data of the volume of the word "debt" as searched in google
debtvol <- read.csv("C:\\Users\\bryso\\Downloads\\multiTimeline (1).csv")

stockts <- ts(stocks)
datets <- stocks[1:1000, 2]
datets <- as.Date(datets)
timhsi <- ts(stocks[1:1000, 3])

# plotting the original time series
plot(datets, timhsi, type = "l")
#original acf does not appear stationary, not resembling white noise

plot(acf(timhsi))
#differenced time series acf values are not significant and resemble white noise
plot(acf(diff(timhsi)))

#average method for forecasting
plot(meanf(timhsi, 10), main= "Graph with mean forecasting")
#naive method for forecasting
plot(naive(timhsi, 10))
#drift method for forecasting
plot(rwf(timhsi, 10, drift=TRUE))
#linear regression 
fit.hsi <- tslm(timhsi ~ trend)
forecasted <- forecast(fit.hsi, h=10)
plot(forecasted, main = "Graph using linear regression")
#exponential smoothing with additive error. This is esentially a naive forecast, but performs slightly better
fit <- ets(timhsi)
forecast <- forecast(fit, h=10)
plot(forecast)
#this model leaves some correlation between residuals and residuals have a slight skew left

#augmented dickey-fuller test to determine stationarity
#p-value is > alpha of 0.05; the data is stationary
adft <- adf.test(timhsi, alternative = "stationary")
#the differenced data is now stationary
adft <- adf.test(diff(timhsi), alternative = "stationary")
#After differencing once the data is now stationary

#Forecasting data using auto.arima as well as the model with the lowest AIC
size <- 50
futures <- stockts[1001:(1000+size), 3]


fit <- auto.arima(timhsi)
forecasted <- forecast(fit, h = size)
plot(forecasted)

fit <- arima(diff(timhsi), order =  c(0,0,0))
plot(forecast(fit, h=10))
(accuracy(fit))
AIC(fit)

fit2 <- arima(diff(timhsi), order=  c(2,1,2))
forecasted <- forecast(fit2, h = size)
AIC(fit2)

#Un differencing the data to check accuracy
undiff_forecast <- cumsum(c(timhsi[1000], forecasted$mean))

print(accuracy(undiff_forecast, futures))
#We can predict the next 50 values with 88% accuracy using arima(2,1,2)


# most accurate
fit <- arima(diff(timhsi), order =  c(2,1,2))
plot(forecast(fit, h=10))
print(accuracy(fit))
print(AIC(fit))
plot(acf(residuals(fit)))
#acf of residuals resemble white noise for the most part

#Using ARIMAX to forecast values. The exogenous variable comes from google trends
#reformatting data into Date
library(zoo)

colnames(debtvol)[colnames(debtvol) == "X2004.01"] <- "Date"
colnames(debtvol)[colnames(debtvol) == "X62"] <- "Volume"

debtvol$Date <- paste(debtvol$Date,  "01", sep = "-")

debtvol$Date <- as.Date(debtvol$Date)

#filling gaps
daily_dates <- seq(min(debtvol$Date), max(debtvol$Date), by = "day")

# Step 2: Convert the monthly data to a zoo object with Date as the index
debtvol_zoo <- zoo(debtvol$Volume, order.by = debtvol$Date)

# Step 3: Interpolate the data to the daily sequence of dates
daily_data <- na.approx(debtvol_zoo, xout = daily_dates)

# Step 4: Convert the interpolated data back to a data frame
daily_data_df <- data.frame(Date = index(daily_data), Volume = coredata(daily_data))

aligned_data <- merge(stocks[1:8493,], daily_data_df, by =  "Date", all = FALSE)

#looping through all data to find where each stock index is mentioned
for(i in 2:length(stocks$Index)){ 
    if(stocks$Index[i] != stocks$Index[i-1]) {
        print(i)  
    }
}

futures <- aligned_data$CloseUSD[1001:1050]
close <- aligned_data$CloseUSD[1:1000]
vol <- aligned_data$Volume.y[1:1000]

#plotting the volume of searches of the word "debt" to check for stationarity or seasonality
plot(vol)
plot(acf(diff(diff(vol))))
print("ADF Vol")
adft <- adf.test(diff(vol), alternative = "stationary")
print(adft)
#after analyzing the adft test and acf charts, it seemed that the data needed differencing given the extremely high autocorrelation between values
#forecasts values from 2004 with an accuracy of 96%
fit <- auto.arima(close)
forecasted <-forecast(fit, h=50)
plot(forecasted)
print(accuracy(forecasted$mean, futures))

#future values of exogenous variable
fitvol <- auto.arima(vol)
forecasted <- forecast(fitvol, h=50)
fithsi <- auto.arima(close, xreg = vol)
forecasthsi <- forecast(fithsi, xreg= forecasted$mean)
future_series <- c(close, forecasthsi$mean)
plot(forecasthsi)
#forecasts values from 2004 with an accuracy of 95.6%
print(accuracy(forecasthsi$mean, futures))
print(summary(fithsi))
plot(acf(residuals(fithsi)))
#acf of residuals of fithsi resemble white noise with almost all values being insignificant

#checking the AIC of an an arima model other than (2,1,2) with errors
fit <- arima(close, xreg = vol, order =  c(3,2,2))
print(AIC(fit))
fit <- arima(close, xreg = vol, order =  c(2,2,2))
print(AIC(fit))




#the accuracy of the method using the exogenous variable from google trends of volume "debt" being lower than forecasting only using the closing prices
#may be less accurate because many of the searches of the word "debt" were coming from countries other than China; the HSI index tracks stocks in China meaning
#that this variable may have little efffect on stock price. the inaccuracy may also be do to the lack of volume of the word "debt" being searched. Some days "debt"
#was not even searched, and this inconsistensies leaves little room for interpretation.









