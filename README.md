In this project I used different time series forecasting models to forecast the stock price of the Hang Seng Stock Index (HSI). I tweaked a large data set that included stock prioces of many top stock
indexes over the past 20 years. To gage a baseline for the accuracy of basic time series models, I used simple exponential smoothing and naive forcasting to forecast values. I then found the most
accurate ARIMA model for this data set that is non-stationary and non-seasonal. Analyzed ACF and PACF plots and used augmented dickey fuller tests to check stationarity and seasonality. Then brought in
an exogenous variable to see its effect on the stock market price predictions. This variable was the number of times that the word "debt" was searched on google. An ARIMAX model was used to include this
exogenous variable in hopes of increasing the accuracy.
