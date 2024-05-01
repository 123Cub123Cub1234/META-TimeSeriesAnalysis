getwd()
setwd("C:/Users/gaval/Documents")

data <- read.csv("meta_2014_2023.csv")
head(data)

library(ggplot2)

model_open <- lm(next_day_close ~ open, data = data)
model_high <- lm(next_day_close ~ high, data = data)
model_low <- lm(next_day_close ~ low, data = data)
model_close <- lm(next_day_close ~ close, data = data)

# Calculate residuals
res_open <- residuals(model_open)
res_high <- residuals(model_high)
res_low <- residuals(model_low)
res_close <- residuals(model_close)

# Create a combined dataframe for plotting
residual_data <- data.frame(
  Residuals = c(res_open, res_high, res_low, res_close),
  Type = factor(rep(c("Open", "High", "Low", "Close"), each = length(res_open)))
)

# Plotting residuals
ggplot(residual_data, aes(x = Type, y = Residuals, color = Type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Residual Analysis", x = "Price Type", y = "Residuals")

# Reshape the data for ggplot
library(reshape2)
melted_data <- melt(data, id.vars = "next_day_close", measure.vars = c("open", "high", "low", "close"))

# Create scatterplots
ggplot(melted_data, aes(x = value, y = next_day_close)) +
  geom_point(alpha = 0.5, color = "blue") + # blue points with some transparency to see density
  geom_smooth(method = "lm", se = FALSE, color = "red") + # linear regression line in red
  facet_wrap(~ variable, scales = "free") + # create separate plots for each type of price
  labs(title = "Scatterplots of Stock Prices vs. Next Day Close", x = "Price", y = "Next Day Close") +
  theme_minimal()




# Q-Q plot for Open vs Next Day Close
qqplot(data$next_day_close, data$open, main="Q-Q plot for Open vs Next Day Close", xlab="Next Day Close Quantiles", ylab="Open Quantiles", col="blue")
abline(0, 1, col = "red") # Add a 45-degree line


# Q-Q plot for High vs Next Day Close
qqplot(data$next_day_close, data$high, main="Q-Q plot for High vs Next Day Close", xlab="Next Day Close Quantiles", ylab="High Quantiles", col="green")
abline(0, 1, col = "black")

# Q-Q plot for Low vs Next Day Close
qqplot(data$next_day_close, data$low, main="Q-Q plot for Low vs Next Day Close", xlab="Next Day Close Quantiles", ylab="Low Quantiles", col="orange")
abline(0, 1, col = "green")

# Q-Q plot for Close vs Next Day Close
qqplot(data$next_day_close, data$close, main="Q-Q plot for Close vs Next Day Close", xlab="Next Day Close Quantiles", ylab="Close Quantiles", col="purple")
abline(0, 1, col = "yellow")


ggplot(data, aes(x = rsi_7, y = next_day_close)) +
  geom_point(aes(color = rsi_7), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "RSI 7 vs Next Day Close")

ggplot(data, aes(x = rsi_14, y = next_day_close)) +
  geom_point(aes(color = rsi_14), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "RSI 14 vs Next Day Close")

ggplot(data, aes(x = cci_7, y = next_day_close)) +
  geom_point(aes(color = cci_7), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "CCI 7 vs Next Day Close")

ggplot(data, aes(x = cci_14, y = next_day_close)) +
  geom_point(aes(color = cci_14), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "CCI 14 vs Next Day Close")

# Time series plot for RSI 14
ggplot(data, aes(x = date, y = rsi_14)) +
  geom_line() +
  labs(title = "RSI 14 Over Time", x = "Date", y = "RSI 14") +
  theme_minimal()

ggplot(data, aes(x = date, y = rsi_7)) +
  geom_line() +
  labs(title = "RSI 7 Over Time", x = "Date", y = "RSI 7") +
  theme_minimal()

ggplot(data, aes(x = date, y = cci_7)) +
  geom_line() +
  labs(title = "CCI 7 Over Time", x = "Date", y = "CCI 7") +
  theme_minimal()

ggplot(data, aes(x = date, y = cci_14)) +
  geom_line() +
  labs(title = "CCI 14 Over Time", x = "Date", y = "CCI 14") +
  theme_minimal()

install.packages("tseries")


library(forecast)
library(tseries)

data$date <- as.Date(data$date)

#1 Time series analysis for open, close, low, and high

ts_open <- ts(data$open, frequency=365)  # Adjust the frequency based on the data's nature
ts_close <- ts(data$close, frequency=365)
ts_low <- ts(data$low, frequency=365)
ts_high <- ts(data$high, frequency=365)

par(mfrow=c(2,2))  # Arrange plots in a 2x2 grid
plot(ts_open, main="Open Prices", xlab="Time", ylab="Price")
plot(ts_close, main="Close Prices", xlab="Time", ylab="Price")
plot(ts_low, main="Low Prices", xlab="Time", ylab="Price")
plot(ts_high, main="High Prices", xlab="Time", ylab="Price")


model_open <- auto.arima(ts_open)
model_close <- auto.arima(ts_close)
model_low <- auto.arima(ts_low)
model_high <- auto.arima(ts_high)

# Summarize the models
summary(model_open)
summary(model_close)
summary(model_low)
summary(model_high)


tsdiag(model_open, main="Diagnostics for Open Prices")

tsdiag(model_close, main="Diagnostics for Close Prices")

tsdiag(model_low, main="Diagnostics for Low Prices")

tsdiag(model_high, main="Diagnostics for High Prices")

# Forecast future values
forecast_open <- forecast(model_open, h=30)  # Forecast next 30 periods
forecast_close <- forecast(model_close, h=30)
forecast_low <- forecast(model_low, h=30)
forecast_high <- forecast(model_high, h=30)

# Plot forecasts
par(mfrow=c(2,2))
plot(forecast_open, main="Forecast for Open Prices")
plot(forecast_close, main="Forecast for Close Prices")
plot(forecast_low, main="Forecast for Low Prices")
plot(forecast_high, main="Forecast for High Prices")

#2 - atr_7, atr_14, bolligner, and truerange

library(rugarch)

ts_bollinger <- ts(data$bollinger, frequency=252)
ts_TrueRange <- ts(data$TrueRange, frequency=252)
ts_atr_7 <- ts(data$atr_7, frequency=252)
ts_atr_14 <- ts(data$atr_14, frequency=252)

fit_garch <- function(ts_data) {
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
  fit <- ugarchfit(spec = spec, data = ts_data)
  return(fit)
}


# Fit GARCH models
model_bollinger <- fit_garch(ts_bollinger)
model_TrueRange <- fit_garch(ts_TrueRange)
model_atr_7 <- fit_garch(ts_atr_7)
model_atr_14 <- fit_garch(ts_atr_14)

# Summarize the models
summary(model_bollinger)
summary(model_TrueRange)
summary(model_atr_7)
summary(model_atr_14)

# Forecasting
forecast_bollinger <- ugarchforecast(model_bollinger, n.ahead = 10)
forecast_TrueRange <- ugarchforecast(model_TrueRange, n.ahead = 10)
forecast_atr_7 <- ugarchforecast(model_atr_7, n.ahead = 10)
forecast_atr_14 <- ugarchforecast(model_atr_14, n.ahead = 10)

# Plotting the forecasts)
plot(forecast_bollinger, main="Forecast for Bollinger Bands")
plot(forecast_TrueRange, main="Forecast for True Range")
plot(forecast_atr_7, main="Forecast for ATR 7")
plot(forecast_atr_14, main="Forecast for ATR 14")


#dis aint mine

tsData <- ts(data$next_day_close, frequency = 12)
decomposedRes <- decompose(tsData, type = "additive")
plot(decomposedRes)


# Create time series objects for regressor variables
regressors_ts <- ts(data[, c("open", "low", "close", "volume", "rsi_7", "rsi_14", "cci_7", "sma_50", "sma_100", "ema_50", "macd", "bollinger", "atr_7", "atr_14")]
                    ,frequency = 12)

# Explore the time series properties
# Decompose the time series to analyze its components
decomposed_ts <- decompose(tsData, type = "multiplicative")
plot(decomposed_ts)

# Check autocorrelation and partial autocorrelation
acf(tsData)
pacf(tsData)

# Fit a multilinear regression model
lm_model <- lm(tsData ~ ., data = as.data.frame(cbind(regressors_ts, tsData)))

# Summarize the model
summary(lm_model)

#new


# Plot the time series data
plot(tsData, main = "Time Series of Next Day Close Price", xlab = "Year", ylab = "Close Price")

# Add legend for regressor variables
legend("topright", legend = colnames(regressors_ts), col = 1:ncol(regressors_ts), lty = 1)

# Plot the fitted values from the regression model
lines(fitted(lm_model), col = "red", lwd = 2)

# Add legend for fitted values
legend("bottomright", legend = c("Fitted Values"), col = "red", lty = 1)

# Add grid lines
grid()


# Fit an ARIMA model. The auto.arima function will automatically select the best parameters.
arima_model <- auto.arima(tsData)

# Plot the original time series
plot(tsData, main="Time Series of Next Day Close Price", xlab="Time", ylab="Close Price")

# Add the ARIMA fitted values to the plot
lines(fitted(arima_model), col="red", lwd=2)

# Add a legend to the plot
legend("bottomright", legend=c("Actual", "ARIMA Fitted"), col=c("black", "red"), lty=1, cex=0.8)

# Add grid lines
grid()

#Regression Line
time_index <- c(1:length(tsData))

trend_model <- lm(tsData ~ time_index)

trend_line <- predict(trend_model, newdata = data.frame(time_index = time_index))

lines(time_index, trend_line, col="blue", lwd=2)

legend("bottomright", legend=c("Actual", "ARIMA Fitted", "Trend Line"), col=c("black", "red", "blue"), lty=1, cex=0.8)

grid()


# Convert 'next_day_close' to a time series object with a monthly frequency
tsData <- ts(data$next_day_close, frequency = 12)

# Fit an ARIMA model
arima_model <- auto.arima(tsData)

# Plot the original time series
plot(tsData, main="Time Series of Next Day Close Price", xlab="Months", ylab="Close Price", col="black", type="l")

# Overlay the ARIMA fitted values
lines(fitted(arima_model), col="red", lwd=1)

# Add a trend line using actual dates
# Assuming the 'date' column is in the correct date format


dates <- as.Date(data$date)
# Create a numerical index from the dates
dates_index <- as.numeric(dates - min(dates)) + 1

# Fit the linear trend model
trend_model <- lm(tsData ~ dates_index)

# Get the trend line predictions
trend_line <- predict(trend_model, newdata = data.frame(dates_index = dates_index))

# Overlay the trend line on the existing plot
lines(dates_index, trend_line, col="blue", lwd=2)

# Add the legend
legend("bottomright", inset=0.05, legend=c("Actual", "ARIMA Fitted", "Trend Line"), 
       col=c("black", "red", "blue"), lty=1, lwd=2)

# Add grid lines
grid()

# Assume data is loaded and 'data' DataFrame is available
library(forecast)

# Since we're working with time series, let's try an ARIMA model instead of linear regression
tsData <- ts(data$next_day_close, frequency=12)

# Fit an ARIMA model. The auto.arima function will automatically select the best parameters.
arima_model <- auto.arima(tsData)

# Plot the original time series
plot(tsData, main="Time Series of Next Day Close Price", xlab="Time", ylab="Close Price")

# Add the ARIMA fitted values to the plot
lines(fitted(arima_model), col="red", lwd=1)

# Add a legend to the plot
legend("bottomright", legend=c("Actual", "ARIMA Fitted"), col=c("black", "red"), lty=1, cex=0.8)

# Add grid lines
grid()


library(forecast)
library(rugarch)

# Assuming 'tsData' is already loaded as a ts object
tsData <- ts(data$next_day_close, frequency = 12)

# Fit the ARIMA model
arima_fit <- auto.arima(tsData)

# Fit a GARCH model to the residuals of the ARIMA model
spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                   distribution.model = "std")

# We use the residuals of the ARIMA model here
garch_fit <- ugarchfit(spec = spec, data = residuals(arima_fit))

# Plot the original time series
plot(tsData, main = "Time Series of Next Day Close Price", xlab = "Time", ylab = "Close Price")

# Overlay the ARIMA fitted values
lines(fitted(arima_fit), col = "red")

# Now, if you want to add the conditional standard deviations (sqrt of conditional variances) to the plot:
cond_sd <- sigma(garch_fit)
lines(seq_along(tsData), cond_sd, col = "blue")

# Add a legend to the plot
legend("bottomright", inset = 0.05, legend = c("Actual", "ARIMA Fitted", "GARCH Volatility"), 
       col = c("black", "red", "blue"), lty = 1, cex = 0.8)

# Add grid lines
grid()

rescaled_volatility <- volatility * 10  # Adjust the multiplier as necessary for visibility

# Plot the original time series
plot(tsData, main="Time Series of Next Day Close Price", xlab="Time", ylab="Close Price", col="black", type="l")

# Overlay the ARIMA model's fitted values
lines(fitted(arima_model), col="red", lwd=1)

# Overlay the rescaled GARCH volatility
lines(rescaled_volatility, col="blue", lwd=1)

# Add a legend to the plot
legend("bottomright", legend=c("Actual", "ARIMA Fitted", "GARCH Volatility"), col=c("black", "red", "blue"), lty=1, lwd=2)

# Add grid lines
grid()


