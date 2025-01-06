
####################################
#
# Time Series Analysis Using ARIMA Model
# The ARIMA (AutoRegressive Integrated Moving Average) 
# to predict future values in a time series 
# based on past trends and pattern
#
####################################
 
# Step 1: Convert Data to a Time Series Object
# Convert the 'anxiety' data into a time series object
# Start in 2014 with 52 observations per year (weekly data)
ts1 <- ts(reshaped_data$anxiety, start = 2014, frequency = 52)

# Step 2: Split the Time Series into Training and Test Data
# Use the first 208 weeks (2014–2018) as training data
ts2 <- ts(ts1[c(1:208)], start = 2014, frequency = 52)
 
# Use the remaining weeks (2018 onwards) as holdout data for validation
hold <- ts(ts1[c(209:260)], start = 2018, frequency = 52)

# Step 3: Fit an ARIMA Model
# Automatically find the best ARIMA model based on the training data
fit <- auto.arima(ts2, stepwise = FALSE, approximation = FALSE)

# Step 4: Forecast Future Values
# Forecast for the next 3 years (52 weeks per year)
fcasts <- forecast(fit, h = 52 * 3, level = 95)

# Step 5: Plot the Forecast
# Plot the forecast with training data and holdout data
plot(fcasts, main = "ARIMA Forecast of Anxiety Levels", 
     ylim = c(0, 100))
lines(hold, col = 'red', type = "l")  # Add the holdout data in red

# Add vertical lines for spring (March–May) in each year
spring <- c(
  2014 + 9 / 52, 2014 + 21 / 52,  # Spring 2014
  2015 + 9 / 52, 2015 + 21 / 52,  # Spring 2015
  2016 + 9 / 52, 2016 + 21 / 52,  # Spring 2016
  2017 + 9 / 52, 2017 + 21 / 52,  # Spring 2017
  2018 + 9 / 52, 2018 + 21 / 52,  # Spring 2018
  2019 + 9 / 52, 2019 + 21 / 52,  # Spring 2019
  2020 + 9 / 52, 2020 + 21 / 52   # Spring 2020
)
abline(v = spring, lty = 2, lwd = 1)  # Dashed vertical lines for spring

# Step 6: Evaluate Forecast Accuracy
# Compare the forecasted values with the actual holdout data
accuracy(fcasts, x = hold)

# Step 7: Check Residuals
# Calculate residuals (differences between predictions and actual data)
res <- residuals(fit)

# Perform the Ljung-Box test to check if residuals are random (white noise)
Box.test(res, type = "Lj")