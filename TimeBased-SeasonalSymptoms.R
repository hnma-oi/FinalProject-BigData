
## Load all of the necessary libraries for the script
library(gtrendsR)
library(reshape2)
library(ggplot2)
library(dplyr)
library(lubridate)


## Disease 1, Influenza ("flu", "influenza") October to March
## Disease 2, Pollen allergies ("pollen", "allergies") March to June
## Disease 3, Cold weather symptoms ("dry skin", "hypothermia", "frostbite") December to February
## Disease 4, Seasonal Affective Dissorder ("depression", "sleep disorder") November to February
## Disease 5, Heatwave symptoms ("heatstroke", "dehydration", "sunburn") June to August

# Get Google Trends data for the selected keywords for all of the diseases in the United States from 2022 to 2024
# Since we have multiple keywords, we need to split them into batches of 5 or fewer to get around the Google Trends API limit of 5 keywords per query

Sys.setenv(TZ = "America/New_York")

# # Lists of keywords
# keywords.All <- c("flu", "influenza", "pollen", "allergies", "dry skin", "hypothermia", "frostbite", "depression", "sleep disorder", "heatstroke", "dehydration", "sunburn")
# 
# # Split into batches of 5 or fewer
# keyword_batches <- split(keywords.All, ceiling(seq_along(keywords.All) / 2))
# 
# for (batch in keyword_batches) {
# 
#   result <- gtrends(keyword = batch, 
#                     geo = "US",
#                     time = "2023-01-01 2024-12-31",
#                     onlyInterest = TRUE)
#   Sys.sleep(60) # Wait 60 seconds between requests
#   
#   print(result)
# }
# # Loop through each batch and fetch data
# results <- lapply(keyword_batches, function(batch) {
#   gtrends(keyword = batch, geo = "US", time = "202-01-01 2024-12-31")
# })


# Get Google Trends data for the selected keywords from each of the disease for all of the diseases in the United States from 2022 to 2024
influenza <- gtrends(keyword = c("flu", "influenza"), geo = "US", time = "2022-01-01 2024-12-31")
pollen <- gtrends(keyword = c("pollen", "allergies"), geo = "US", time = "2022-01-01 2024-12-31")
cold_weather <- gtrends(keyword = c("dry skin", "hypothermia"), geo = "US", time = "2022-01-01 2024-12-31")
SAD <- gtrends(keyword = c("depression", "sleep disorder"), geo = "US", time = "2022-01-01 2024-12-31")
heatwave <- gtrends(keyword = c("dehydration", "sunburn"), geo = "US", time = "2022-01-01 2024-12-31")

#convert the data to a data frame and only get the interest over time
df_influenza <- data.frame(influenza$interest_over_time)
df_pollen <- data.frame(pollen$interest_over_time)
df_cold_weather <- data.frame(cold_weather$interest_over_time)
df_SAD <- data.frame(SAD$interest_over_time)
df_heatwave <- data.frame(heatwave$interest_over_time)

# Combine results
result <- list(influenza = df_influenza, pollen = df_pollen, cold_weather = df_cold_weather, SAD = df_SAD, heatwave = df_heatwave)

# Copy the selected data from the Google Trends query and back it up to a csv file
filename <- paste("TrendData/",  "Influenza",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(df_influenza, filename, fileEncoding = "UTF-8", row.names = FALSE)

filename <- paste("TrendData/",  "Pollen",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(df_pollen, filename, fileEncoding = "UTF-8", row.names = FALSE)

filename <- paste("TrendData/",  "cold_weather",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(df_cold_weather, filename, fileEncoding = "UTF-8", row.names = FALSE)

filename <- paste("TrendData/",  "SAD",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(df_SAD, filename, fileEncoding = "UTF-8", row.names = FALSE)

filename <- paste("TrendData/",  "Heatwave",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(df_heatwave, filename, fileEncoding = "UTF-8", row.names = FALSE)

# Manipulate the data from result to create a time series plot
consolidateds <- rbind(df_influenza, df_pollen, df_SAD, df_cold_weather, df_heatwave)

filename <- paste("TrendData/",  "Consolidate",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(consolidateds, filename, fileEncoding = "UTF-8", row.names = FALSE)

# Check the structure of your data
str(consolidateds)

# Convert `time` to Date format if necessary
consolidateds$date <- as.Date(consolidateds$date)

# Assuming `consolidateds` has a `date` column in Date format and `hits` column
consolidateds <- consolidateds %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%  # Create a month column
  group_by(month)  # Group by the month column

# Add a column with full month names
consolidateds <- consolidateds %>%
  mutate(month_name = format(as.Date(month), "%B"))  # "%B" gives the full month name

consolidateds$geo <- NULL
consolidateds$category <- NULL
consolidateds$gprop <- NULL
consolidateds$month <- NULL


consolidateds <- consolidateds %>%
  mutate(Influenza = ifelse(keyword %in% c("influenza", "flu"), hits, NA)) %>%
  group_by(month_name) %>%  # Group by month_name
  mutate(Influenza = mean(Influenza, na.rm = TRUE)) %>%
  ungroup()

consolidateds <- consolidateds %>%
  mutate(Pollen_allergies = ifelse(keyword %in% c("pollen", "allergies"), hits, NA)) %>%
  group_by(month_name) %>%  # Group by month_name
  mutate(Pollen_allergies = mean(Pollen_allergies, na.rm = TRUE)) %>%
  ungroup()

consolidateds <- consolidateds %>%
  mutate(SAD = ifelse(keyword %in% c("depression", "sleep disorder"), hits, NA)) %>%
  group_by(month_name) %>%  # Group by month_name
  mutate(SAD = mean(SAD, na.rm = TRUE)) %>%
  ungroup()

consolidateds <- consolidateds %>%
  mutate(cold_weather = ifelse(keyword %in% c("dry skin", "hypothermia"), hits, NA)) %>%
  group_by(month_name) %>%  # Group by month_name
  mutate(cold_weather = mean(cold_weather, na.rm = TRUE)) %>%
  ungroup()

consolidateds <- consolidateds %>%
  mutate(heatwave = ifelse(keyword %in% c("dehydration", "sun burn"), hits, NA)) %>%
  group_by(month_name) %>%  # Group by month_name
  mutate(heatwave = mean(heatwave, na.rm = TRUE)) %>%
  ungroup()

# Prepare data by summarizing as needed
summary_datas <- consolidateds %>%
  group_by(month_name) %>%
  summarize(
    Influenza = unique(Influenza),          # Ensure consistency
    Pollen_allergies = unique(Pollen_allergies),
    SAD = unique(SAD),
    cold_weather = unique(cold_weather),
    heatwave = unique(heatwave),
    .groups = "drop"
  )

# Convert month_name to an ordered factor to ensure proper time-series ordering
summary_datas <- summary_datas %>%
  mutate(month_name = factor(month_name, levels = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月")))

# Create the time series plot
ggplot(summary_datas, aes(x = month_name)) +
  # Line and point for Influenza
  geom_line(aes(y = Influenza, color = "Influenza"), linewidth = 1) +  
  geom_point(aes(y = Influenza, color = "Influenza"), size = 3) +
  
  # Line and point for Pollen_allergies
  geom_line(aes(y = Pollen_allergies, color = "Pollen_allergies"), linewidth = 1) +
  geom_point(aes(y = Pollen_allergies, color = "Pollen_allergies"), size = 3) +
  
  # Line and point for SAD
  geom_line(aes(y = SAD, color = "SAD"), linewidth = 1) +
  geom_point(aes(y = SAD, color = "SAD"), size = 3) +
  
  # Line and point for cold_weather
  geom_line(aes(y = cold_weather, color = "cold_weather"), linewidth = 1) +
  geom_point(aes(y = cold_weather, color = "cold_weather"), size = 3) +
  
  # Line and point for heatwave
  geom_line(aes(y = heatwave, color = "heatwave"), linewidth = 1) +
  geom_point(aes(y = heatwave, color = "heatwave"), size = 3) +
  
  # Labels and theme
  labs(
    title = "Time Series Plot of Influenza and Pollen Allergies",
    x = "Month",
    y = "Value",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )

filename <- paste("TrendData/",  "Summary Datas",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(summary_datas, filename, fileEncoding = "UTF-8", row.names = FALSE)


# Save the plot as a PNG file
ggsave("TimeSeriesPlot.png", width = 8, height = 6, dpi = 300)

# ARIMA Prediction

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