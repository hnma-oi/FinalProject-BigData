
## Load all of the necessary libraries for the script
library(gtrendsR)
library(reshape2)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)


## Disease 1, Influenza ("flu", "Influenza") October to March
## Disease 2, Pollen Allergies ("Pollen", "Allergies") March to June
## Disease 3, Cold weather symptoms ("dry skin", "hypothermia", "frostbite") December to February
## Disease 4, Seasonal Affective Dissorder ("depression", "sleep disorder") November to February
## Disease 5, Heatwave symptoms ("heatstroke", "dehydration", "sunburn") June to August

# Get Google Trends data for the selected keywords for all of the diseases in the United States from 2022 to 2024
# Since we have multiple keywords, we need to split them into batches of 5 or fewer to get around the Google Trends API limit of 5 keywords per query

Sys.setenv(TZ = "America/New_York")

# GTrends command to fetch all of the search volume data for the selected keywords in one go, but it may exceed the API limit and return an error 
# # Lists of keywords
# keywords.All <- c("flu", "Influenza", "Pollen", "Allergies", "dry skin", "hypothermia", "frostbite", "depression", "sleep disorder", "heatstroke", "dehydration", "sunburn")
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
Influenza <- gtrends(keyword = c("flu", "Influenza"), geo = "US", time = "2022-01-01 2024-12-31")
Pollen <- gtrends(keyword = c("Pollen", "Allergies"), geo = "US", time = "2022-01-01 2024-12-31")
Cold_weather <- gtrends(keyword = c("dry skin", "hypothermia"), geo = "US", time = "2022-01-01 2024-12-31")
SAD <- gtrends(keyword = c("depression", "sleep disorder"), geo = "US", time = "2022-01-01 2024-12-31")
Heatwave <- gtrends(keyword = c("dehydration", "sunburn"), geo = "US", time = "2022-01-01 2024-12-31")

#convert the data to a data frame and only get the interest over time for each of the seasonal disease
df_Influenza <- data.frame(Influenza$interest_over_time)
df_Pollen_Allergies <- data.frame(Pollen$interest_over_time)
df_Cold_Weather <- data.frame(Cold_Weather$interest_over_time)
df_SAD <- data.frame(SAD$interest_over_time)
df_Heatwave <- data.frame(Heatwave$interest_over_time)

# Combine all of the results into a list
result <- list(Influenza = df_Influenza, Pollen = df_Pollen, Cold_Weather = df_Cold_Weather, SAD = df_SAD, Heatwave = df_Heatwave)

# Copy the selected data from the Google Trends query and back it up to a csv file
symptoms <- list("Influenza" = df_Influenza, 
                 "Pollen" = df_Pollen, 
                 "Cold_Weather" = df_Cold_Weather, 
                 "Seasonal Affective Disorder" = df_SAD, 
                 "Heatwave" = df_Heatwave)

for (symptom in names(symptoms)) {
  filename <- paste("TrendData/", symptom, 
                    format(Sys.time(), "%b%d_%H%M_%Y"), '.csv', sep = "")
  write.csv(symptoms[[symptom]], filename, fileEncoding = "UTF-8", row.names = FALSE)
}

## In case of GTrends query not being possible due to Google API Limit, we can load the data from the CSV files
df_Influenza <- read.csv("TrendData/Influenza106_0914_2025.csv", header = TRUE)
df_Pollen_Allergies <- read.csv("TrendData/Pollen106_0914_2025.csv", header = TRUE)
df_SAD <- read.csv("TrendData/SAD106_0914_2025.csv", header = TRUE)
df_Cold_Weather <- read.csv("TrendData/cold_Weather106_0914_2025.csv", header = TRUE)
df_Heatwave <- read.csv("TrendData/Heatwave106_0914_2025.csv", header = TRUE)

# Manipulate the data from result to create a time series plot
consolidated <- rbind(df_Influenza, df_Pollen, df_SAD, df_Cold_Weather, df_Heatwave)

filename <- paste("TrendData/",  "Consolidate",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(consolidated, filename, fileEncoding = "UTF-8", row.names = FALSE)

# Check the structure of your data
str(consolidated)

# Convert `time` to Date format if necessary
consolidated$date <- as.Date(consolidated$date)

# Assuming `consolidated` has a `date` column in Date format and `hits` column
consolidated <- consolidated %>%
  mutate(month = floor_date(as.Date(date), "month"),  # Create a month column
         month_name = format(as.Date(month), "%B")) %>%  # Add a column with full month names
  group_by(month)  # Group by the month column

# Drop the unnecessary columns
consolidated <- consolidated %>%
  ungroup() %>%  # Ungroup the data
  select(-geo, -category, -gprop, -month)  # Remove the unnecessary columns

# Define the symptoms and their corresponding keywords
symptom_keywords <- list(
  Influenza = c("Influenza", "flu"),
  Pollen_Allergies = c("Pollen", "Allergies"),
  SAD = c("depression", "sleep disorder"),
  Cold_Weather = c("dry skin", "hypothermia"),
  Heatwave = c("dehydration", "sun burn")
)

# Loop through each symptom and calculate the mean hits
for (symptom in names(symptom_keywords)) {
  consolidated <- consolidated %>%
    mutate(!!symptom := ifelse(keyword %in% symptom_keywords[[symptom]], hits, NA)) %>%
    group_by(month_name) %>%
    mutate(!!symptom := mean(!!sym(symptom), na.rm = TRUE)) %>%
    ungroup()
}

# Prepare data by summarizing as needed
summary_data <- consolidated %>%
  group_by(month_name) %>%
  summarize(
    Influenza = unique(Influenza),          # Ensure consistency
    Pollen_Allergies = unique(Pollen_Allergies),
    SAD = unique(SAD),
    Cold_Weather = unique(Cold_Weather),
    Heatwave = unique(Heatwave),
    .groups = "drop"
  )

# Convert month_name to an ordered factor to ensure proper time-series ordering
summary_data <- summary_data %>%
  mutate(month_name = factor(month_name, levels = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月")))

# Create the time series plots for each symptoms using ggplot2
ggplot(summary_data, aes(x = month_name)) +
  # Line and point for Influenza
  geom_line(aes(y = Influenza, color = "Influenza"), linewidth = 1) +  
  geom_point(aes(y = Influenza, color = "Influenza"), size = 3) +
  
  # Line and point for Pollen_Allergies
  geom_line(aes(y = Pollen_Allergies, color = "Pollen_Allergies"), linewidth = 1) +
  geom_point(aes(y = Pollen_Allergies, color = "Pollen_Allergies"), size = 3) +
  
  # Line and point for SAD
  geom_line(aes(y = SAD, color = "SAD"), linewidth = 1) +
  geom_point(aes(y = SAD, color = "SAD"), size = 3) +
  
  # Line and point for Cold_Weather
  geom_line(aes(y = Cold_Weather, color = "Cold_Weather"), linewidth = 1) +
  geom_point(aes(y = Cold_Weather, color = "Cold_Weather"), size = 3) +
  
  # Line and point for Heatwave
  geom_line(aes(y = Heatwave, color = "Heatwave"), linewidth = 1) +
  geom_point(aes(y = Heatwave, color = "Heatwave"), size = 3) +
  
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

filename <- paste("TrendData/",  "Summary Data",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(summary_data, filename, fileEncoding = "UTF-8", row.names = FALSE)

# Save the plot as a PNG file
ggsave("TimeSeriesPlot.png", width = 8, height = 6, dpi = 300)


# ARIMA Prediction
####################################
#
# Time Series Analysis Using ARIMA Model
# The ARIMA (Auto Regressive Integrated Moving Average) 
# to predict future values in a time series 
# based on past trends and pattern
#
####################################
 
# Step 1: Convert Data to a Time Series Object
# Sum the two keywords for each disease of the diseases to create a time series object
# Initialize an empty list to store the results
consolidated_list <- list()

# Loop through each symptom and calculate the average hits
for (symptom in names(symptom_keywords)) {
  df_name <- paste0("df_", symptom)
  df <- get(df_name)
  
  consolidated_list[[symptom]] <- df %>%
    filter(keyword %in% symptom_keywords[[symptom]]) %>%
    group_by(date) %>%
    summarize(avg.hits = mean(hits, na.rm = TRUE))
}

# Access the consolidated data for each symptom
Influenza_cons <- consolidated_list$Influenza
Pollen_cons <- consolidated_list$Pollen_Allergies
SAD_cons <- consolidated_list$SAD
Cold_Weather_cons <- consolidated_list$Cold_Weather
Heatwave_cons <- consolidated_list$Heatwave

# Convert the data to time series format (assuming 'value' column contains the data)
Influenza_ts <- ts(Influenza_cons$avg.hits, start = c(2022, 1), frequency = 52.18)
Pollen_ts <- ts(Pollen_cons$avg.hits, start = c(2022, 1), frequency = 52.18)
SAD_ts <- ts(SAD_cons$avg.hits, start = c(2022, 1), frequency = 52.18)
Cold_Weather_ts <- ts(Cold_Weather_cons$avg.hits, start = c(2022, 1), frequency = 52.18)
Heatwave_ts <- ts(Heatwave_cons$avg.hits, start = c(2022, 1), frequency = 52.18)

# Plot the time series data
ggplot(Influenza_ts, aes(x = time(Influenza_ts), y = Influenza_ts)) +
  geom_line(color = "blue", linewidth = 1) +  # Add line graph
  geom_point(color = "blue", size = 3) +      # Add points for each data point
  labs(
    title = "Influenza Time Series",
    x = "Time",
    y = "Influenza Count"
  ) +
  theme_minimal()

ggplot(Pollen_ts, aes(x = time(Pollen_ts), y = Pollen_ts)) +
  geom_line(color = "green", linewidth = 1) +  # Add line graph
  geom_point(color = "green", size = 3) +      # Add points for each data point
  labs(
    title = "Pollen Time Series",
    x = "Time",
    y = "Pollen Count"
  ) +
  theme_minimal()

ggplot(SAD_ts, aes(x = time(SAD_ts), y = SAD_ts)) +
  geom_line(color = "red", linewidth = 1) +  # Add line graph
  geom_point(color = "red", size = 3) +      # Add points for each data point
  labs(
    title = "SAD Time Series",
    x = "Time",
    y = "SAD Count"
  ) +
  theme_minimal()

ggplot(Cold_Weather_ts, aes(x = time(Cold_Weather_ts), y = Cold_Weather_ts)) +
  geom_line(color = "orange", linewidth = 1) +  # Add line graph
  geom_point(color = "orange", size = 3) +      # Add points for each data point
  labs(
    title = "Cold Weather Time Series",
    x = "Time",
    y = "Cold Weather Count"
  ) +
  theme_minimal()

ggplot(Heatwave_ts, aes(x = time(Heatwave_ts), y = Heatwave_ts)) +
  geom_line(color = "yellow", linewidth = 1) +  # Add line graph
  geom_point(color = "yellow", size = 3) +      # Add points for each data point
  labs(
    title = "Heatwave Time Series",
    x = "Time",
    y = "Heatwave Count"
  ) +
  theme_minimal()


# Plot the time series
plot(Pollen_ts, main = "Pollen Time Series", xlab = "Time", ylab = "Pollen Count")
plot(Influenza_ts, main = "Influenza Time Series", xlab = "Time", ylab = "Influenza Count")
plot(SAD_ts, main = "SAD Time Series", xlab = "Time", ylab = "SAD Count")
plot(Cold_Weather_ts, main = "Cold Weather Time Series", xlab = "Time", ylab = "Cold Weather Count")
plot(Heatwave_ts, main = "Heatwave Time Series", xlab = "Time", ylab = "Heatwave Count")

# Fit ARIMA model for eacg of the disease
arima_Pollen <- auto.arima(Pollen_ts)
arima_Influenza <- auto.arima(Influenza_ts)
arima_SAD <- auto.arima(SAD_ts)
arima_Cold_Weather <- auto.arima(Cold_Weather_ts)
arima_Heatwave <- auto.arima(Heatwave_ts)

# Summary of ARIMA model for each of the disease
summary(arima_Pollen)
summary(arima_Influenza)
summary(arima_SAD)
summary(arima_Cold_Weather)
summary(arima_Heatwave)

# Forecast future values using ARIMA model
# Forecast the next 12 months for each of the disease
forecast_Influenza <- forecast(arima_Influenza, h = 52)
forecast_Pollen <- forecast(arima_Pollen, h = 52)
forecast_SAD <- forecast(arima_SAD, h = 52)
forecast_Cold_Weather <- forecast(arima_Cold_Weather, h = 52)
forecast_Heatwave <- forecast(arima_Heatwave, h = 52)

# Plot forecast for Pollen
plot(forecast_Influenza, main = "Influenza Forecast")
plot(forecast_Pollen, main = "Pollen Allergies Forecast")
plot(forecast_SAD, main = "SAD Forecast")
plot(forecast_Cold_Weather, main = "Cold Weather Forecast")
plot(forecast_Heatwave, main = "Heatwave Forecast")

# Evaluate the accuracy of the Pollen forecast
accuracy(forecast_Influenza)
accuracy(forecast_Pollen)
accuracy(forecast_SAD)
accuracy(forecast_Cold_Weather)
accuracy(forecast_Heatwave)

## Validating the ARIMA Model Predictions by comparing the predictions of the model to the actual search volume data of September to December 2024
# Create for the time-series search volume data from January 2022 to September 2024 for each of the seasonal disease
# Subset the data for January 2022 to September 2024
training_data_Influenza <- subset(Influenza_cons, date >= as.Date("2022-01-01") & date <= as.Date("2024-09-30"))
training_data_Pollen <- subset(Pollen_cons, date >= as.Date("2022-01-01") & date <= as.Date("2024-09-30"))
training_data_SAD <- subset(SAD_cons, date >= as.Date("2022-01-01") & date <= as.Date("2024-09-30"))
training_data_Cold_Weather <- subset(Cold_Weather_cons, date >= as.Date("2022-01-01") & date <= as.Date("2024-09-30"))
training_data_Heatwave <- subset(Heatwave_cons, date >= as.Date("2022-01-01") & date <= as.Date("2024-09-30"))

# Convert the training data to time series format
Influenza_ts_training <- ts(training_data_Influenza$avg.hits, 
                   start = c(2022, 1), 
                   frequency = 52)  # Weekly data of Influenza

Pollen_ts_training <- ts(training_data_Pollen$avg.hits,
                    start = c(2022, 1),
                    frequency = 52)  # Weekly data of Pollen Allergies

SAD_ts_training <- ts(training_data_SAD$avg.hits,
                    start = c(2022, 1),
                    frequency = 52)  # Weekly data of SAD

Cold_Weather_ts_training <- ts(training_data_Cold_Weather$avg.hits,
                    start = c(2022, 1),
                    frequency = 52)  # Weekly data of Cold Weather

Heatwave_ts_training <- ts(training_data_Heatwave$avg.hits,
                    start = c(2022, 1),
                    frequency = 52)  # Weekly data of Heatwave

# Generate the predictions for the period of September to December 2024
# Fit the ARIMA model to data up to September 2024
Influenza_Test <- auto.arima(Influenza_ts_training)
Pollen_Test <- auto.arima(Pollen_ts_training)
SAD_Test <- auto.arima(SAD_ts_training)
Cold_Weather_Test <- auto.arima(Cold_Weather_ts_training)
Heatwave_Test <- auto.arima(Heatwave_ts_training)

# Forecast for October to December 2024
Forecast_oct_dec_Influenza <- forecast(Influenza_Test, h = 13)  # h = 3 months
Forecast_oct_dec_Pollen <- forecast(Pollen_Test, h = 13)  # h = 3 months
Forecast_oct_dec_SAD <- forecast(SAD_Test, h = 13)  # h = 3 months
Forecast_oct_dec_Cold_Weather <- forecast(Cold_Weather_Test, h = 13)  # h = 3 months
Forecast_oct_dec_Heatwave <- forecast(Heatwave_Test, h = 13)  # h = 3 months

# View the first few observations
head(Influenza_ts_training)
head(Pollen_ts_training)
head(SAD_ts_training)
head(Cold_Weather_ts_training)
head(Heatwave_ts_training)

# Plot and save the time series
png("ARIMA_Influenza_Validation_Plot.png", width = 800, height = 600)  # Open PNG device
    plot(Forecast_oct_dec_Influenza, main = "Influenza Search Volume ARIMA Forecast Validation", 
     xlab = "Time", ylab = "Average Hits", col = "blue", type = "o")
dev.off()  # Close the device

png("ARIMA_Pollen_Validation_Plot.png", width = 800, height = 600)
plot(Forecast_oct_dec_Pollen, main = "Pollen Allergies Search Volume ARIMA Forecast Validation", 
     xlab = "Time", ylab = "Average Hits", col = "green", type = "o")
dev.off()

png("ARIMA_SAD_Validation_Plot.png", width = 800, height = 600)
plot(Forecast_oct_dec_SAD, main = "SAD Search Volume ARIMA Forecast Validation", 
     xlab = "Time", ylab = "Average Hits", col = "red", type = "o")
dev.off()

png("ARIMA_Cold Weather_Validation_Plot.png", width = 800, height = 600)
plot(Forecast_oct_dec_Cold_Weather, main = "Cold Weather Search Volume ARIMA Forecast Validation", 
     xlab = "Time", ylab = "Average Hits", col = "orange", type = "o")
dev.off()

png("ARIMA_Heatwave_Validation_Plot.png", width = 800, height = 600)
plot(Forecast_oct_dec_Heatwave, main = "Heatwave Search Volume ARIMA Forecast Validation", 
     xlab = "Time", ylab = "Average Hits", col = "yellow", type = "o")
dev.off()

# Create a ggplot object
p <- ggplot(Forecast_oct_dec_Influenza, aes(x = date, y = avg.hits)) +
     geom_line() +
     labs(title = "Influenza Search Volume", x = "Date", y = "Average Hits")

# Save the plot
ggsave("my_ggplot.png", plot = p, width = 8, height = 6, dpi = 300)

predicted_values <- forecast_oct_dec_Influenza$mean

# Print the predicted values
print(predicted_values)
