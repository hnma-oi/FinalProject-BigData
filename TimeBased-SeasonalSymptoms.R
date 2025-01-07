
## Load all of the necessary libraries for the script
library(gtrendsR)
library(reshape2)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)


## Disease 1, Influenza ("flu", "influenza") October to March
## Disease 2, Pollen allergies ("pollen", "allergies") March to June
## Disease 3, Cold weather symptoms ("dry skin", "hypothermia", "frostbite") December to February
## Disease 4, Seasonal Affective Dissorder ("depression", "sleep disorder") November to February
## Disease 5, Heatwave symptoms ("heatstroke", "dehydration", "sunburn") June to August

# Get Google Trends data for the selected keywords for all of the diseases in the United States from 2022 to 2024
# Since we have multiple keywords, we need to split them into batches of 5 or fewer to get around the Google Trends API limit of 5 keywords per query

Sys.setenv(TZ = "America/New_York")

# GTrends command to fetch all of the search volume data for the selected keywords in one go, but it may exceed the API limit and return an error 
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

#convert the data to a data frame and only get the interest over time for each of the seasonal disease
df_influenza <- data.frame(influenza$interest_over_time)
df_pollen_allergies <- data.frame(pollen$interest_over_time)
df_cold_weather <- data.frame(cold_weather$interest_over_time)
df_sad <- data.frame(SAD$interest_over_time)
df_heatwave <- data.frame(heatwave$interest_over_time)

# Combine all of the results into a list
result <- list(influenza = df_influenza, pollen = df_pollen, cold_weather = df_cold_weather, SAD = df_SAD, heatwave = df_heatwave)

# Copy the selected data from the Google Trends query and back it up to a csv file
symptoms <- list("Influenza" = df_influenza, 
                 "Pollen" = df_pollen, 
                 "cold_weather" = df_cold_weather, 
                 "Seasonal Affective Disorder" = df_SAD, 
                 "Heatwave" = df_heatwave)

for (symptom in names(symptoms)) {
  filename <- paste("TrendData/", symptom, 
                    format(Sys.time(), "%b%d_%H%M_%Y"), '.csv', sep = "")
  write.csv(symptoms[[symptom]], filename, fileEncoding = "UTF-8", row.names = FALSE)
}


# Manipulate the data from result to create a time series plot
consolidated <- rbind(df_influenza, df_pollen, df_SAD, df_cold_weather, df_heatwave)

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
  Influenza = c("influenza", "flu"),
  pollen_allergies = c("pollen", "allergies"),
  SAD = c("depression", "sleep disorder"),
  cold_weather = c("dry skin", "hypothermia"),
  heatwave = c("dehydration", "sun burn")
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
    Pollen_allergies = unique(Pollen_allergies),
    SAD = unique(SAD),
    cold_weather = unique(cold_weather),
    heatwave = unique(heatwave),
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
  df_name <- paste0("df_", tolower(symptom))
  df <- get(df_name)
  
  consolidated_list[[symptom]] <- df %>%
    filter(keyword %in% symptom_keywords[[symptom]]) %>%
    group_by(date) %>%
    summarize(avg.hits = mean(hits, na.rm = TRUE))
}

# Access the consolidated data for each symptom
influenza_cons <- consolidated_list$Influenza
pollen_cons <- consolidated_list$Pollen_allergies
SAD_cons <- consolidated_list$SAD
cold_weather_cons <- consolidated_list$cold_weather
heatwave_cons <- consolidated_list$heatwave

# Convert the data to time series format (assuming 'value' column contains the data)
influenza_ts <- ts(influenza_cons$avg.hits, start = c(2022, 1), frequency = 52.18)
pollen_ts <- ts(pollen_cons$avg.hits, start = c(2022, 1), frequency = 52.18)
SAD_ts <- ts(SAD_cons$avg.hits, start = c(2022, 1), frequency = 52.18)
cold_weather_ts <- ts(cold_weather_cons$avg.hits, start = c(2022, 1), frequency = 52.18)
heatwave_ts <- ts(heatwave_cons$avg.hits, start = c(2022, 1), frequency = 52.18)

# Plot the time series data
ggplot(influenza_ts, aes(x = time(influenza_ts), y = influenza_ts)) +
  geom_line(color = "blue", linewidth = 1) +  # Add line graph
  geom_point(color = "blue", size = 3) +      # Add points for each data point
  labs(
    title = "Influenza Time Series",
    x = "Time",
    y = "Influenza Count"
  ) +
  theme_minimal()

ggplot(pollen_ts, aes(x = time(pollen_ts), y = pollen_ts)) +
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

ggplot(cold_weather_ts, aes(x = time(cold_weather_ts), y = cold_weather_ts)) +
  geom_line(color = "orange", linewidth = 1) +  # Add line graph
  geom_point(color = "orange", size = 3) +      # Add points for each data point
  labs(
    title = "Cold Weather Time Series",
    x = "Time",
    y = "Cold Weather Count"
  ) +
  theme_minimal()

ggplot(heatwave_ts, aes(x = time(heatwave_ts), y = heatwave_ts)) +
  geom_line(color = "yellow", linewidth = 1) +  # Add line graph
  geom_point(color = "yellow", size = 3) +      # Add points for each data point
  labs(
    title = "Heatwave Time Series",
    x = "Time",
    y = "Heatwave Count"
  ) +
  theme_minimal()


# Plot the time series
plot(pollen_ts, main = "Pollen Time Series", xlab = "Time", ylab = "Pollen Count")
plot(influenza_ts, main = "Influenza Time Series", xlab = "Time", ylab = "Influenza Count")
plot(SAD_ts, main = "SAD Time Series", xlab = "Time", ylab = "SAD Count")
plot(cold_weather_ts, main = "Cold Weather Time Series", xlab = "Time", ylab = "Cold Weather Count")
plot(heatwave_ts, main = "Heatwave Time Series", xlab = "Time", ylab = "Heatwave Count")

# Fit ARIMA model for eacg of the disease
arima_pollen <- auto.arima(pollen_ts)
arima_influenza <- auto.arima(influenza_ts)
arima_SAD <- auto.arima(SAD_ts)
arima_cold_weather <- auto.arima(cold_weather_ts)
arima_heatwave <- auto.arima(heatwave_ts)

# Summary of ARIMA model for each of the disease
summary(arima_pollen)
summary(arima_influenza)
summary(arima_SAD)
summary(arima_cold_weather)
summary(arima_heatwave)

# Forecast future values using ARIMA model
# Forecast the next 12 months for each of the disease
forecast_pollen <- forecast(arima_pollen, h = 52)
forecast_influenza <- forecast(arima_influenza, h = 52)
forecast_SAD <- forecast(arima_SAD, h = 52)
forecast_cold_weather <- forecast(arima_cold_weather, h = 52)
forecast_heatwave <- forecast(arima_heatwave, h = 52)

# Plot forecast for Pollen
plot(forecast_pollen, main = "Pollen Allergies Forecast")
plot(forecast_influenza, main = "Influenza Forecast")
plot(forecast_SAD, main = "SAD Forecast")
plot(forecast_cold_weather, main = "Cold Weather Forecast")
plot(forecast_heatwave, main = "Heatwave Forecast")

# Evaluate the accuracy of the Pollen forecast
accuracy(forecast_pollen)
accuracy(forecast_influenza)
accuracy(forecast_SAD)
accuracy(forecast_cold_weather)
accuracy(forecast_heatwave)

