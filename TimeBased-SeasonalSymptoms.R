
## Load all of the necessary libraries for the script
library(gtrendsR)
library(reshape2)
library(ggplot2)

## Disease 1, Influnza ("flu", "influenza") October to March
## Disease 2, Pollen allergies ("pollen", "allergies") March to June
## Disease 3, Cold weather symptoms ("sneezing", "dry skin", "hypothermia", "frostbite") December to February
## Disease 4, Seasonal Affective Dissorder ("SAD", "depression", "sleep disorder") November to February
## Disease 5, Heatwave symptoms ("heatstroke", "dehydration", "sunburn") June to August

# Get Google Trends data for the selected keywords for all of the diseases in the United States from 2022 to 2024
# Since we have multiple keywords, we need to split them into batches of 5 or fewer to get around the Google Trends API limit of 5 keywords per query

Sys.setenv(TZ = "Japan")
# Lists of keywords
keywords.All <- c("flu", "influenza", "pollen", "allergies", "sneezing", "dry skin", "hypothermia", "frostbite", "SAD", "depression", "sleep disorder", "heatstroke", "dehydration", "sunburn")

# Split into batches of 5 or fewer
keyword_batches <- split(keywords.All, ceiling(seq_along(keywords.All) / 5))

for (batch in keyword_batches) {

  Sys.sleep(5) # Wait 5 seconds between requests
  result <- gtrends(keyword = batch, geo = "US", time = "2023-01-01 2024-12-31")
  print(result)
}


# Loop through each batch and fetch data
results <- lapply(keyword_batches, function(batch) {
  gtrends(keyword = batch, geo = "US", time = "202-01-01 2024-12-31")
})

# Combine results if needed
combined_results <- do.call(rbind, lapply(results, function(res) res$interest_over_time))

# Extract the interest over time data as a data frame
mylist <- data.frame(df0$interest_over_time)

# Convert the time column to a proper datetime format with a valid time zone. Since we are collecting data from United States, then we are using United State's timezone
mylist$time <- as.POSIXct(mylist$time, tz = "America/New_York")

# Copy the selected data from the Google Trends query and back it up to a csv file
filename <- paste("TrendData/",  "Seasonal Symptoms",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(mylist, filename, fileEncoding = "UTF-8", row.names = FALSE)

# Since we have multiple keywords, we need to reshape the data to calculate the average score for each diseases
df2 <- dcast(mylist, time ~ keyword, value.var = "hits", fun.aggregate = mean)
# Reshape the data to calculate the average score for "flu" and "infuenza" for Disease 1, and so on for the other diseases.
df2$flu <- rowMeans(df2[,c("flu", "influenza")], na.rm = TRUE)
df2$pollen <- rowMeans(df2[,c("pollen", "allergies")], na.rm = TRUE)
df2$cold_weather <- rowMeans(df2[,c("sneezing", "dry skin", "hypothermia", "frostbite")], na.rm = TRUE)
df2$SAD <- rowMeans(df2[,c("SAD", "depression", "sleep disorder")], na.rm = TRUE)
df2$heatwave <- rowMeans(df2[,c("heatstroke", "dehydration", "sunburn")], na.rm = TRUE)
