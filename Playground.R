library(gtrendsR)

# Your list of keywords
keywords <- c("flu", "influenza", "pollen", "allergies", "virus", "covid", "fever")

# Split into batches of 5 or fewer
keyword_batches <- split(keywords, ceiling(seq_along(keywords) / 5))

# Loop through each batch and fetch data
results <- lapply(keyword_batches, function(batch) {
  gtrends(keyword = batch, geo = "US", time = "2022-01-01 2023-01-01")
})

# Combine results if needed
combined_results <- do.call(rbind, lapply(results, function(res) res$interest_over_time))

# Lists of keywords
keywords.All <- c("flu", "influenza", "pollen", "allergies", "sneezing", "dry skin", "hypothermia", "frostbite", "SAD", "depression", "sleep disorder", "heatstroke", "dehydration", "sunburn")

# Split into batches of 5 or fewer
keyword_batches <- split(keywords.All, ceiling(seq_along(keywords.All) / 5))

# Loop through each batch and fetch data
results <- lapply(keyword_batches, function(batch) {
  gtrends(keyword = batch, geo = "US", time = "2022-01-01 2024-12-31")
})

# Combine results if needed
combined_results <- do.call(rbind, lapply(results, function(res) res$interest_over_time))

df0 <- gtrends(
  keyword = c("flu", "influenza", "pollen", "allergies", "sneezing", "dry skin", "hypothermia", "frostbite", "SAD", "depression", "sleep disorder", "heatstroke", "dehydration", "sunburn"),
  geo = 'US',
  time = "2023-01-01 2024-12-31",
  gprop = "web")
