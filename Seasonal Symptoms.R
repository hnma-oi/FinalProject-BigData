
# Install and load the necessary package for accessing Google Trends data
# Only run the install line once if not already installed
# install.packages("gtrendsR")

####################################
#
# Preparing Google Trend data
#
####################################
# Load required library
library(gtrendsR)

# Get Google Trends data, the keywords for this query is "flu", "pollen", "allergies", "sneezing", as they are symptoms of seasonal transitions
df0 <- gtrends(
  keyword = c("flu", "pollen", "allergies", "sneezing"),
  geo = 'UK',
  time = "2024-01-01 2024-12-31",
  gprop = "web")

# Extract interest over time as a data frame from the google trends data
mylist <- data.frame(df0$interest_over_time)

# Convert the time column to a proper datetime format with a valid time zone. Since we are collecting data from United Kingdom, then we are using United Kingdom's timezone
mylist$time <- as.POSIXct(mylist$time, tz = "Europe/London")

# Copy the selected data from the Google Trends query and back it up to csv file
filename <- paste("TrendData/",  "Seasonal Symtoms",
                  format(Sys.time(), "%b%d_%H%M_%Y"),'.csv', sep = "")
write.csv(mylist, filename, fileEncoding = "Shift-JIS", row.names = FALSE)
# To Mac users, "UTF-8" may work, not "Shift-JIS"

# Import reshape2 library to modify the data
library(reshape2)

df2 <- dcast(mylist, time ~ keyword, value.var = "hits", fun.aggregate = mean) # Refer df2 from mylist and only get the hits, date, and keyword data
df2$date <- as.Date(df2$date) # Refer df2$date from the "date" column in df2
df2$days <- as.integer(difftime(df2$date, as.POSIXct('2020-04-13'), units="days")) # In Df2$days use integer to calculate the difference of days with 20th April 2020 as the start reference
df2$discontinuity <- as.factor(ifelse(df2$days < 0,0,1)) # Refer df2$discontinuity as a factor between 0 and 1 depending whether the date is before or after the start reference date
df2$score <- rowMeans(df2[,c("flu","pollen")], na.rm = TRUE) # Refer df2$score as the average value of all of the search queries's hits


# Refer df3 from df2 and back up the data into csv file
df3<-df2
write.csv(df3, 'df3_try.csv', fileEncoding = "Shift-JIS", row.names = FALSE)
# To Mac users, "UTF-8" may work, not "Shift-JIS"


####################################
#
# Plotting the Data
#
####################################

# Plot the data
library(ggplot2)

df3_plot <- df3[,c(5:7)]
fig1 <- ggplot(df3_plot, aes(x=days, y=score, size=discontinuity)) +  geom_point() + geom_vline(xintercept = 0, linetype="dotted") + geom_smooth(method='lm', se=FALSE) + guides(colour = "legend", size = "none") + scale_size_manual(values=c(1,1))
fig1
ggsave('Figure 1.png',fig1, device = 'png')


####################################
#
# Regression Discontinuity Analysis
#
####################################

fit1 <- lm(flu ~ pollen + days, data=df3)
fit1.ci<- confint(fit1)
table1 <- data.frame(summary(fit1)$coefficients,fit1.ci)
table1 <- table1[,c(1,4:6)]

colnames(table1) <- c('flu','p','CI_l','CI_h')
table1 <- round(table1,digits =2)
write.csv(table1, 'table 1.csv')

#Discontinuity effect sizes
library(psych)
a<- describe(df3)
effect_size <-table1[1,1]/a[8,4] #flu
effect_size <- table1[2, 1] / a[4]  # 'a[4]' gives the standard deviation of 'score'
effect_size  

