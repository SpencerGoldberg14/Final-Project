# read in data
Albany_County <- read.csv("/cloud/project/Albany-County-Heavy-Snow-Data.csv")
Oneida_County <- read.csv("/cloud/project/Oneida-County-Heavy-Snow-Data.csv")
Suffolk_County <- read.csv("/cloud/project/Suffolk-County-Heavy-Snow-Data.csv")

Oneida_County.Temp <- read.csv("/cloud/project/Oneida_County.Temp.csv")

# install packages
install.packages(c("dplyr", "lubridate", "ggplot2", "forecast", "tidyverse"))
# load package
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(tidyverse)

# parse date
exampleDate <- c("01/02/1996")
mdy(exampleDate)

# plot heavy snowfall occurence trends over time by county
# plot heavy snowfall occurence trends over time in Albany County
# convert date column to date type
Albany_County$BEGIN_DATE <- mdy(Albany_County$BEGIN_DATE)

# extract year from date column
Albany_County$Year <- year(Albany_County$BEGIN_DATE)

# group by year and count occurrences
ALByearly_snowfall <- Albany_County %>%
  group_by(Year) %>%
  summarize(Occurrences = n())

# plot occurrences over time
ggplot(data = ALByearly_snowfall, aes(x = Year, y = Occurrences)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Albany County Heavy Snowfall Occurrences Over Time",
    x = "Year",
    y = "Occurrences"
  ) +
  theme_minimal()

# plot heavy snowfall occurence trends over time in Oneida County
# convert date column to date type
Oneida_County$BEGIN_DATE <- mdy(Oneida_County$BEGIN_DATE)

# extract year from date column
Oneida_County$Year <- year(Oneida_County$BEGIN_DATE)

# group by year and count occurrences
ONyearly_snowfall <- Oneida_County %>%
  group_by(Year) %>%
  summarize(Occurrences = n())

# plot occurrences over time
ggplot(data = ONyearly_snowfall, aes(x = Year, y = Occurrences)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Oneida County Heavy Snowfall Occurrences Over Time",
    x = "Year",
    y = "Occurrences"
  ) +
  theme_minimal()

# plot heavy snowfall occurence trends over time in Suffolk County
# convert date column to date type
Suffolk_County_WS$BEGIN_DATE <- mdy(Suffolk_County_WS$BEGIN_DATE)

# extract year from date column
Suffolk_County_WS$Year <- year(Suffolk_County$BEGIN_DATE)

# group by year and count occurrences
SUyearly_snowfall <- Suffolk_County %>%
  group_by(Year) %>%
  summarize(Occurrences = n())

# plot occurrences over time
ggplot(data = SUyearly_snowfall, aes(x = Year, y = Occurrences)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Suffolk County Heavy Snowfall Occurrences Over Time",
    x = "Year",
    y = "Occurrences"
  ) +
  theme_minimal()

# Oneida County winter average temperature time series
# filter for winter months (Dec to Feb)
ON_winter_months <- Oneida_County.Temp %>%
  filter(substr(X...Oneida.County, 5, 6) %in% c("12", "01", "02"))

# create new columns for year and month
ON_winter_months$Year <- as.numeric(substr(ON_winter_months$X...Oneida.County, 1, 4))
ON_winter_months$Month <- as.numeric(substr(ON_winter_months$X...Oneida.County, 5, 6))

# ensure the temp column is numeric 
ON_winter_months$New.York.Average.Temperature <- as.numeric(ON_winter_months$New.York.Average.Temperature)

# remove rows with NA values in the temp column
ON_winter_months <- ON_winter_months %>% filter(!is.na(New.York.Average.Temperature))

# create time series 
ON_winter_ts <- ts(ON_winter_months$New.York.Average.Temperature, 
                   start = c(min(ON_winter_months$Year), min(ON_winter_months$Month)), 
                   frequency = 3)

# decompose time series
ON_winter_decomp <- decompose(ON_winter_ts)

# plot decomposition
plot(ON_winter_decomp)

# Oneida County regression between winter temp and heavy snow occurences
# extract month from the BEGIN_DATE column
Oneida_County$Month <- month(Oneida_County$BEGIN_DATE)

# filter for winter months (Dec-Feb) and count occurrences per month
snow_events_winter <- Oneida_County %>%
  filter(Month %in% c(12, 1, 2)) %>%
  group_by(Year, Month) %>%
  summarize(snow_count = n())  

# merge the winter temperature time series with the snow event counts
ON_merged_data <- left_join(ON_winter_months, snow_events_winter, by = c("Year", "Month"))

# log-transform temperature
merged_data$log_temperature <- log(merged_data$New.York.Average.Temperature)

# regression with log-transformed temperature
model <- lm(snow_count ~ New.York.Average.Temperature, data = merged_data)

# summarize model
summary(model)

