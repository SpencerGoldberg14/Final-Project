# read in data
Albany_County <- read.csv("/cloud/project/Albany-County-Heavy-Snow-Data.csv")
Oneida_County <- read.csv("/cloud/project/Oneida-County-Heavy-Snow-Data.csv")
Suffolk_County <- read.csv("/cloud/project/Suffolk-County-Heavy-Snow-Data.csv")

Albany_County.Temp <- read.csv("/cloud/project/Albany_County.Temp.csv")
Oneida_County.Temp <- read.csv("/cloud/project/Oneida_County.Temp.csv")
Suffolk_County.Temp <- read.csv("/cloud/project/Suffolk_County.Temp.csv")

# install packages
install.packages(c("dplyr", "lubridate", "ggplot2", "forecast", "tidyverse"))
# load package
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(tidyverse)

# plot heavy snowfall occurrence trends over time by county
# plot heavy snowfall occurrence trends over time in Albany County
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
Oneida_County$BEGIN_DATE <- ymd(Oneida_County$BEGIN_DATE)

# extract year from date column
Oneida_County$Year <- year(Oneida_County$BEGIN_DATE)

# group by year and count occurrences
ON_yearly_snowfall <- Oneida_County %>%
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
Suffolk_County$BEGIN_DATE <- mdy(Suffolk_County$BEGIN_DATE)

# extract year from date column
Suffolk_County$Year <- year(Suffolk_County$BEGIN_DATE)

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
# end of plots for heavy snow occurrences #

# Albany County winter average temperature time series and decomposition
# filter for winter months (Dec to Feb)
ALB_winter_months <- Albany_County.Temp %>%
  filter(substr(X...Albany.County, 5, 6) %in% c("12", "01", "02"))

# create new columns for year and month
ALB_winter_months$Year <- as.numeric(substr(ALB_winter_months$X...Albany.County, 1, 4))
ALB_winter_months$Month <- as.numeric(substr(ALB_winter_months$X...Albany.County, 5, 6))

# ensure the temp column is numeric 
ALB_winter_months$New.York.Average.Temperature <- as.numeric(ALB_winter_months$New.York.Average.Temperature)

# remove rows with NA values in the temp column
ALB_winter_months <- ALB_winter_months %>% filter(!is.na(New.York.Average.Temperature))

# create time series 
ALB_winter_ts <- ts(ALB_winter_months$New.York.Average.Temperature, 
                   start = c(min(ALB_winter_months$Year), min(ALB_winter_months$Month)), 
                   frequency = 3)

# decompose time series
ALB_winter_decomp <- decompose(ALB_winter_ts)

# plot decomposition
plot(ALB_winter_decomp)
# end of Albany County time series and decomposition #

# Albany County regression between winter temp and heavy snow occurrences
# extract month from the BEGIN_DATE column
Albany_County$Month <- month(Albany_County$BEGIN_DATE)

# filter for winter months (Dec-Feb) and count occurrences per month
ALB_snow_events_winter <- Albany_County %>%
  filter(Month %in% c(12, 1, 2)) %>%
  group_by(Year, Month) %>%
  summarize(ALB_snow_count = n())  

# merge the winter temperature time series with the snow event counts
ALB_merged_data <- left_join(ALB_winter_months, ALB_snow_events_winter, by = c("Year", "Month"))

# log-transform temperature
ALB_merged_data$log_temperature <- log(ALB_merged_data$New.York.Average.Temperature)

# regression with log-transformed temperature
ALB_model <- lm(ALB_snow_count ~ New.York.Average.Temperature, data = ALB_merged_data)

# summarize model
summary(ALB_model)
# end of Albany County regression #

# Oneida County winter average temperature time series and decomposition
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
# end of Oneida County time series and decomposition #

# Oneida County regression between winter temp and heavy snow occurrences
# extract month from the BEGIN_DATE column
Oneida_County$Month <- month(Oneida_County$BEGIN_DATE)

# filter for winter months (Dec-Feb) and count occurrences per month
ON_snow_events_winter <- Oneida_County %>%
  filter(Month %in% c(12, 1, 2)) %>%
  group_by(Year, Month) %>%
  summarize(ON_snow_count = n())  

# merge the winter temperature time series with the snow event counts
ON_merged_data <- left_join(ON_winter_months, ON_snow_events_winter, by = c("Year", "Month"))

# log-transform temperature
ON_merged_data$log_temperature <- log(ON_merged_data$New.York.Average.Temperature)

# regression with log-transformed temperature
ON_model <- lm(ON_snow_count ~ New.York.Average.Temperature, data = ON_merged_data)

# summarize model
summary(ON_model)
# end of Oneida County regression #

# Suffolk County winter average temperature time series and decomposition
# filter for winter months (Dec to Feb)
SU_winter_months <- Suffolk_County.Temp %>%
  filter(substr(X...Suffolk.County, 5, 6) %in% c("12", "01", "02"))

# create new columns for year and month
SU_winter_months$Year <- as.numeric(substr(SU_winter_months$X...Suffolk.County, 1, 4))
SU_winter_months$Month <- as.numeric(substr(SU_winter_months$X...Suffolk.County, 5, 6))

# ensure the temp column is numeric 
SU_winter_months$New.York.Average.Temperature <- as.numeric(SU_winter_months$New.York.Average.Temperature)

# remove rows with NA values in the temp column
SU_winter_months <- SU_winter_months %>% filter(!is.na(New.York.Average.Temperature))

# create time series 
SU_winter_ts <- ts(SU_winter_months$New.York.Average.Temperature, 
                   start = c(min(SU_winter_months$Year), min(SU_winter_months$Month)), 
                   frequency = 3)

# decompose time series
SU_winter_decomp <- decompose(SU_winter_ts)

# plot decomposition
plot(SU_winter_decomp)
# end of Suffolk County time series and decomposition #

# Suffolk County regression between winter temp and heavy snow occurrences
# extract month from the BEGIN_DATE column
Suffolk_County$Month <- month(Suffolk_County$BEGIN_DATE)

# filter for winter months (Dec-Feb) and count occurrences per month
SU_snow_events_winter <- Suffolk_County %>%
  filter(Month %in% c(12, 1, 2)) %>%
  group_by(Year, Month) %>%
  summarize(SU_snow_count = n())  

# merge the winter temperature time series with the snow event counts
SU_merged_data <- left_join(SU_winter_months, SU_snow_events_winter, by = c("Year", "Month"))

# log-transform temperature
SU_merged_data$log_temperature <- log(SU_merged_data$New.York.Average.Temperature)

# regression with log-transformed temperature
SU_model <- lm(SU_snow_count ~ New.York.Average.Temperature, data = SU_merged_data)

# summarize model
summary(SU_model)
# end of Suffolk County regression #


