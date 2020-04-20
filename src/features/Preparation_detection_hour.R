
##########################################
# Calculate hourly detection percentages #
##########################################

# Jolien Goosens - Flanders Marine Institute (VLIZ) / Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2


#### Load packages ####
library(plyr)
library(dplyr)
library(lubridate)

#### Get data ####
ev <- read.csv("data/raw/tilt_noise.csv", stringsAsFactors = F) # tilt and noise measured by each receiver
df <- read.csv("data/interim/df_det.csv", stringsAsFactors = F) # detection data per 10 minutes
rectrans <- read.csv("data/raw/receiver_transmitterbuiltin.csv", stringsAsFactors = F) # table with receiver and built-in transmitter combinations


#### Data organization ####
# Detection data
df <- df %>% 
  mutate(
    Time = parse_date_time(Time, orders = "ymd HMS", tz= "UTC"),
    Time_minute = parse_date_time(Time_minute, orders = "ymd HMS", tz= "UTC"),
    Year = as.factor(Year),
    Turbine = as.factor(Turbine),
    Hour = parse_date_time(paste(year(Time_minute), month(Time_minute), day(Time_minute), hour(Time_minute)),
                           orders = "ymd H", tz= "UTC")
  )

# Noise data
ev <- ev %>% # format variables
  mutate(
    Time = parse_date_time(Time, orders = "ymd HMS", tz= "UTC"),
    Year = as.factor(Year),
    Turbine = as.factor(Turbine),
    Hour = parse_date_time(paste(year(Time), month(Time), day(Time), hour(Time)),
                           orders = "ymd H", tz= "UTC"),
    Noise = as.numeric(Noise))

#### Calculate detection percentages per hour ####
# Group detection data per hour and calculate amount of detections per hour (for every transmitter picked up by every receiver at a turbine)
df_hourtrans <- df %>% 
  group_by(Turbine, Hour, TransmitterBuiltin, Transmitter, Distance, Distance_class, Year) %>% 
  summarize(n_det = sum(Detection)) %>% 
  as.data.frame()

# Calculate detection percentage
df_hourtrans <- df_hourtrans %>%
  filter(Distance == 0) %>% # filter for transmitted signals
  mutate(maxi = n_det) %>%  # maxi = number of transmitted signals in that hour (by every transmitter)
  select(Transmitter, Turbine, Hour, Year, maxi) %>%  # selection of variables
  left_join(df_hourtrans) %>% # join the filtered data with the full data
  mutate(percent_count = n_det/maxi) # calculate detection percentage by dividing by maxi

#### Group noise data per hour #### 
evhour <- ev %>% # group per hour and calculate mean, median and sd values
  group_by(Receiver, Year, Turbine, Hour) %>% 
  summarise(Tilt_mean = mean(Tilt),
            Tilt_median = median(Tilt),
            Tilt_sd = sd(Tilt),
            Noise_mean = mean(Noise, na.rm = T),
            Noise_median = median(Noise, na.rm = T),
            Noise_sd = sd(Noise, na.rm = T)) %>% 
  as.data.frame()

#### Join data and calculate summary per turbine ####
# Join
evhour <- left_join(evhour, rectrans)
allhour <- left_join(df_hourtrans, evhour)

# Group and summarise
df_hoursum <- allhour %>% 
  group_by(Turbine, Distance_class, Hour) %>% 
  summarise(mean_perc = mean(percent_count),
            mean_noisemean = mean(Noise_mean),
            mean_tiltmean = mean(Tilt_mean),
            mean_tiltsd = mean(Tilt_sd),
            median_perc = median(percent_count),
            median_noisemedian = median(Noise_median),
            median_tiltmedian = median(Tilt_median),
            median_tiltsd = median(Tilt_sd))

rm(ev, evhour, rectrans, df, df_hourtrans, allhour)

#### Save ev ####
write.csv(df_hoursum, "data/interim/df_hoursum.csv", row.names = F)
