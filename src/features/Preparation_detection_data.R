
###############################################
# Calculate detections of transmitted signals #
###############################################

# Jolien Goosens - Flanders Marine Institute (VLIZ) / Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2


#### Load packages ####
library(plyr)
library(dplyr)
library(lubridate)
library(geosphere)
library(zoo)
library(ggplot2)

#### Get data ####
df <- read.csv("data/raw/detections.csv", stringsAsFactors = F) # detections of built-in transmitters
set <- read.csv("data/raw/stations.csv", stringsAsFactors = F) # station metadata
ev <- read.csv("data/raw/tilt_noise.csv", stringsAsFactors = F) # tilt and noise measured by each receiver
rectrans <- read.csv("data/raw/receiver_transmitterbuiltin.csv", stringsAsFactors = F) # table with receiver and built-in transmitter combinations

# Format time variable
df$Date <- parse_date_time(df$Date, orders = "ymd", tz= "UTC")
df$Time <- parse_date_time(df$Time, orders = "ymd HMS", tz= "UTC")
ev$Time <- parse_date_time(ev$Time, orders = "ymd HMS", tz= "UTC")

# Filter the event data for the period where a sufficiently low amount of fish was present in the area (the detection data is already filtered for this period)
ev <- ev %>% 
  mutate(Date = parse_date_time(date(Time), orders = "ymd", tz= "UTC")) %>% 
  filter(
    Turbine == "F05" &
      Date >= parse_date_time("2017-08-17", orders = "ymd", tz= "UTC")  & 
      Date <  parse_date_time("2017-09-28", orders = "ymd", tz= "UTC")| 
      Turbine %in% c("B10", "C09", "B08") & 
      Date >= parse_date_time("2018-08-19", orders = "ymd", tz= "UTC")  & 
      Date <  parse_date_time("2018-10-08", orders = "ymd", tz= "UTC")) %>% 
  select(-Date)

#### Identify per transmitted signal whether it was detected ####
# Each receiver registers the transmission of its built-in transmitter. The code below checks for every transmitted signal whether it was detected by the five other receivers around the same turbine.
# Note hierarchy of lists: all calculations are performed for every signal (z) of every built-in transmitter (y) at every Turbine (x)
# (long processing of code)
df_temp_list <- lapply(unique(df$Turbine), function(x){ # apply code over list of turbines
  
  # filter for each unique Turbine, define Turbine as x
  df <- df %>% filter(Turbine == x) 
  set <- set %>% filter(Turbine == x) 
  
  # retain only the detections of built-in transmitters around that turbine
  df <- df %>% 
    filter(Transmitter %in% unique(TransmitterBuiltin)) %>% 
    select(Time, Receiver, TransmitterBuiltin, Transmitter, Date, StationName) %>% 
    left_join(set) # join with the set dataframe to use coordinates
  
  # Calculate distance
  settemp <- set %>% select(TransmitterBuiltin, Latitude, Longitude) # temporary subset of settings data
  settemp <- settemp %>% # rename column names to join with detection data
    rename(Transmitter = TransmitterBuiltin,
           Lattrans = Latitude,
           Longtrans = Longitude)
  df <- left_join(df, settemp) # join df_day and settemp to calculate distance
  rm(settemp)
  
  df <- df %>% # calculate distance between transmitter and built-in transmitter
    mutate(Distance = distGeo(matrix(c(df$Longitude, df$Latitude), ncol = 2),
                              matrix(c(df$Longtrans, df$Lattrans), ncol = 2)))
  
  df_geo <- df %>% 
    group_by(Transmitter, TransmitterBuiltin, Distance) %>% summarise() # save distance in df_geo
  
  # Calculations for separate transmitter
  df_temp_y_list <- lapply(unique(df$Transmitter), function(y){ # apply code over list of transmitters
    
    # make a temporary dataframe of Transmitter y with only the outgoing acoustic signals (Distance == 0)
    df_temp <- df %>% 
      filter(Transmitter == y & Distance == 0) %>%
      arrange(Time) %>% 
      mutate(X = c(1:n())) # unique X for each signal sent
    
    df_temp$Time_minute <- df_temp$Time # Create a new time variable
    second(df_temp$Time_minute) <- 0 # adapt the resolution of the time variable (remove seconds)
    
    # make a temporary dataframe with only the detection of acoustic signals (Distance != 0)
    df_temp2 <- df %>% 
      filter(Transmitter == y & Distance != 0) %>%
      mutate(X = NA, # for now, X is NA for the detected signals
             Time_minute = NA) # for now, Time_minute is NA for the detected signals
    
    # rbind those temporary dataframes
    df_temp <- rbind(df_temp, df_temp2)
    
    # Calculations for separate acoustic signals (X)
    df_temp_z_list <- lapply(unique(na.omit(df_temp$X)), function(z){ # apply code over list of X
      
      # filter for each unique X, define X as z, store in different dataframe
      df_temp_sel <- df_temp %>% filter(X == z)
      # filter df_temp for the detections that took place within 1 minute before and 1 minute after the signal was saved on the receiver itself
      df_temp <- df_temp %>% 
        filter(df_temp_sel$Time - minutes(1) <= df_temp$Time & 
                 df_temp$Time < df_temp_sel$Time +minutes(1))
      
      df_temp$X <- df_temp_sel$X # Use that X value for all the detections (max 6 in total)
      df_temp$Time_minute <-  df_temp_sel$Time_minute # Time_minute stamp
      df_temp
    })
    list_df_check <- ldply(df_temp_z_list) # make a dataframe of the list of all X
    
    
    # Make a template of all possible combinations  of X and TransmitterBuiltin
    df_template <- expand.grid(
      X = unique(na.omit(df_temp$X)),
      TransmitterBuiltin = unique(df_temp$TransmitterBuiltin))
    
    df_template$Transmitter <- unique(df_temp$Transmitter) # Transmitter is y
    
    # Join the template with settings and distance
    df_template <- df_template %>% 
      left_join(set) %>% 
      left_join(df_geo)
    
    # Join the template with the data
    df_temp <- left_join(df_template, list_df_check)
    
    # Organization of df_temp
    df_temp <- df_temp %>% 
      select(-Lattrans, -Longtrans, -Receiver) %>% 
      # arrange along X and Distance to fill in NA values later
      arrange(X, Distance) %>% 
      # replace NA values with non-NA value from row above
      mutate(
        Date = na.locf(Date),
        Time_minute = na.locf(Time_minute)
      ) %>% 
      # Add detection variable
      mutate(
        Detection = ifelse(is.na(Time), yes = 0, no = 1)
      )
    
    df_temp
  })
  df_temp_y <- ldply(df_temp_y_list) # make a dataframe of the list of all Transmitters
  
})
df_temp <- ldply(df_temp_list) # make a dataframe of the list of all Turbines

#### Data organization for detection df ####
df_temp <- df_temp %>% 
  select(-X) %>% # remove X variable
  mutate(Distance_class = as.factor( # Add a variable distance_class
    ifelse(Distance == 0, "0", ifelse(
      120 < Distance & Distance < 180, "[120, 180[", ifelse(
        250 < Distance & Distance < 270, "[250, 270[", "[290, 310[")))
  )) 

rm(df_temp_list, set)

#### Save file ###
write.csv(df_temp, "data/interim/df_det.csv", row.names = FALSE) # save the file 


#### Combine detection data with noise data ####
ev <- left_join(ev, rectrans) # join the event data (noise and tilt) with rectrans to get built-in transmitter codes

# Make an extra column of the combination Turbine + Built-in tag
ev$Turb_Builtin <- paste(ev$Turbine, ev$TransmitterBuiltin, sep = '_')
df_temp$Turb_Builtin <- paste(df_temp$Turbine, df_temp$TransmitterBuiltin, sep = '_')

# Interpolate noise
# The noise measurements did not take place at the exact time of transmission. Here we interpolate the noise measurements to the time the transmitted signal was registered by the receiver.
df.list <- lapply(unique(ev$Turb_Builtin), function(x){
  ev <- ev %>%  
    filter(Turb_Builtin == x) %>% 
    arrange(Time) %>% 
    as.data.frame()
  df <- df_temp %>% 
    filter(Distance == 0) %>% 
    filter(Turb_Builtin == x) %>% 
    arrange(Time_minute) %>% 
    as.data.frame()
  ts_noise <- approx(ev$Time, ev$Noise, xout = df$Time_minute, rule = 2, method = "linear", ties = mean)
  df$Noise.ts <- ts_noise$y
  return(df)
})
df_noise <- ldply(df.list) 

df_noise <- df_noise %>% 
  select(Transmitter, Time_minute, Noise.ts)

df <- left_join(df_temp, df_noise) # df now combines detection data with the noise measurements at the time the receiver registered the transmitted signal (of its built-in transmitter)
rm(df_temp, df_noise, df.list, rectrans, ev)

#### Save file ###
write.csv(df, "data/interim/df_noise.csv", row.names = FALSE) # save the file 
