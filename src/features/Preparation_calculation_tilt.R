
###########################
# Investigate tilt values #
###########################

# Jolien Goosens - Flanders Marine Institute (VLIZ) / Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2


#### Load packages ####
library(plyr)
library(dplyr)
library(lubridate)
library(caTools)

#### Read data ####
ev <- read.csv("data/raw/tilt_noise.csv", stringsAsFactors = F)

#### Data prep ####
ev <- ev %>% 
  mutate(
    Time = parse_date_time(Time, orders = "ymd HMS", tz= "UTC"),
    Year = as.factor(Year),
    Turbine = as.factor(Turbine),
    recyear = paste(Year,Receiver, sep = "_"), # create new variable for easy lapply
    Deploy = ifelse(Year == 2017, "Stone mooring", "Tripod frame"))


#### Calculate autocorrelation ####
evauto <- lapply(unique(ev$recyear), function(x) {
  ev <- ev %>% filter(recyear == x)
  auto <- acf(ev$Tilt, lag.max = 150, plot = F)
  #plot(auto, main = x) # visual inspection of ACF of every receiver
  auto <- data.frame(auto = auto$acf)
  auto$recyear <- unique(ev$recyear)
  auto$Year <- unique(ev$Year)
  auto$Receiver <- unique(ev$Receiver)
  
  return(auto)
}) 
evauto <- ldply(evauto)
evauto$lag <- rep(c(0:150), times = length(unique(ev$recyear)))

evauto <- evauto %>% 
  group_by(Year, lag) %>% 
  summarise(autosum = mean(auto),
            automed = median(auto)) %>% 
  right_join(evauto) %>% 
  as.data.frame() %>% 
  mutate(Deploy = ifelse(Year == 2017, "Stone mooring", "Tripod frame"))

evauto %>% 
  filter(Year == "2017") %>% 
  filter(15 < lag & lag < 30) %>% 
  group_by(Receiver) %>% 
  filter(auto == min(auto)) %>% 
  distinct(lag) # 2017: auto at 18,19,20 -> take window of 21 points (= 3.5 hours)

#### Running standard deviation ####
evlist <- lapply(unique(ev$recyear), function(x) {
  ev <- ev %>% filter(recyear == x)
  ev$evsd <- runsd(x = ev$Tilt, k = 21, endrule = "NA")
  return(ev)
})

ev <- ldply(evlist)
rm(evlist)

#### Summarise: mean and median values ####
ev <- ev %>% 
  group_by(recyear) %>% 
  mutate(X = 1:length(recyear)) %>% 
  as.data.frame()

evsum <- ev %>% 
  filter(recyear != "2017_VR2AR-546892") %>%  # issue with this receiver: was probably stuck
  filter(Tilt < 90) %>% # remove extreme values for calculation of mean
  group_by(X, Year, Deploy) %>% 
  summarise(Time_min = min(Time), # First check difference between Time for each value of X (see evtest)
            tilt_mean = mean(Tilt, na.rm=T),
            tilt_median = median(Tilt, na.rm=T),
            sd_median = median(evsd, na.rm=T),
            sd_mean = mean(evsd, na.rm=T)) %>% 
  as.data.frame()


#### Calculate summary statistics ####
ev %>% 
  group_by(Deploy) %>% 
  summarise(min_tilt= min(Tilt),
            max_tilt = max(Tilt),
            mean_tilt = mean(Tilt),
            median_tilt = median(Tilt),
            sd_tilt = sd(Tilt),
            min_sdtilt= min(evsd, na.rm =T),
            max_sdtilt = max(evsd, na.rm =T),
            mean_sdtilt = mean(evsd, na.rm =T),
            median_sdtilt = median(evsd, na.rm =T))

difftilt <- ev %>% group_by(recyear) %>% 
  summarise(difftilt = max(Tilt) - min(Tilt))

#### Table of tilt values per receiver ####
ev_table <- ev %>% 
  group_by(Turbine, Receiver,Deploy) %>% 

  summarise(min_tilt= min(Tilt),
            median_tilt = median(Tilt),
            max_tilt = max(Tilt),
            min_sdtilt= min(evsd, na.rm =T),
            median_sdtilt = median(evsd, na.rm =T),
            max_sdtilt = max(evsd, na.rm =T)) %>% 
  as.data.frame() %>% 
  arrange(Deploy)

#### Save data and table ####
write.csv(ev_table, "data/processed/Table1_Tilt.csv", row.names = F)
write.csv(evauto, "data/interim/evauto.csv", row.names = F)
write.csv(evsum, "data/interim/evsum.csv", row.names = F)
