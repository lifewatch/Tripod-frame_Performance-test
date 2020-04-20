
###############################
# Model detection probability #
###############################

# Jolien Goosens - Flanders Marine Institute (VLIZ) / Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2


#### Load packages ####
library(plyr)
library(dplyr)
library(lubridate)
library(rsq)

#### Read data ####
df <- read.csv("data/interim/df_noise.csv", stringsAsFactors = F)

#### Format preparation ####
MyStd <- function(x) {  (x - mean(x))  /  sd(x) }

df <- df %>% 
  mutate(Time_minute = parse_date_time(Time_minute, orders = "ymd HMS"),
         Noise.ts.st = MyStd(Noise.ts) ) %>% # Standardize interpolated noise measurements
  filter(Distance_class != "0")

#### Models  ####
# No interactions
mylogit1_all <- glm(Detection ~ Distance_class + Year + Noise.ts.st, 
                    family = "binomial", 
                    data = df)

# Two-way interactions
mylogit2_all <- glm(Detection ~ Distance_class + Year + Noise.ts.st +
                      Distance_class:Year + Year:Noise.ts.st + Distance_class:Noise.ts.st, 
                    family = "binomial", 
                    data = df)

mylogit2_bis1_all <-  glm(Detection ~ Distance_class + Year + Noise.ts.st +
                        Distance_class:Year, 
                        family = "binomial", 
                        data = df)

mylogit2_bis2_all <- glm(Detection ~ Distance_class + Year + Noise.ts.st +
                      Year:Noise.ts.st, 
                    family = "binomial", 
                    data = df)

mylogit2_bis3_all <- glm(Detection ~ Distance_class + Year + Noise.ts.st +
                      Distance_class:Noise.ts.st, 
                    family = "binomial", 
                    data = df)

mylogit2_bis4_all <-  glm(Detection ~ Distance_class + Year + Noise.ts.st +
                        Distance_class:Year + Year:Noise.ts.st, 
                      family = "binomial", 
                      data = df)

mylogit2_bis5_all <- glm(Detection ~ Distance_class + Year + Noise.ts.st +
                       Year:Noise.ts.st + Distance_class:Noise.ts.st, 
                     family = "binomial", 
                     data = df)

mylogit2_bis6_all <- glm(Detection ~ Distance_class + Year + Noise.ts.st +
                       Distance_class:Year + Distance_class:Noise.ts.st, 
                     family = "binomial", 
                     data = df)

# Three-way interaction
mylogit3_all <-  glm(Detection ~ Distance_class * Year * Noise.ts.st, 
                     family = "binomial", 
                     data = df)

#### Model summaries ####
summary(mylogit1_all)
summary(mylogit2_all)
summary(mylogit3_all)

#### Compare models: drop ####
drop1(mylogit1_all, test ="Chi")
drop1(mylogit2_all, test ="Chi")
drop1(mylogit3_all, test ="Chi")

#### Compare models: table of statistics ####
list_stats <- lapply(ls(pattern = "mylogit.+_all"), function(x){
  mylogit <- get(x)
  tbl_stat <- data.frame(
    model_name = as.character(x),
    model_formula = as.character(formula(mylogit))[3],
    AIC = AIC(mylogit),
    BIC = BIC(mylogit),
    logLik = logLik(mylogit))
})
tbl_stats <- do.call(rbind, list_stats)

attach(df)
tbl_stats$rsq <- NA
list_rsq <- lapply(1:nrow(tbl_stats), function(x){
  rsq <- rsq(get(as.character(tbl_stats[x,1])))
})
tbl_stats$rsq <- do.call(rbind, list_rsq)
tbl_stats$rsq <- tbl_stats$rsq*100
detach(df)

# Save table
write.csv(tbl_stats, "data/processed/Supportinginformation_Table_Models.csv", row.names = F)

# Full model is best model: continue with three-way interaction

#### Model predictions: all models ####
# Calculate predicted values
list_pred <- lapply(ls(pattern = "mylogit.+_all"), function(x){
  mylogit <- get(x)
  pred <- predict(mylogit, df, type="response")
})
names(list_pred) <- paste("pred", gsub("mylogit", "", ls(pattern = "mylogit.+_all")), sep = "")
df <- cbind(df, as.data.frame(do.call(cbind, list_pred)))

# Calculate difference between predicted and real values
list_diff <- lapply(ls(pattern = "mylogit.+_all"), function(x){
  mylogit <- get(x)
  pred <- predict(mylogit, df, type="response")
  diff <- df$Detection - pred
  return(diff)
})
names(list_diff) <- paste("diff", gsub("mylogit", "", ls(pattern = "mylogit.+_all")), sep = "")
df <- cbind(df, as.data.frame(do.call(cbind, list_diff)))

#### Model predictions: final model for plotting ####
# Construct new data frame 
newdata <- expand.grid(Year = unique(df$Year),
                       Distance_class = unique(df$Distance_class),
                       Noise = c(100:1000))
newdata$Noise.ts.st <- (newdata$Noise - mean(df$Noise.ts))  /  sd(df$Noise.ts)
# predict values
newdata$pred <- predict(mylogit3_all, newdata, type="response")

# Construct factors for easier visualization
newdata <- newdata %>% 
  mutate(Year = ifelse(Year == "2017", "Stone mooring", "Tripod frame"),
         Distance_class = ifelse(Distance_class == "[120, 180[", "120 - 180 m",
                                 ifelse(Distance_class == "[250, 270[", "250 - 270 m",
                                        "290 - 310 m"))) %>% 
  mutate(Noise_levels = ifelse(100 <= Noise & Noise < 300, "100 - 300",
                               ifelse(300 <= Noise & Noise < 500, "300 - 500",
                                      ifelse(500 <= Noise & Noise < 1000, "500 - 1000",
                                             NA)))) %>% 
  filter(!is.na(Noise_levels))

# Group predictions and calculate min, max, mean, median prediction values
newdata <- newdata %>% 
  group_by(Year, Distance_class, Noise_levels) %>% 
  summarise(pred_min = min(pred),
            pred_mean = mean(pred),
            pred_max = max(pred),
            pred_med = median(pred)) %>% 
  as.data.frame()

#### Save predictions ####
write.csv(newdata, "data/interim/df_pred.csv", row.names = F)

