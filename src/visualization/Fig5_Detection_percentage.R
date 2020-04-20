
#####################################
# Plot hourly detection percentages #
#####################################

# Jolien Goosens - Flanders Marine Institute (VLIZ) / Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2


#### Load packages ####
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(lemon)
library(ggpubr)

#### Get data ####
df_hoursum <- read.csv("data/interim/df_hoursum.csv", stringsAsFactors = F) # hourly summary data per turbine

#### Organise data ####
df_hoursum <- df_hoursum %>% 
  filter(Distance_class !=0) %>% 
  mutate(
    Turbine = factor(Turbine, levels = c("F05", "B08", "B10", "C09")),
    Distance_class = as.factor(Distance_class),
    Hour = parse_date_time(Hour, orders = "ymd HMS")
  )

#### Plot with detection percentage and noise over time ####
# Set language to English for month spelling
Sys.setlocale("LC_ALL", "English") 

# Plot detections vs year and distance class
theme_frame <- theme(panel.background = element_rect(fill = "white"),
                     axis.line = element_line(colour = "black", size = 0.5),
                     axis.title.y = element_text(size = 14, family = "NimbusSan"),
                     axis.text.y = element_text(size = 11, family = "NimbusSan"),
                     axis.text.x = element_blank(),
                     axis.line.y.right = element_blank(),
                     panel.spacing = unit(0.8, "lines"),
                     legend.position = "none")

p1a <- ggplot(filter(df_hoursum, Distance_class == "[120, 180[")) +
  geom_point(aes(x = Hour, y = mean_perc), size = 0.2, alpha = 0.8) +
  geom_line(aes(x = Hour, y = mean_perc), size = 0.1, alpha = 0.3) +
  theme_frame +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14, family = "NimbusSan"),
        axis.ticks.x = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.title.y.right = element_text(face = "bold", size = 14, family = "NimbusSan"),
        plot.margin=unit(c(0.5,0.5,0,0.5), "cm")) +
  facet_grid(~Turbine, scales = "free_x") +
  scale_y_continuous(expand = c(0,0),breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 25, 50, 75, 100),
                     sec.axis = dup_axis(name = "120 - 180 m")) +
  labs(x = "", y = "Detection\npercentage (%)")

p1b <- ggplot(filter(df_hoursum, Distance_class == "[250, 270[")) +
  geom_point(aes(x = Hour, y = mean_perc), size = 0.2, alpha = 0.8) +
  geom_line(aes(x = Hour, y = mean_perc), size = 0.1, alpha = 0.3) +
  theme_frame +
  theme(strip.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_text(face = "bold", size = 14, family = "NimbusSan"),
        plot.margin=unit(c(0,0,0,0), "cm")) +
  facet_grid(~Turbine, scales = "free_x") +
  scale_y_continuous(expand = c(0,0),breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 25, 50, 75, 100),
                     sec.axis = dup_axis(name = "250 - 270 m")) +
  labs(x = "", y = "Detection\npercentage (%)")

p1c <- ggplot(filter(df_hoursum, Distance_class == "[290, 310[")) +
  geom_point(aes(x = Hour, y = mean_perc), size = 0.2, alpha = 0.8) +
  geom_line(aes(x = Hour, y = mean_perc), size = 0.1, alpha = 0.3) +
  theme_frame +
  theme(strip.background = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.title.y.right = element_text(face = "bold", size = 14, family = "NimbusSan"),
        plot.margin=unit(c(0,0,0,0), "cm")) +
  facet_grid(~Turbine, scales = "free_x") +
  scale_y_continuous(expand = c(0,0),breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 25, 50, 75, 100),
                     sec.axis = dup_axis(name = "290 - 310 m")) +
  labs(x = "", y = "\nHourly mean detection percentage (%)")

# Plot noise
date_vec <- c("2017-08-17", "2017-09-01", "2017-09-28", "2018-08-19", "2018-09-01", "2018-10-08")
date_vec <- parse_date_time(date_vec, orders = "ymd")

p3 <- ggplot(df_hoursum) +
  geom_point(aes(x = Hour, y = median_noisemedian), size = 0.1, alpha = 0.4) +
  geom_line(aes(x = Hour, y = median_noisemedian), size = 0.1, alpha = 0.3) +
  theme_frame +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size = 11, family = "NimbusSan", hjust = c(0, 0, 0.7), vjust = 0.1),
        plot.margin=unit(c(0.5,1,0.1,1), "cm")) +
  facet_grid(~Turbine, scales = "free_x") +
  scale_x_datetime(breaks = date_vec, date_labels = c("%d %b\n(%Y)", "%b\n", "%d %b\n")) +
  scale_y_continuous(breaks = seq(0, 800, 200)) +
  labs(x = "", y = "Noise\n(mV)")

ptot <- ggarrange(p1a, p1b, p1c, p3, ncol = 1, heights = c(1.6, 1.2, 1.2, 1), align = "v", 
                  labels = c("\nA", "", "", "B"))
ptot

ggsave(filename = "reports/figures/Fig5_Detection_percentages.png", plot = ptot, scale = 1, dpi = 600, width = 37, height = 15, units = "cm")

# Save: width = 1300, height = 700


