
######################################
# Plot tilt and tilt autocorrelation #
######################################

# Jolien Goosens - Flanders Marine Institute (VLIZ) / Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2


#### Load packages ####
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(lemon)
library(ggpubr)

#### Read data ####
evauto <- read.csv("data/interim/evauto.csv", stringsAsFactors = F)
evsum <- read.csv("data/interim/evsum", stringsAsFactors = F)

#### Plot ####
# format time variable
evsum <- evsum %>% 
  mutate(Time_min = parse_date_time(Time_min, orders = "ymd HMS"))

# set theme
theme_frame <- theme(panel.background = element_rect(fill = "white"),
                     axis.line = element_line(colour = "black", size = 0.5),
                     axis.title.y = element_text(size = 14, family = "NimbusSan"),
                     axis.text = element_text(size = 11, family = "NimbusSan"),
                     panel.spacing = unit(0.8, "lines"))

p1 <- ggplot(evsum) +
  geom_path(aes(x= Time_min, y = tilt_median), size = 0.3) +
  theme_frame +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14, family = "NimbusSan"),
        plot.margin=unit(c(0.2,0.5,0,0.5),"cm")) +
  scale_x_datetime(expand = c(0,0), date_breaks = "1 month", date_labels = c("%b", "%b (%Y)", "%b", "%b")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,42)) +
  facet_rep_wrap(~Deploy, scales = "free_x", ncol = 2) +
  labs(x = "", y = "\nTilt (°)")

p2 <- ggplot(filter(evauto, recyear != "2017_VR2AR-546892")) +
  #  geom_path(aes(x = lag, y = auto, group = recyear), colour = "gray76") +
  geom_path(aes(x = lag, y = autosum, group = recyear)) +
  facet_rep_wrap(~Deploy, nrow = 1) +
  theme_frame +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"cm")) +
  scale_x_continuous(breaks = c(36, 72, 108, 144), 
                     labels = c(6, 12, 18, "24 h"), expand = c(0,0)) +
  scale_y_continuous(limits= c(0,1), expand = c(0,0), labels = c("0", "0.25", "0.50", "0.75", "1")) +
  labs(x = "", y= "Tilt\nAutocorrelation" )

p3 <- ggplot(evsum) +
  geom_path(aes(x= Time_min, y = sd_median), size = 0.3) +
  facet_rep_wrap(~Deploy, scales = "free_x", ncol = 2) +
  theme_frame + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"cm")) +
  scale_x_datetime(expand = c(0,0), date_breaks = "1 month", date_labels = c("%b", "%b (%Y)", "%b", "%b")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,12.5), breaks = c(0,5,10)) +
  labs(x = "", y = "Tilt (°)\nRunning SD")

ptot <- ggarrange(p1, p2, p3, ncol = 1, heights = c(1.3, 1, 1), align = "v" , labels = c("A", "B", "C"), hjust = -0.1) 

ggsave(filename = "reports/figures/Fig4_Tilt.png", plot = ptot, scale = 1, dpi = 600, width = 30, height = 15, units = "cm")

# Save: width = 1000, height = 500

# add labels within ggarrange: labels = c("A", "B", "C"), hjust = -5.5, vjust = c(3.5, 2, 2)
