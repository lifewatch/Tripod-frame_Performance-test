
################################
# Plot model output prediction #
################################

# Jolien Goosens - Flanders Marine Institute (VLIZ) / Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2


#### Load packages ####
library(plyr)
library(dplyr)
library(lubridate)
library(lemon)
library(ggplot2)

#### Read data ####
df_pred <- read.csv("data/interim/df_pred.csv", stringsAsFactors = F)

#### Data organization ####
df_pred <- df_pred %>% 
  mutate(Year = as.factor(Year),
         Distance_class = as.factor(Distance_class),
         Noise_levels = as.factor(Noise_levels))

#### Plot ####
# Theme
theme_frame <- theme(panel.background = element_rect(fill = "white"),
                     axis.line = element_line(colour = "black", size = 0.5),
                     axis.title.y = element_text(size = 14, family = "NimbusSan"),
                     axis.text = element_text(size = 11, family = "NimbusSan"),
                     panel.spacing = unit(2, "lines"))

# Plot
ptot <- ggplot(df_pred) +
  geom_pointrange(aes(x = Distance_class, 
                      y = pred_med, ymin= pred_min, ymax=pred_max, 
                      shape = Noise_levels), 
                  position = position_dodge(width = 0.5), fatten = 7) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     expand = c(0,0)) +
  facet_rep_wrap(~Year) +
  theme_frame +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14, family = "NimbusSan"),
        axis.text.x = element_text(hjust = 0.4),
        plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) +
  theme(legend.key = element_rect(fill = "white"),
        legend.position = c(0.64, 0.23),
        legend.background = element_rect(colour = "black"),
        legend.text = element_text(size = 11, family = "NimbusSan"),
        legend.title = element_text(size = 14, family = "NimbusSan", margin = margin(b =0.5)),
        legend.margin = margin(10,10,10,10)) +
  labs(x = "", y = "Probability of detection", shape = "Noise (mV)")
# Save: width = 700, height = 350

ggsave(filename = "reports/figures/Fig6_Model.png", plot = ptot, scale = 1, dpi = 600, width = 26, height = 13, units = "cm")
