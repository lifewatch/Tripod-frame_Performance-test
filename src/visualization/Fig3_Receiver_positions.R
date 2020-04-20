
###############################
# Plot positions of receivers #
###############################

# Jolien Goosens - Flanders Marine Institute (VLIZ) / Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2


#### Load packages ####
library(ggplot2)
library(rgdal)
library(raster)
library(dplyr)
library(ggsn)
library(RColorBrewer)
library(lubridate)
library(ggpubr)

#### Read data ####
# Read Belwind turbine locations
#belwind <- readOGR("data/external/mapping/Belwind", layer = "belwind_turbines_as_built")

# Read receiver locations
set <- read.csv("data/raw/stations.csv", stringsAsFactors = F) # station metadata

#### Organise data ####
# Belwind turbine locations
# belwind <- spTransform(belwind, CRS("+proj=longlat +datum=WGS84"))
# belwind <-   as.data.frame(belwind)
# belwind <- belwind %>% dplyr::select(name,
#                                      long = coords.x1,
#                                      lat = coords.x2)
# 
# belwind_pcad <- belwind %>% 
#   filter(name == "F05")

# Position of turbine F05: 51.7027°N, 2.808958°E
belwind_pcad <- data.frame(name = "F05",
                           long = 2.808958,
                           lat = 51.7027)

# Receiver locations
set$setup <- factor(ifelse(set$Year == 2017, "Stone", "Frame"), levels = c("Stone", "Frame"))

#### Distances around turbines without background ####
# Set theme
theme_map <- theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))


p2 <- ggplot() +
  coord_map(xlim = c(2.806, 2.812), ylim = c(51.701, 51.7045)) +
  theme_map +
  theme(legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0.1,0,0.1,0),"cm")) +
  geom_point(data = filter(set, setup == "Stone"), aes(Longitude, Latitude), size = 3) +
  geom_point(data = filter(belwind_pcad, name == "F05"), 
             aes(long,lat), size = 12, colour = "gray50") +
  geom_segment(data = filter(set, setup == "Stone" & Position !=2), 
               aes(x = 2.808952, y = 51.70405 - 0.00015,
                   xend = Longitude, yend = Latitude + 0.00013),
               arrow = arrow(length = unit(0.1,"cm")),
               size = 0.4) +
  
  #  annotate("text", x = 2.806+(2.812-2.806)*0.09, y = 51.701 +(51.7045-51.701)*0.92, label = "C", size = 9, fontface = 2) +
  
  geom_text(aes(x = 2.8074, y = 51.7039, label = "120 - 180 m"), size = 3) +
  geom_text(aes(x = 2.8106, y = 51.7039, label = "120 - 180 m"), size = 3) +
  geom_text(aes(x = 2.8069, y = 51.7028, label = "250 -\n270 m"), size = 3) +
  geom_text(aes(x = 2.8110, y = 51.7028, label = "250 -\n270 m"), size = 3) +
  geom_text(aes(x = 2.8100, y = 51.7017, label = "290 - 310 m"), size = 3)

# Save as width 315 height 350
ggsave(filename = "reports/figures/Fig3_Receiver_positions.png", plot = p2, scale = 1, dpi = 600, width = 8, height = 8, units = "cm")
