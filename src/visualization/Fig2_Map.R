
################################
# Create map of the study area #
################################

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
# Read windfarms polygons
wind <- readOGR("data/external/mapping/Emodnet_HA_WindFarms_20181120", 
                layer = "Emodnet_HA_WindFarms_pg_20181120") 
# Read EEZ
eezbel <- readOGR("data/external/mapping/eezbel", layer = "eez") # Belgium
eezned <- readOGR("data/external/mapping/eezned", layer = "eez") # Netherlands
eezuk <- readOGR("data/external/mapping/eezuk", layer = "eez") # UK

# Read shapefiles area
netherlands_coast <- readOGR("data/external/mapping/netherlands_coast", layer = "world_countries_coasts")
ns <- readOGR("data/external/mapping/nsea", layer = "iho")


#### Organise data ####
# Windfarm polygons
windfort <- fortify(wind)
winddata <- data.frame(wind)
windpol <- winddata %>% 
  mutate(id = as.character(c(0:205))) %>% 
  filter(NAME == "Gemini" | NAME == "Belwind I") %>% 
  left_join(windfort)

# Fortify shapefiles
eezbelfort <- fortify(eezbel)
netherlands_coastfort <- fortify(netherlands_coast)
nsfort <- fortify(ns)

#### Map Belwind + Gemini ####
# Set theme
theme_map <- theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))

p1 <- ggplot() +
  coord_map(xlim = c(2.1,6.5), ylim = c(51,54.5)) +
  theme_map +
  theme(plot.margin=unit(c(0.1,0,0.1,0),"cm")) +
  
  geom_polygon(data = nsfort, aes(long,lat, group = group), fill ="#a5e0dd") +
  geom_polygon(data = netherlands_coastfort, aes(long,lat, group = group), fill = "white") +
  
  geom_path(data = nsfort, aes(long,lat, group = group), size = 0.3) +
  geom_path(data = eezbelfort, aes(long,lat, group = group), size = 0.3) +
  geom_path(data = eezned, aes(long,lat, group = group), size = 0.3) +
  geom_path(data = eezuk, aes(long,lat, group = group), size = 0.3) +
  
  geom_path(data= windpol, aes(long,lat, group=group))+
  
  geom_vline(xintercept = seq(2, 6.5, 1), size = 0.5, colour = "gray20", alpha = 0.15) + 
  geom_hline(yintercept = seq(51, 54.5, 1), size = 0.5, colour = "gray20", alpha = 0.15) +
  
  geom_text(aes(x = c(seq(3, 6.5, 1) + 0.1), y = 54.38),
            label = c("3°E", "4°", "5°", "6°"),
            size=2.5, hjust = 0.1, vjust = 0, colour = "gray20") +
  geom_text(aes(x = 2.16, y = c(seq(52, 54.5, 1)+0.05)),
            label = c("52°", "53°", "54°N"),
            size=2.5, hjust = 0, vjust = 0, colour = "gray20") +

  geom_polygon(data= windpol, aes(long,lat, group=group), colour = "red", fill = NA, size = 0.8) + 
  
  annotate("text", x = 2.67, y = 51.365, label = "BPNS", size = 3) +
  annotate("text", x = 5.5, y = 53.6, label = "DPNS", size = 3) +
  
#  annotate("text", x = 2.1+(6.5-2.1)*0.09, y = 51 +(54.5-51)*0.93, label = "A", size = 9, fontface = 2) +
  
  scalebar(data = eezbelfort, model = "WGS84", dist = 50, st.size = 2.7,  anchor = c(x = 6.19, y = 51.2), st.dist = 0.12, height = 0.08, 
            transform = T, dist_unit = "km", border.size = 0.3)

# Save as width 315 height 350
ggsave(filename = "reports/figures/Fig2_Map.png", plot = p1, scale = 1, dpi = 600, width = 8, height = 10, units = "cm")



