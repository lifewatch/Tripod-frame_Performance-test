
#############################
# Get data and make folders #
#############################

# Jolien Goosens - Flanders Marine Institute (VLIZ) / Marine Biology Research Group, Ghent University (Marbiol)
# R version 3.6.2


#### Function to create directory ####
mkdirs <- function(fp) {
  if(!dir.exists(fp)) {
    dir.create(fp)
  }
} 

#### Create directories ####
mkdirs("data")
mkdirs("data/external")
mkdirs("data/interim")
mkdirs("data/processed")
mkdirs("data/raw")
mkdirs("reports")
mkdirs("reports/figures")

#### Get detection data, tilt and noise measurements and receiver metadata #####
# DOI: 
# Download data and save in data/raw


#### Mapping data ####
# Shape files originate from MarineRegions.org and EMODnet.

