# Data downloaded using the following options from the PRISM website:
# 1. Historical Past
# 2. Precipitation & mean temperature (downloaded separately)
# 3. "Download All Historical Data (.bil)"

rm(list = ls())

library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(sf)
library(rgdal)

# List all files that we want to read in ('bil' files)
ppt_files <- list.files(path='Climate_Data/PRISM_ppt_stable_4kmM2_189501_198012_bil/',pattern=paste(".*_",".*\\.bil$", sep = ""),full.names=TRUE)
tmean_files <- list.files(path = 'Climate_Data/PRISM_tmean_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_','.*\\.bil$', sep = ''), full.names = TRUE)

# Stack the files
ppt <- raster::stack(ppt_files)
tmean <- raster::stack(tmean_files)

# Make points from rasters
ppt <- raster::rasterToPoints(ppt)
tmean <- raster::rasterToPoints(tmean)

# Save all points
save(ppt, tmean, file = 'Climate_Data/climate_matrix.RData')

# Re-load saved data
load('Climate_Data/climate_matrix.RData')

# Reformat
ppt <- as.data.frame(ppt)
tmean <- as.data.frame(tmean)

# Reproject
# Currently in GCS_North_American_1983 EPSG 4269
# Want to reproject to EPSG 4326

# Add coordinates
coordinates(ppt) <- ~x + y
coordinates(tmean) <- ~x + y

# Add current projection
# Currently in GCS_North_American_1983 EPSG 4269
proj4string(ppt) <- CRS('+init=epsg:4269')
proj4string(tmean) <- CRS('+init=epsg:4269')

# Reproject to EPSG 4326
ppt <- spTransform(ppt, CRS('+init=epsg:4326'))
tmean <- spTransform(tmean, CRS('+init=epsg:4326'))

# Map of region of interest
states <- map_data('state') |>
  filter(region %in% c('michigan', 'wisconsin', 'minnesota'))

# Check to make sure the projection looks correct
states |>
  ggplot() +
  geom_point(data = test, aes(x = x, y = y)) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = 'white')

# Boundaries of pollen data
min_lon <- -98.11711
max_lon <- -82.59814
min_lat <- 41.50088
max_lat <- 50.17222

# Store coordinates
ppt_coords <- as.data.frame(ppt@coords)
tmean_coords <- as.data.frame(tmean@coords)

# Change back to regular data frame
ppt <- as.data.frame(ppt)
tmean <- as.data.frame(tmean)

# Filter precipitation matrix to include only spatial area of interest
# Then reformat to make easier to average over years while keeping separate months
ppt_long <- ppt |>
  filter(Latitude >= min_lat & Latitude <= max_lat) |>
  filter(Longitude >= min_lon & Longitude <= max_lon) |>
  pivot_longer(colnames(ppt)[3]:colnames(ppt[ncol(ppt)]), 
               names_to = 'var', values_to = 'PPT')

# Separate year and month from the "var" variable
# Done in a separate step to avoid maxing out memory
ppt_long <- ppt_long |>  
  mutate(year = substr(var, 24, 27),
         month = substr(var, 28, 29))

# Find average per month over period 1895-1980
average_ppt <- ppt_long |>
  dplyr::select(-var) |>
  group_by(Latitude, Longitude, month) |>
  summarize(PPT = mean(PPT))

# Repeat for temperature
tmean_long <- tmean |>
  rename(Latitude = y,
         Longitude = x) |>
  filter(Latitude >= min_lat & Latitude <= max_lat) |>
  filter(Longitude >= min_lon & Longitude <= max_lon) |>
  pivot_longer(colnames(tmean)[3]:colnames(tmean[ncol(tmean)]),
               names_to = 'var', values_to = 'T')

tmean_long <- tmean_long |>
  mutate(year = substr(var, 26, 29),
         month = substr(var, 30, 31))

average_tmean <- tmean_long |>
  dplyr::select(-var) |>
  group_by(Latitude, Longitude, month) |>
  summarize(T = mean(T))

average_ppt$month <- as.factor(average_ppt$month)
average_tmean$month <- as.factor(average_tmean$month)

# Combine precipitation and temperature
average_clim <- cbind(average_ppt, average_tmean$T)
colnames(average_clim)[ncol(average_clim)] <- 'T'

# Map or study region
states <- map_data('state') |>
  filter(region %in% c('minnesota', 'wisconsin', 'michigan'))

# Plot precipitation
average_clim |>
  #filter(month %in% c('1', '4', '7', '10')) |>
  ggplot(aes(x = Longitude, y = Latitude, color = PPT)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~month) +
  scale_color_viridis_c(option = 'H') +
  theme_void()


### NEED TO CHANGE PROJECTION