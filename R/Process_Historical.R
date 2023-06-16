# Processing historical CMIP6 simulations
# Simulations match past2k simulations except in simulation

rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(sf)

# Make list of files to read in
clim_pr_files <- list.files('CMIP_Reconstructions/Historical_Precipitation/')
clim_at_files <- list.files('CMIP_Reconstructions/Historical_SurfaceAT/')

# Storage
clim_pr <- list()
clim_at <- list()

# Read in files and store them in lists
for(i in 1:length(clim_at_files)){
  pr_file <- paste0('CMIP_Reconstructions/Historical_Precipitation/', clim_pr_files[i])
  clim_pr[[i]] <- nc_open(pr_file)
  
  at_file <- paste0('CMIP_Reconstructions/Historical_SurfaceAT/', clim_at_files[i])
  clim_at[[i]] <- nc_open(at_file)
}

# Storage
arr_pr <- list()
arr_at <- list()

# Reformatting data into more manageable arrays
for(i in 1:length(clim_at_files)){
  temp <- clim_pr[[i]]
  
  temp2 <- ncvar_get(temp, 'pr')
  
  lons <- ncvar_get(temp, 'lon')
  lats <- ncvar_get(temp, 'lat')
  times <- ncvar_get(temp, 'time')
  
  dimnames(temp2) <- list(lons, lats, times)
  
  arr_pr[[i]] <- temp2
  
  temp <- clim_at[[i]]
  
  temp2 <- ncvar_get(temp, 'tas')
  
  lons <- ncvar_get(temp, 'lon')
  lats <- ncvar_get(temp, 'lat')
  times <- ncvar_get(temp, 'time')
  
  dimnames(temp2) <- list(lons, lats, times)
  
  arr_at[[i]] <- temp2
  
  print(i)
}

# Removing unnecessary objects to clear up storage
rm(clim_at, clim_pr)

# Finding the boundaries of our study region
test <- arr_at[[1]]

# lon, lat, time, temp
test2 <- melt(test)
colnames(test2) <- c('LON', 'LAT', 'TIME', 'TEMP')

# Converting longitude
test2 <- test2 |>
  mutate(LON = if_else(LON > 180, -360 + LON, LON))

# Reduce the climate data by the maximum extents
# of the pollen data
# This is necessary because using the entire dataset
# was really computationally expensive
# since we will be finding pairwise distances
min_lon <- -98.11711
max_lon <- -82.59814
min_lat <- 41.50088
max_lat <- 50.17222

# Filter the climate reconstructions only within the bounds
# of the pollen data
test3 <- test2 |>
  filter(LON >= min_lon & LON <= max_lon) |>
  filter(LAT >= min_lat & LAT <= max_lat)

# Make vectors of all unique lats & lons
lons <- unique(test3$LON)
lats <- unique(test3$LAT)

# Finding indexes of the climate grid taht are within
# the bounds of the pollen data
# Will be used as indices for each of the climate files
dim1 <- dimnames(test)[1]
dim1 <- as.numeric(unlist(dim1))
dim1 <- if_else(dim1 > 180, -360 + dim1, dim1)
# Longitudes within the pollen bounds
ind1 <- which(dim1 %in% lons)

dim2 <- dimnames(test)[2]
dim2 <- as.numeric(unlist(dim2))
# Latitudes within the pollen bounds
ind2 <- which(dim2 %in% lats)

# Take our area of interest in the midwest
for(i in 1:length(clim_at_files)){
  temp <- arr_at[[i]]
  temp <- temp[ind1,ind2,]
  arr_at[[i]] <- temp
  
  temp <- arr_pr[[i]]
  temp <- temp[ind1,ind2,]
  arr_pr[[i]] <- temp
}

# Reformatting
melt_at <- melt(arr_at)
melt_pr <- melt(arr_pr)

# Remove unnecessary objects again
rm(arr_at, arr_pr)

# Join the temperature and precipitation data together
clim_historical <- melt_at |>
  full_join(melt_pr, by = c('Var1', 'Var2', 'Var3', 'L1'))

# Remove unnecessary objects one more time
rm(melt_at, melt_pr)

# Formatting
colnames(clim_historical) <- c('Longitude', 'Latitude', 'Time', 'Temperature', 'L1', 'Precipitation')

clim_historical <- clim_historical |>
  select(-L1)

# Formatting specific columns
# LATITUDE & LONGTIUDE in CRS EPSG 4326
clim_historical <- clim_historical |>
  mutate(Longitude = if_else(Longitude > 180, -360 + Longitude, Longitude), # Make longitude between -180 and 180 degrees
         Time = as.Date(Time, origin = c('1850-01-01')), # Reformatting time as the date
         Month = month(Time),
         Year = year(Time),
         Temperature = Temperature - 273.15, # convert temperature to Celsius
         Precipitation = Precipitation * 24 * 60 * 60 * 30) # convert precipitation to mm/month

# Keep only dates for which we have PRISM data
clim_historical <- clim_historical |>
  filter(Year >= 1895 & Year <= 1980)

# Map of minnesota, wisconsin, and michigan for plotting
states <- map_data('state') |>
  filter(region %in% c('minnesota', 'wisconsin', 'michigan'))

# Plot temperature over space for first time period
clim_historical |>
  filter(Year %in% min(Year)) |>
  ggplot(aes(x = Longitude, y = Latitude, color = Temperature)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  geom_point() +
  coord_sf(datum = st_crs(4326)) +
  facet_wrap(~Month) +
  scale_color_viridis_c(option = 'C') +
  theme_void() +
  theme(strip.text = element_text(size = 12))

# Last time period
clim_historical |>
  filter(Year %in% max(Year)) |>
  ggplot(aes(x = Longitude, y = Latitude, color = Temperature)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  geom_point() +
  coord_sf(datum = st_crs(4326)) +
  facet_wrap(~Month) +
  scale_color_viridis_c(option = 'C') +
  theme_void() +
  theme(strip.text = element_text(size = 12))

# Repeat for precipitation
clim_historical |>
  filter(Year %in% min(Year)) |>
  ggplot(aes(x = Longitude, y = Latitude, color = Precipitation)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  geom_point() +
  coord_sf(datum = st_crs(4326)) +
  facet_wrap(~Month) +
  scale_color_viridis_c(option = 'C') +
  theme_void() +
  theme(strip.text = element_text(size = 12))

clim_historical |>
  filter(Year %in% max(Year)) |>
  ggplot(aes(x = Longitude, y = Latitude, color = Precipitation)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  geom_point() +
  coord_sf(datum = st_crs(4326)) +
  facet_wrap(~Month) +
  scale_color_viridis_c(option = 'C') +
  theme_void() +
  theme(strip.text = element_text(size = 12))

# Save
save(clim_historical, file = 'CMIP_Reconstructions/processed_historical.RData')