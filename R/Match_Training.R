## Find matching locations between PRISM and historical data

rm(list = ls())

library(dplyr)
library(sp)
library(rgeos)
library(ggplot2)

load('Climate_Data/prism_clim.RData')
load('CMIP_Reconstructions/processed_historical.RData')

prism_clim <- prism_clim |>
  rename(Year = year,
         Month = month) |>
  mutate(Month = as.numeric(Month),
         Year = as.numeric(Year),
         loc = paste0(Longitude,'_',Latitude))

# Lat lon pairs in the historical data
latlong_pairs <- clim_historical |>
  filter(Time == unique(Time)[1]) |>
  select(Longitude:Latitude)

# Lat lon pairs in the prism data
prism_latlong <- prism_clim |>
  filter(var == unique(var)[1]) |>
  select(Longitude:Latitude)

# Format as spatial data
coordinates(latlong_pairs) <- ~Longitude+Latitude
coordinates(prism_latlong) <- ~Longitude+Latitude

# Find distance between all points
d <- gDistance(spgeom1 = latlong_pairs, spgeom2 = prism_latlong, byid = T)
# Find closest prism data point to each historical data point
mins <- apply(d, 2, which.min)

spat_historical <- clim_historical |>
  filter(Time == unique(Time)[1]) |>
  mutate(loc = paste0(Latitude,'_',Longitude),
         match = mins)

spat_prism <- prism_clim |>
  filter(var == unique(var)[1])

full_spat_historical <- clim_historical |>
  mutate(loc = paste0(Latitude,'_',Longitude))

spat_map <- spat_historical |>
  select(loc, match)

full_data <- matrix(, nrow = nrow(full_spat_historical),
                    ncol = 11)

for(i in restart:nrow(full_spat_historical)){
  loc <- full_spat_historical$loc[i]
  match <- spat_map$match[which(spat_map$loc == loc)]
  year <- as.numeric(full_spat_historical$Year[i])
  month <- as.numeric(full_spat_historical$Month[i])
  full_data[i,1] <- match # Match between datasets
  full_data[i,2] <- full_spat_historical[i,1] # lon of cmip
  full_data[i,3] <- full_spat_historical[i,2] # lat of cmip
  full_data[i,4] <- full_spat_historical[i,4] # cmip temperature
  full_data[i,5] <- full_spat_historical[i,5] # cmip precipitation
  full_data[i,6] <- year # year
  full_data[i,7] <- month # month
  
  prism_loc <- spat_prism$loc[match]
  temp <- prism_clim |> filter(loc == prism_loc) |> filter(Year == year) |> filter(Month == month)
  full_data[i,8] <- temp$Latitude # lat of prism
  full_data[i,9] <- temp$Longitude # lon of prism
  full_data[i,10] <- temp$PPT # prism precipitaton
  full_data[i,11] <- temp$T # prism temperature
}

full_data <- as.data.frame(full_data)
colnames(full_data) <- c('Match', 'CMIP_Longitude', 'CMIP_Latitude',
                         'CMIP_Temperature', 'CMIP_Precipitation',
                         'Year', 'Month', 'PRISM_Latitude', 'PRISM_Longitude',
                         'PRISM_Precipitation', 'PRISM_Temperature')

full_data |>
  ggplot() +
  geom_point(aes(x = CMIP_Temperature, y = PRISM_Temperature)) +
  geom_smooth(aes(x = CMIP_Temperature, y = PRISM_Temperature), method = 'lm') +
  geom_abline(slope = 1, color = 'pink')

full_data |>
  ggplot() +
  geom_point(aes(x = CMIP_Temperature, y = PRISM_Temperature, color = as.factor(Month))) +
  geom_smooth(aes(x = CMIP_Temperature, y = PRISM_Temperature), method = 'lm') +
  geom_abline(slope = 1, color = 'pink')

full_data |>
  ggplot() +
  geom_point(aes(x = CMIP_Precipitation, y = PRISM_Precipitation)) +
  geom_smooth(aes(x = CMIP_Precipitation, y = PRISM_Precipitation), method = 'lm') +
  geom_abline(slope = 1, color = 'pink')

full_data |>
  ggplot() +
  geom_point(aes(x = CMIP_Precipitation, y = PRISM_Precipitation, color = as.factor(Month))) +
  geom_smooth(aes(x = CMIP_Precipitation, y = PRISM_Precipitation), method = 'lm') +
  geom_abline(slope = 1, color = 'pink')

save(full_data, file = 'Climate_Data/prism_historical.RData')
