## Downscaling historical MPI-ESM simulation

rm(list = ls())

library(dplyr)
library(sp)
library(rgeos)
library(tibble)
library(tidyr)
library(ggplot2)

# Load data and model fits
load('CMIP_Reconstructions/processed_historical.RData')
load('Climate_Data/prism_clim.RData')
load('Fit/latlon_fit.RData')

# Filter for one time period to get unique locations
spat_historical <- clim_historical |> filter(Time == unique(Time)[1])
# Make spatial data object
coordinates(spat_historical) <- ~Longitude+Latitude

# Repeat for prism climate
spat_prism <- prism_clim |> filter(var == unique(var)[1])
coordinates(spat_prism) <- ~Longitude+Latitude

# Find distance between all points
d <- gDistance(spgeom1 = spat_prism, spgeom2 = spat_historical, byid = T)
# Find the closest cmip point to each prism point
mins <- apply(d, 2, which.min)

spat_prism <- prism_clim |>
  filter(var == unique(var)[1]) |>
  mutate(loc = paste0(Latitude,'_',Longitude),
         match = mins) |>
  select(Latitude, Longitude, loc, match)

full_spat_prism <- prism_clim |>
  mutate(loc = paste0(Latitude,'_',Longitude))

full_spat_prism <- full_spat_prism |>
  full_join(spat_prism, by = c('Longitude', 'Latitude', 'loc'))

spat_historical <- clim_historical |>
  filter(Time == unique(Time)[1]) |>
  mutate(loc = paste0(Latitude,'_',Longitude)) |>
  rowid_to_column(var = 'match') |>
  select(Latitude, Longitude, loc, match) |>
  filter(match %in% unique(spat_prism$match))

full_spat_historical <- clim_historical |>
  mutate(loc = paste0(Latitude,'_',Longitude))

full_spat_historical <- full_spat_historical |>
  full_join(spat_historical, by = c('Longitude', 'Latitude', 'loc'))

rm(clim_historical, d, prism_clim, spat_historical, spat_prism, mins)

match_clim <- full_spat_prism |>
  mutate(month = as.numeric(month),
         year = as.numeric(year)) |>
  rename(Year = year,
         Month = month)

match_clim <- match_clim |>
  left_join(full_spat_historical, by = c('Month', 'Year', 'match'))

save(match_clim, file = 'Climate_Data/full_matched_historical.RData')

match_clim <- match_clim |>
  rename(PRISM_Longitude = Longitude.x,
         PRISM_Latitude = Latitude.x,
         PRISM_Precipitation = PPT,
         PRISM_Temperature = T,
         CMIP_Longitude = Longitude.y,
         CMIP_Latitude = Latitude.y,
         CMIP_Temperature = Temperature,
         CMIP_Precipitation = Precipitation) |>
  select(PRISM_Longitude, PRISM_Latitude,
         PRISM_Precipitation, PRISM_Temperature,
         CMIP_Longitude, CMIP_Latitude,
         CMIP_Temperature, CMIP_Precipitation,
         Year, Month)

rm(full_spat_historical, full_spat_prism)

predict_temp <- match_clim |>
  select(CMIP_Temperature, Month, PRISM_Latitude)

predictions_temp <- predict(fit_temp, predict_temp)

predict_precip <- match_clim |>
  select(CMIP_Precipitation, Month, PRISM_Latitude, PRISM_Longitude)

predictions_precip <- predict(fit_precip, predict_precip)

save(predictions_temp, predictions_precip, file = 'CMIP_Reconstructions/downscaled_historical.RData')

match_clim$Predicted_Temperature <- predictions_temp
match_clim$Predicted_Precipitation <- predictions_precip

states <- map_data('state') |> filter(region %in% c('michigan', 'minnesota', 'wisconsin'))

match_clim |>
  filter(Year == max(Year)) |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, fill = Predicted_Temperature)) +
  geom_raster() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  facet_wrap(~Month) +
  scale_fill_viridis_c(option = 'H', name = 'Predicted\nTemperature')

match_clim |>
  filter(Year == max(Year)) |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, fill = Predicted_Precipitation)) +
  geom_raster() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  facet_wrap(~Month) +
  scale_fill_viridis_c(option = 'G', name = 'Predicted\nPrecipitation')

match_clim |>
  filter(Year == max(Year)) |>
  ggplot(aes(x = PRISM_Temperature, y = Predicted_Temperature, color = as.factor(Month))) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_abline(slope = 1, color = 'black', linetype = 'dashed')

match_clim |>
  filter(Year == max(Year)) |>
  ggplot(aes(x = PRISM_Temperature, y = Predicted_Temperature, color = as.factor(Month))) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_abline(slope = 1, color = 'black', linetype = 'dashed') +
  facet_wrap(~Month, scales = 'free')

match_clim |>
  filter(Year == max(Year)) |>
  ggplot(aes(x = PRISM_Precipitation, y = Predicted_Precipitation, color = as.factor(Month))) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_abline(slope = 1, color = 'black', linetype = 'dashed') +
  facet_wrap(~Month, scales = 'free')

match_clim |>
  filter(Year == max(Year)) |>
  ggplot(aes(x = PRISM_Precipitation, y = Predicted_Precipitation, color = as.factor(Month))) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') +
  geom_abline(slope = 1, color = 'black', linetype = 'dashed')

match_clim |>
  filter(Year == max(Year)) |>
  mutate(difference = abs(PRISM_Temperature - Predicted_Temperature)) |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, fill = difference)) +
  geom_raster() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Month) +
  scale_fill_viridis_c(option = 'A', name = 'Difference\n(deg. C)')

match_clim |>
  filter(Year == min(Year)) |>
  mutate(difference = abs(PRISM_Precipitation - Predicted_Precipitation)) |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, fill = difference)) +
  geom_raster() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Month) +
  scale_fill_viridis_c(option = 'A', name = 'Difference\n(mm/month)')
