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

# Make a dataframe that includes all the unique locations in
# the prism dataset along with the index of the corresponding
# cmip point
spat_prism <- prism_clim |>
  filter(var == unique(var)[1]) |>
  mutate(loc = paste0(Latitude,'_',Longitude),
         match = mins) |>
  select(Latitude, Longitude, loc, match)

# Make a dataframe with all prism data points (for all months and years)
full_spat_prism <- prism_clim |>
  mutate(loc = paste0(Latitude,'_',Longitude))

# Add the corresponding cmip point to the full prism dataset
full_spat_prism <- full_spat_prism |>
  full_join(spat_prism, by = c('Longitude', 'Latitude', 'loc'))

# Make a dataframe that includes all the unique locations of cmip
# data and make rownumbers into a column to match with prism
spat_historical <- clim_historical |>
  filter(Time == unique(Time)[1]) |>
  mutate(loc = paste0(Latitude,'_',Longitude)) |>
  rowid_to_column(var = 'match') |>
  select(Latitude, Longitude, loc, match) |>
  filter(match %in% unique(spat_prism$match))

# Make a dataframe of all cmip data points
full_spat_historical <- clim_historical |>
  mutate(loc = paste0(Latitude,'_',Longitude))

# Add the index to the full data frame
full_spat_historical <- full_spat_historical |>
  full_join(spat_historical, by = c('Longitude', 'Latitude', 'loc'))

# Clean up to help our our RAM
rm(clim_historical, d, prism_clim, spat_historical, spat_prism, mins)

# Prepare prism data for joining with cmip
match_clim <- full_spat_prism |>
  mutate(month = as.numeric(month),
         year = as.numeric(year)) |>
  rename(Year = year,
         Month = month)

# Spatially match each prism point to a cmip point
match_clim <- match_clim |>
  left_join(full_spat_historical, by = c('Month', 'Year', 'match'))

# Save output
save(match_clim, file = 'Climate_Data/full_matched_historical.RData')

# Formatting
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

# Clean up to help our RAM
rm(full_spat_historical, full_spat_prism)

# Make a dataframe of the covariates of our temperature model
predict_temp <- match_clim |>
  select(CMIP_Temperature, Month, PRISM_Latitude)

# Make predictions for all locations with prism data
predictions_temp <- predict(fit_temp, predict_temp)

# Make a dataframe of the covariates of our precipitation model
predict_precip <- match_clim |>
  select(CMIP_Precipitation, Month, PRISM_Latitude, PRISM_Longitude)

# Make predictions for all locations with prism data
predictions_precip <- predict(fit_precip, predict_precip)

# Save predictions
save(predictions_temp, predictions_precip, file = 'CMIP_Reconstructions/downscaled_historical.RData')

# Add to our dataframe of cmip and prism temperature and precipitation
match_clim$Predicted_Temperature <- predictions_temp
match_clim$Predicted_Precipitation <- predictions_precip

# data to have states in a figure
states <- map_data('state') |> filter(region %in% c('michigan', 'minnesota', 'wisconsin'))

# Plot predictions in one year over space
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

# plot actual vs predicted climate for one year
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

# Plot the difference in climate between actual and predicted
# over space for one year
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
