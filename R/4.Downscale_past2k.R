## Downscaling past2k simulations

rm(list = ls())

library(dplyr)
library(sp)
library(rgeos)
library(tibble)
library(ggplot2)
library(tidyr)
library(matrixStats)

load('CMIP_Reconstructions/processed.RData')
load('full_melt_UMW.RData')
load('Fit/latlon_fit.RData')

# Take only one year to get unique locations
# All locations have data for all time points
spat_climate <- clim |> filter(Year == 1900)
# make spatial data object
coordinates(spat_climate) <- ~Longitude+Latitude

# Use the pollen dataset to identify all the locations we need climate for
spat_pollen <- full_melt |> filter(time == 1900)
coordinates(spat_pollen) <- ~long+lat

# Find distance between all points
d <- gDistance(spgeom1 = spat_pollen, spgeom2 = spat_climate, byid = T)
# Find closest climate data point to each pollen data point
mins <- apply(d, 2, which.min)
# result = 704 points corresponding to each grid cell of our pollen dataset

# Make a dataframe that includes all the unique locations in
# the pollen dataset along with the index of the corresponding
# climate point
spat_pollen <- full_melt |>
  filter(time == 1900) |>
  mutate(loc = paste0(lat,'_',long),
         match = mins) |>
  select(lat, long, loc, match)

# Make a dataframe with all pollen data points (for all months and years)
full_spat_pollen <- full_melt |>
  mutate(loc = paste0(lat,'_',long))

# Add the corresponding climate point to the full prism dataset
full_spat_pollen <- full_spat_pollen |>
  full_join(spat_pollen, by = c('long', 'lat', 'loc'))

# Make a dataframe that inlcudes all the unique locations of climate
# data and make row numbers into a column to match with pollen data
spat_climate <- clim |>
  filter(Time == unique(Time)[1]) |>
  mutate(loc = paste0(Latitude,'_',Longitude)) |>
  rowid_to_column(var = 'match') |>
  select(Latitude, Longitude, loc, match)

# Make a dataframe of all the climate data points
full_spat_climate <- clim |>
  mutate(loc = paste0(Latitude,'_',Longitude))

wide_temp <- full_spat_climate |>
  select(-Precipitation) |>
  pivot_wider(names_from = Month, 
              values_from = Temperature,
              id_cols = c('Longitude':'Latitude',
                          'Year':'loc')) |>
  dplyr::rename(Temperature_1 = `1`,
                Temperature_2 = `2`,
                Temperature_3 = `3`,
                Temperature_4 = `4`,
                Temperature_5 = `5`,
                Temperature_6 = `6`,
                Temperature_7 = `7`,
                Temperature_8 = `8`,
                Temperature_9 = `9`,
                Temperature_10 = `10`,
                Temperature_11 = `11`,
                Temperature_12 = `12`)

wide_precip <- full_spat_climate |>
  select(-Temperature) |>
  pivot_wider(names_from = Month,
              values_from = Precipitation,
              id_cols = c('Longitude':'Latitude',
                          'Year':'loc')) |>
  dplyr::rename(Precipitation_1 = `1`,
                Precipitation_2 = `2`,
                Precipitation_3 = `3`,
                Precipitation_4 = `4`,
                Precipitation_5 = `5`,
                Precipitation_6 = `6`,
                Precipitation_7 = `7`,
                Precipitation_8 = `8`,
                Precipitation_9 = `9`,
                Precipitation_10 = `10`,
                Precipitation_11 = `11`,
                Precipitation_12 = `12`)

full_spat_climate <- wide_temp |>
  full_join(wide_precip, by = c('Longitude', 'Latitude', 'Year', 'loc'))

# Add the index to the full data frame
full_spat_climate <- full_spat_climate |>
  full_join(spat_climate, by = c('Longitude', 'Latitude', 'loc'))

# Prepare pollen data for joining with climate
match_clim <- full_spat_pollen |>
  dplyr::rename(Year = time,
         Longitude = long,
         Latitude = lat)

# Spatially match each pollen point to a climate point
match_clim <- match_clim |>
  left_join(full_spat_climate, by = c('Year', 'match'))

# Formatting
match_clim <- match_clim |>
  dplyr::rename(Pollen_Longitude = Longitude.x,
         Pollen_Latitude = Latitude.x,
         CMIP_Longitude = Longitude.y,
         CMIP_Latitude = Latitude.y) |>
  select(-loc_time, -loc.x, -match, -loc.y)

# Reformat data frame to have monthly climate in "long" format
match_clim_long <- match_clim |>
  pivot_longer(Temperature_1:Precipitation_12) |>
  mutate(var = substr(name, 1, 1),
         Month = as.numeric(sub('.*_','',name))) |>
  select(-name) |>
  pivot_wider(names_from = var, values_from = value) |>
  dplyr::rename(CMIP_Temperature = T,
                CMIP_Precipitation = P,
                # This is something stupid I did in the model
                # and since you have to have the same variable
                # names to run the model, I'm just making the change
                # here
                PRISM_Latitude = Pollen_Latitude,
                PRISM_Longitude = Pollen_Longitude)

# Make datafarme of the covariates of our temperature model
predict_temp <- match_clim_long |>
  select(CMIP_Temperature, Month, PRISM_Latitude)

# Make predictions for all locations with pollen data
predictions_temp <- predict(fit_temp, predict_temp)

# Make a dataframe of the covariates of our precipitation model
predict_precip <- match_clim_long |>
  select(CMIP_Precipitation, Month, PRISM_Latitude, PRISM_Longitude)

# Make predictions for all locations with pollen data
predictions_precip <- predict(fit_precip, predict_precip)

# Save predictions
save(predictions_temp, predictions_precip, file = 'CMIP_Reconstructions/downscaled_past2k.RData')

# Insert into dataframe with pollen and climate information
match_clim_long$Downscale_Temperature <- predictions_temp
match_clim_long$Downscale_Precipitation <- predictions_precip

# Pivot climate portion of dataframe
downscale_climate_wide <- match_clim_long |>
  select(-CMIP_Longitude, -CMIP_Latitude, -CMIP_Temperature, -CMIP_Precipitation) |>
  pivot_wider(names_from = 'Month',
              values_from = Downscale_Temperature:Downscale_Precipitation,
              id_cols = c('Year', 'PRISM_Longitude', 'PRISM_Latitude'))

# Match pivoted data with pollen
xydata <- match_clim_long |>
  filter(Month == 1) |>
  select(Year:PRISM_Latitude) |>
  full_join(downscale_climate_wide, by = c('Year', 'PRISM_Longitude', 'PRISM_Latitude')) |>
  filter(Year < 2000)

# Plotting to make sure everything looks correct
states <- map_data('state') |> filter(region == c('michigan', 'minnesota', 'wisconsin'))

xydata |>
  select(-(Downscale_Precipitation_1:Downscale_Precipitation_12)) |>
  pivot_longer(Downscale_Temperature_1:Downscale_Temperature_12, names_to = 'Variable', values_to = 'Value') |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = Value)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  geom_point() +
  facet_wrap(~Variable)

xydata |>
  select(-(Downscale_Temperature_1:Downscale_Temperature_12)) |>
  pivot_longer(Downscale_Precipitation_1:Downscale_Precipitation_12, names_to = 'Variable', values_to = 'Value') |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = Value)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  geom_point() +
  facet_wrap(~Variable)

# Subset for only monthly temperature  columns
summary_temp <- xydata |>
  select(Downscale_Temperature_1:Downscale_Temperature_12)

# Find mean and SD (we're more interested in these summary statistics than monthly values)
summary_temp$Mean <- rowMeans(summary_temp)
summary_temp$SD <- rowSds(as.matrix(summary_temp[,1:12]))

# Do the same for precipitation
summary_precip <- xydata |>
  select(Downscale_Precipitation_1:Downscale_Precipitation_12)

summary_precip$Mean <- rowMeans(summary_precip)
summary_precip$SD <- rowSds(as.matrix(summary_precip[,1:12]))

# Put these summary statistics in the full data frame
xydata$Mean_Temperature <- summary_temp$Mean
xydata$SD_Temperature <- summary_temp$SD
xydata$Mean_Precipitation <- summary_precip$Mean
xydata$SD_Precipitation <- summary_precip$SD

# Save!
save(xydata, file = 'downscaled_processed_xydata.RData')
save(xydata, file = '~/Google Drive 2/longterm_feedbacks/FossilPollen/Data/downscaled_processed_xydata.RData')
