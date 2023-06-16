## Downscaling past2k simulations
## THIS SCRIPT IS NOT FINISHED

rm(list = ls())

library(dplyr)
library(sp)
library(rgeos)
library(tibble)

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

# Spatailly match each pollen point to a climate point
match_clim <- match_clim |>
  left_join(full_spat_climate, by = c('Year', 'match'))

# Formatting
match_clim <- match_clim |>
  dplyr::rename(Pollen_Longitude = Longitude.x,
         Pollen_Latitude = Latitude.x,
         CMIP_Longitude = Longitude.y,
         CMIP_Latitude = Latitude.y) |>
  select(-loc_time, -loc.x, -match, -loc.y)

# Make a dataframe of the covariates of our temperature model
predict_temp <- match_clim |>
  select()