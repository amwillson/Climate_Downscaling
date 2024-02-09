## Find matching locations between PRISM and historical data

rm(list = ls())

# Load data from previous steps in workflow
load('Climate_Data/prism_clim.RData')
load('CMIP_Reconstructions/processed_historical.RData')

# Format data
prism_clim <- prism_clim |>
  dplyr::rename(Year = year,
                Month = month) |>
  dplyr::mutate(Month = as.numeric(Month),
                Year = as.numeric(Year),
                loc = paste0(Longitude,'_',Latitude))

# Lat lon pairs in the historical data
latlong_pairs <- clim_historical |>
  dplyr::filter(Time == unique(Time)[1]) |>
  dplyr::select(Longitude:Latitude)

# Lat lon pairs in the prism data
prism_latlong <- prism_clim |>
  dplyr::filter(var == unique(var)[1]) |>
  dplyr::select(Longitude:Latitude)

# Find distance between all points
# We already ensured that they have the same CRS in step 1
dists <- fields::rdist(latlong_pairs, prism_latlong)
# Find closest prism data point to each historical data point
# Should have length = nrow(latlong_pairs) because we're looking 
# for an answer for each historical point
mins <- apply(dists, 1, which.min)

# Make dataframe of each unique location for cmip
spat_historical <- clim_historical |>
  dplyr::filter(Time == unique(Time)[1]) |>
  dplyr::mutate(loc = paste0(Latitude,'_',Longitude),
                match = mins)

# Make dataframe of each unique location for prism
spat_prism <- prism_clim |>
  dplyr::filter(var == unique(var)[1])

# Make dataframe of all locations of cmip data
full_spat_historical <- clim_historical |>
  dplyr::mutate(loc = paste0(Latitude,'_',Longitude))

# Make dataframe of the location and the corresponding
# index to match cmip and prism dataframes
spat_map <- spat_historical |>
  dplyr::select(loc, match)

# Storage
full_data <- matrix(, nrow = nrow(full_spat_historical),
                    ncol = 11)

# Loop over all locations with cmip data and find
# corresponding prism data
for(i in 1:nrow(full_spat_historical)){
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
  temp <- prism_clim |> dplyr::filter(loc == prism_loc) |> dplyr::filter(Year == year) |> dplyr::filter(Month == month)
  full_data[i,8] <- temp$Latitude # lat of prism
  full_data[i,9] <- temp$Longitude # lon of prism
  full_data[i,10] <- temp$PPT # prism precipitaton
  full_data[i,11] <- temp$Tmean # prism temperature
  
  print(i/nrow(full_spat_historical))
}

# Formatting
full_data <- as.data.frame(full_data)
colnames(full_data) <- c('Match', 'CMIP_Longitude', 'CMIP_Latitude',
                         'CMIP_Temperature', 'CMIP_Precipitation',
                         'Year', 'Month', 'PRISM_Latitude', 'PRISM_Longitude',
                         'PRISM_Precipitation', 'PRISM_Temperature')

# Check that coordinates are a close match
full_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = CMIP_Longitude, y = PRISM_Longitude)) +
  ggplot2::geom_abline()
full_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = CMIP_Latitude, y = PRISM_Latitude)) +
  ggplot2::geom_abline()

# Plot cmip and prism temperature
full_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = CMIP_Temperature, y = PRISM_Temperature)) +
  ggplot2::geom_smooth(ggplot2::aes(x = CMIP_Temperature, y = PRISM_Temperature), method = 'lm') +
  ggplot2::geom_abline(slope = 1, color = 'pink')

full_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = CMIP_Temperature, y = PRISM_Temperature, color = as.factor(Month))) +
  ggplot2::geom_smooth(ggplot2::aes(x = CMIP_Temperature, y = PRISM_Temperature), method = 'lm') +
  ggplot2::geom_abline(slope = 1, color = 'pink')

# plot cmip and prism precipitation
full_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = CMIP_Precipitation, y = PRISM_Precipitation)) +
  ggplot2::geom_smooth(ggplot2::aes(x = CMIP_Precipitation, y = PRISM_Precipitation), method = 'lm') +
  ggplot2::geom_abline(slope = 1, color = 'pink')

full_data |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = CMIP_Precipitation, y = PRISM_Precipitation, color = as.factor(Month))) +
  ggplot2::geom_smooth(ggplot2::aes(x = CMIP_Precipitation, y = PRISM_Precipitation), method = 'lm') +
  ggplot2::geom_abline(slope = 1, color = 'pink')

# Save
save(full_data, file = 'Climate_Data/prism_historical.RData')
