# Data downloaded using the following options from the PRISM website:
# 1. Historical Past
# 2. Precipitation & mean temperature (downloaded separately)
# 3. "Download All Historical Data (.bil)"

rm(list = ls())

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
ppt <- sf::st_as_sf(ppt, coords = c('x', 'y'))
tmean <- sf::st_as_sf(tmean, coords = c('x', 'y'))

# Add current projection
# Currently in GCS_North_American_1983 EPSG 4269
sf::st_crs(ppt) <- sf::st_crs(tmean) <- 'EPSG:4269'

# Reproject to EPSG 4326
ppt <- sf::st_transform(ppt, crs = 'EPSG:4326')
tmean <- sf::st_transform(tmean, crs = 'EPSG:4326')

# Boundaries of pollen data
min_lon <- -98.11711
max_lon <- -82.59814
min_lat <- 41.50088
max_lat <- 50.17222

# Change back to regular data frame
ppt <- sfheaders::sf_to_df(ppt, fill = TRUE)
tmean <- sfheaders::sf_to_df(tmean, fill = TRUE)

ppt <- dplyr::select(ppt, -c(sfg_id, point_id))
tmean <- dplyr::select(tmean, -c(sfg_id, point_id))

# Filter precipitation matrix to include only spatial area of interest
# Then reformat to make easier to average over years while keeping separate months
ppt_cols <- colnames(ppt)
ppt_cols <- ppt_cols[-(1033:1034)]

ppt_long <- ppt |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(Latitude >= min_lat & Latitude <= max_lat) |>
  dplyr::filter(Longitude >= min_lon & Longitude <= max_lon) |>
  tidyr::pivot_longer(dplyr::all_of(ppt_cols), 
                      names_to = 'var', values_to = 'PPT')

# Separate year and month from the "var" variable
# Done in a separate step to avoid maxing out memory
ppt_long <- ppt_long |>  
  dplyr::mutate(year = substr(var, 24, 27),
                month = substr(var, 28, 29))

# Find average per month over period 1895-1980
average_ppt <- ppt_long |>
  dplyr::select(-var) |>
  dplyr::group_by(Latitude, Longitude, month) |>
  dplyr::summarize(PPT = mean(PPT))

# Repeat for temperature
tmean_cols <- colnames(tmean)
tmean_cols <- tmean_cols[-(1033:1034)]

tmean_long <- tmean |>
  dplyr::rename(Latitude = y,
                Longitude = x) |>
  dplyr::filter(Latitude >= min_lat & Latitude <= max_lat) |>
  dplyr::filter(Longitude >= min_lon & Longitude <= max_lon) |>
  tidyr::pivot_longer(dplyr::all_of(tmean_cols),
                      names_to = 'var', values_to = 'Tmean')

tmean_long <- tmean_long |>
  dplyr::mutate(year = substr(var, 26, 29),
                month = substr(var, 30, 31))

average_tmean <- tmean_long |>
  dplyr::select(-var) |>
  dplyr::group_by(Latitude, Longitude, month) |>
  dplyr::summarize(Tmean = mean(Tmean))

average_ppt$month <- as.factor(average_ppt$month)
average_tmean$month <- as.factor(average_tmean$month)

# Combine precipitation and temperature
average_clim <- cbind(average_ppt, average_tmean$Tmean)
colnames(average_clim)[ncol(average_clim)] <- 'Tmean'

# Combine precipitation and temperature with years
prism_clim <- cbind(ppt_long, tmean_long$Tmean)
colnames(prism_clim)[ncol(prism_clim)] <- 'Tmean'

# Plot of region
states <- sf::st_as_sf(maps::map('state', region = c('minnesota', 'wisconsin', 'michigan'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Plot precipitation
average_clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Longitude, y = Latitude, color = PPT), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~month) +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_void()

# Plot temperature
average_clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Longitude, y = Latitude, color = Tmean), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~month) +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_void()

# Save
save(average_clim, file = 'Climate_Data/average_prism.RData')
save(prism_clim, file = 'Climate_Data/prism_clim.RData')
