## Visualizing climate data

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

# Map of states for plotting
states <- map_data('state') |> filter(region %in% c('minnesota', 'michigan', 'wisconsin'))

# Start with climate projections from CMIP6

load('CMIP_Reconstructions/processed.RData')

## Look at January and July temperature and precipitation 
## for a few selected years

# Temperature January
clim |>
  filter(Year %in% c(200, 700, 1300, 1900)) |>
  filter(Month == 1) |>
  mutate(Year = as.character(Year),
         Year = if_else(Year == 200, '200 y BP', Year),
         Year = if_else(Year == 700, '700 y BP', Year),
         Year = if_else(Year == 1300, '1300 y BP', Year),
         Year = if_else(Year == 1900, '1900 y BP', Year),
         Year = factor(Year, levels = c('1900 y BP', '1300 y BP', 
                                        '700 y BP', '200 y BP'))) |>
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, color = Temperature)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Year) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrRd', direction = 'horizontal') +
  theme(strip.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Temperature July
clim |>
  filter(Year %in% c(200, 700, 1300, 1900)) |>
  filter(Month == 7) |>
  mutate(Year = as.character(Year),
         Year = if_else(Year == 200, '200 y BP', Year),
         Year = if_else(Year == 700, '700 y BP', Year),
         Year = if_else(Year == 1300, '1300 y BP', Year),
         Year = if_else(Year == 1900, '1900 y BP', Year)) |>
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, color = Temperature)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Year) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrRd', direction = 'horizontal') +
  theme(strip.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Precipitation January
clim |>
  filter(Year %in% c(200, 700, 1300, 1900)) |>
  filter(Month == 1) |>
  ggplot(aes(x = Longitude, y = Latitude, color = Precipitation)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Year) +
  theme_void() +
  scale_color_distiller(palette = 'Blues', direction = 'horizontal') +
  theme(strip.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Precipitation July
clim |>
  filter(Year %in% c(200, 700, 1300, 1900)) |>
  filter(Month == 7) |>
  ggplot(aes(x = Longitude, y = Latitude, color = Precipitation)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Year) +
  theme_void() +
  scale_color_distiller(palette = 'Blues', direction = 'horizontal') +
  theme(strip.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

## Now plot downscaled climate to compare with the original

load('downscaled_processed_xydata.RData')

xdata <- xydata |>
  select(c(Year, PRISM_Longitude:SD_Precipitation))

# pivot data
xdata <- xdata |>
  pivot_longer(cols = Downscale_Temperature_1:Downscale_Precipitation_12,
               names_to = 'Variable.Month', values_to = 'Value') |>
  mutate(Variable = substring(Variable.Month, 11, nchar(Variable.Month)-2),
         Month = substring(Variable.Month, nchar(Variable.Month), nchar(Variable.Month)))

# Temperature January
xdata |>
  filter(Year %in% c(200, 700, 1300, 1900)) |>
  filter(Month == '1') |>
  filter(Variable == 'Mean_Temperature') |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = Value)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Year) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrRd', direction = 'horizontal',
                        name = 'Temperature') +
  ggtitle('Mean January Temperature') +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
        strip.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Temperature July
xdata |>
  filter(Year %in% c(200, 700, 1300, 1900)) |>
  filter(Month == '7') |>
  filter(Variable == 'Temperature') |>
  ggplot() +
  geom_point(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = Value)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Year) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrRd', direction = 'horizontal',
                        name = 'Temperature') +
  ggtitle('Mean July Temperature') +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = 'bold'),
        strip.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Precipitation January
xdata |>
  filter(Year %in% c(400, 700, 1300, 1900)) |>
  filter(Month == '1') |>
  filter(Variable == 'Precipitation') |>
  ggplot() +
  geom_point(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = Value)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Year) +
  theme_minimal() +
  scale_color_distiller(palette = 'Blues', direction = 'horizontal')

# Precipitation July
xdata |>
  filter(Year %in% c(200, 700, 1300, 1900)) |>
  filter(Month == '7') |>
  filter(Variable == 'Precipitation') |>
  ggplot() +
  geom_point(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = Value)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  facet_wrap(~Year) +
  theme_minimal() +
  scale_color_distiller(palette = 'Blues', direction = 'horizontal')

## Calculate PDSI from season climate
## Compare with drought atlas version of PDSI in plots
## Could eventually use drought atlas to upweight ensemble members more consistent with drought atlas PDSI