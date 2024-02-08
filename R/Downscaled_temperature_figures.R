## Visualizations of downscaled past2k mean and SD of temperature & precipitation

rm(list = ls())

library(dplyr)
library(ggplot2)
library(cowplot)

load('downscaled_processed_xydata.RData')

# For mapping
states <- map_data(map = 'state', region = c('michigan', 'minnesota', 'wisconsin'))

## Mean temperature over space for one time point

p1 <- xydata |>
  filter(YBP == 200) |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = Mean_Temperature)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrRd', direction = 'horizontal',
                        name = 'Temperature (°C)') +
  ggtitle('Mean annual temperature') +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))
p1

## Mean precipitation over space for one time point

p2 <- xydata |>
  filter(YBP == 200) |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = Mean_Precipitation)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  scale_color_distiller(palette = 'Blues', direction = 'horizontal',
                        name = 'Precipitation\n(mm/month)') +
  ggtitle('Mean annual precipitation') +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))
p2

## SD of temperature over space for one time point

p3 <- xydata |>
  filter(YBP == 200) |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = SD_Temperature)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  scale_color_distiller(palette = 'YlOrRd', direction = 'horizontal',
                        name = 'Temperature\nSD (°C)') +
  ggtitle('SD of annual temperature') +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))
p3 

p4 <- xydata |>
  filter(YBP == 200) |>
  ggplot(aes(x = PRISM_Longitude, y = PRISM_Latitude, color = SD_Precipitation)) +
  geom_point() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  theme_void() +
  scale_color_distiller(palette = 'Blues', direction = 'horizontal',
                        name = 'Precipitation\nSD (mm/month)') +
  ggtitle('SD of annual precipitation') +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5))
p4

plot_grid(p1, p2,
          p3, p4, nrow = 2)
