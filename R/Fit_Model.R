rm(list = ls())

library(dplyr)
library(ggplot2)

load('Climate_Data/prism_historical.RData')

# Without topography for temperature
fit_temp <- lm(PRISM_Temperature ~ CMIP_Temperature + abs(Month - 7) +
             PRISM_Latitude, data = full_data)

summary(fit_temp)

temperature_fitted <- fit_temp$fitted.values

temperature_fitted <- as.data.frame(temperature_fitted)
temperature_fitted$Longitude <- full_data$CMIP_Longitude
temperature_fitted$Latitude <- full_data$CMIP_Latitude
temperature_fitted$Month <- full_data$Month

states <- map_data('state') |> filter(region %in% c('michigan', 'minnesota', 'wisconsin'))

temperature_fitted |>
  ggplot(aes(x = Longitude, y = Latitude, color = temperature_fitted)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  geom_point() +
  theme_void() +
  scale_color_viridis_c(option = 'F', name = 'Temperature') +
  facet_wrap(~Month)

# Without topography precipitation
fit_precip <- lm(PRISM_Precipitation ~ CMIP_Precipitation + abs(Month - 6.5) +
              PRISM_Latitude + PRISM_Longitude, data = full_data)

summary(fit_precip)

precipitation_fitted <- fit_precip$fitted.values

precipitation_fitted <- as.data.frame(precipitation_fitted)

precipitation_fitted <- precipitation_fitted |>
  mutate(Latitude = full_data$CMIP_Latitude,
         Longitude = full_data$CMIP_Longitude,
         Month = full_data$Month)

precipitation_fitted |>
  ggplot(aes(x = Longitude, y = Latitude, color = precipitation_fitted)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = 'black', fill = NA) +
  geom_point() +
  theme_void() +
  scale_color_viridis_c(option = 'G', name = 'Precipitation') +
  facet_wrap(~Month)

save(fit_temp, fit_precip, file = 'Fit/latlon_fit.RData')
