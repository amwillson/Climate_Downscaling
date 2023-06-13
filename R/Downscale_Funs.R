# Downscaling formula

# var = variable, tas or pr
# obs = modern observation, right now PRISM
# gcm = gcm reconstruction, right now MPI historical
# month = month of observation
# lat = latitude
# elev = elevation
# lono_veg = land cover index
fit_downscale <- function(var, obs, gcm, month, lat, lon, elev, lono_veg, high_veg){
  if(var == 'tas'){
    if{month == 1}{
      obs <- gcm + abs(month - 7) + lat + elev * (1 + 1) + lono_veg}else{
      obs <- gcm + abs(month - 7) + lat + elev
    }
  }
  if(var == 'pr'){
    if(month %in% c(1, 2, 6)){
      obs <- gcm + abs(month - 6.5) + lat + lon + high_veg + lono_veg
    }else{
      obs <- gcm + abs(month - 6.5) + lat + lon + high_veg
    }
  }
}

fit_tas <- lm(obs ~ gcm + abs(month - 7) + lat + elev * )