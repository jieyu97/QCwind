#' @name interpolate_quantiles
#' @title Find the kriging estimated "truth" quantiles of a given test station.
#' @description Ordinary kriging.
#' @param kriging.reference.quantiles a large list, output kriging quantiles of `kriging_quantile_true`.
#' @param station.id a list of test station IDs, relating to `station.longitude` and `station.latitude`.
#' @param station.longitude a numeric vector, contains longitude information of test stations,
#' should relate to the column names (station IDs) of `official.windspeed`.
#' @param station.latitude a numeric vector, contains latitude information of test stations,
#' should relate to the column names (station IDs) of `official.windspeed`.
#' @return a large list of interpolated quantiles for different test station, each sublist includes
#' estimate quantiles during each of the six periods.
#' @import tidyverse
#' @importFrom dplyr tibble
#' @import gstat
#' @import sp
#' @export

interpolate_quantiles = function(kriging.reference.quantiles, station.id,
                                 station.longitude, station.latitude)
{
  stopifnot(length(station.id) == length(station.longitude),
            length(station.id) == length(station.latitude))

  station.location = tibble(station_id = station.id, lon = station.longitude, lat = station.latitude)
  coordinates(station.location) = ~lon+lat
  proj4string(station.location) = CRS("+proj=longlat +datum=WGS84")
  station.location_xy = spTransform(station.location, CRS("+proj=utm +zone=51 ellps=WGS84"))
  station.location_xy.df = tibble(station_id = station.id,
                                 x = station.location_xy@coords[,1],
                                 y = station.location_xy@coords[,2])

  quantile_estimate_wow_xy = function(target.coord_x, target.coord_y, kriging_locat)
  {
    all_distance_sq = (kriging_locat$x - target.coord_x)^2 + (kriging_locat$y - target.coord_y)^2
    label_min = which.min(all_distance_sq)
    value = kriging_locat[label_min,]
    return(value)
  }

  quantile.estimate.each_station = list()
  for (w in 1:length(station.id)) {
    quantile.estimate = list()
    for (period in 1:6) {
      kriging_each_period = kriging.reference.quantiles[[period]]

      interpolate_quantiles = tibble(interpolate_x = rep(0.0,100),
                                     interpolate_y = rep(0.0,100),
                                     estimate_quantile = rep(0.0,100))
      for (nq in 1:100) {
        interpolate_quantiles[nq,] = quantile_estimate_wow_xy(station.location_xy.df$x[w],
                                                              station.location_xy.df$y[w],
                                                              kriging_each_period[[nq]])
      }
      quantile.estimate[[period]] = interpolate_quantiles$estimate_quantile
    }
    quantile.estimate.each_station[[w]] = quantile.estimate
  }

  return(quantile.estimate.each_station)
}
