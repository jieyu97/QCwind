#' @name kriging_quantiles.new
#' @title Generate spatial interpolated estimates at specified locations by ordinary
#' kriging for different quantiles of wind speed distribution.
#' @description For the input official wind speed data with locations, first find their quantiles from
#' 0.01 to 1 with step interval of 0.01 (100 in total), then for quantiles at each level using
#' ordinary kriging to interpolate estimated quantiles at other locations.
#' @param official.windspeed a data.frame of official/reference wind speed observations,
#' each column represents an individual station, each row represents a timestamp.
#' @param datetime.sequence the date time stamps/sequence of the data `official.windspeed`,
#' should indicate the row names of `official.windspeed`.
#' @param official.longitude a numeric vector, contains longitude information of official stations,
#' should relate to the column names (station IDs) of `official.windspeed`.
#' @param official.latitude a numeric vector, contains latitude information of official stations,
#' should relate to the column names (station IDs) of `official.windspeed`.
#' @param split.season a logical, if `TRUE` we perform the spatial interpolation by six different periods
#' (winter/summer/transition season, day/night time) respectively;
#' The six periods are determined by months and hours in a day:
#' winter - 12,1,2,3; summer - 6,7,8,9; trans (transition periods between winter and summer) - 4,5,10,11;
#' day - 7:00~18:00; night - 19:00~6:00;
#' 6 periods: winter_night, winter_day, trans_night, trans_day, summer_night, summer_day.
#' @param station.id a list of test station IDs, relating to `station.longitude` and `station.latitude`.
#' @param station.longitude a numeric vector, contains longitude information of test stations,
#' should relate to the column names (station IDs) of `official.windspeed`.
#' @param station.latitude a numeric vector, contains latitude information of test stations,
#' should relate to the column names (station IDs) of `official.windspeed`.
#' @return a large list of interpolated quantiles for different test station, each sublist includes
#' estimate quantiles (0.01-1.00) during each of the six periods.
#' @import tidyverse
#' @import gstat
#' @import sp
#' @importFrom dplyr tibble %>%
#' @importFrom lubridate month hour
#' @importFrom automap autoKrige
#' @export


kriging_quantiles.new = function (official.windspeed, datetime.sequence, official.longitude,
                                  official.latitude, split.season = TRUE, station.id,
                                  station.longitude, station.latitude)
{
  stopifnot(is.data.frame(official.windspeed))
  stopifnot(lubridate::is.POSIXt(datetime.sequence))
  stopifnot(is.numeric(official.longitude), is.numeric(official.latitude))
  stopifnot(length(datetime.sequence) == nrow(official.windspeed))
  stopifnot(length(official.longitude) == ncol(official.windspeed),
            length(official.latitude) == ncol(official.windspeed))
  stopifnot(is.logical(split.season))
  stopifnot(length(station.id) == length(station.longitude),
            length(station.id) == length(station.latitude))

  station.location = tibble(station_id = station.id, lon = station.longitude,
                            lat = station.latitude)

  datetime_split = tibble(datetime = datetime.sequence, month = lubridate::month(datetime.sequence),
                          hour = lubridate::hour(datetime.sequence), season = "summer",
                          daytime = "night", class = "day_s")
  datetime_split$season[which(datetime_split$month %in% c(12,
                                                          1, 2, 3))] = "winter"
  datetime_split$season[which(datetime_split$month %in% c(4,
                                                          5, 10, 11))] = "trans"
  datetime_split$daytime[which(datetime_split$hour %in% c(7:18))] = "day"
  datetime_split$class = paste0(datetime_split$season, "_",
                                datetime_split$daytime)
  split_class = unique(datetime_split$class)
  official_speed_quantiles = c()
  for (period in 1:6) {
    official_speed_quantile = as.data.frame(matrix(NA, 100,
                                                   ncol(official.windspeed)))
    colnames(official_speed_quantile) = colnames(official.windspeed)
    label = which(datetime_split$class == split_class[period])
    for (k in 1:ncol(official.windspeed)) {
      official_speed_quantile[, k] = quantile(official.windspeed[label,
                                                                 k], probs = seq(0.01, 1, 0.01), na.rm = TRUE)
    }
    attr(official_speed_quantile, "6periods.split_class") = split_class[period]
    official_speed_quantiles[[period]] = official_speed_quantile
  }

  coordinates(station.location) = ~lon + lat
  proj4string(station.location) = CRS(SRS_string = "EPSG:4326")
  station.location_xy = spTransform(station.location,
                                    CRS(SRS_string = "EPSG:32631"))
  # # about geo spatial system:
  # (x = CRS("+init=epsg:32631")) # CRS(SRS_string="EPSG:32631")
  # # CRS arguments: +proj=utm +zone=31 +datum=WGS84 +units=m +no_defs
  # cat(comment(x), "\n")
  # (y = CRS("+init=epsg:4326")) # CRS(SRS_string="EPSG:4326")
  # # CRS arguments: +proj=longlat +datum=WGS84 +no_defs
  # cat(comment(y), "\n")

  kriging_estimate.6periods = list()
  for (period in 1:6) {
    quantile_df = t(official_speed_quantiles[[period]])
    quantile_df.all = as.data.frame(cbind(official.latitude,
                                          official.longitude, quantile_df))
    colnames(quantile_df.all) = c("lat", "lon",
                                  paste0("quantile", 1:100))
    quantile_ll = quantile_df.all
    coordinates(quantile_ll) = ~lon + lat
    proj4string(quantile_ll) = CRS(SRS_string = "EPSG:4326")
    quantile_xy = spTransform(quantile_ll, CRS(SRS_string = "EPSG:32631"))
    kriging_each_period = list()
    for (nq in 1:100) {
      quantile_nq = paste0("quantile", nq)
      kriging_df <- automap::autoKrige(formula = quantile_xy[[quantile_nq]] ~
                                         1, input_data = quantile_xy,
                                       new_data = station.location_xy,
                                       model = "Exp") %>%
        .$krige_output %>% as.data.frame() %>%
        dplyr::select(quantile = var1.pred)
      kriging_each_period[[nq]] = kriging_df
    }
    kriging_estimate.6periods[[period]] = kriging_each_period
  }

  quantile.estimate.each_station = list()
  for (w in 1:length(station.id)) {
    quantile.estimate = list()
    for (period in 1:6) {
      kriging_each_period = kriging_estimate.6periods[[period]]
      interpolate_quantiles = rep(NA, 100)
      for (nq in 1:100) {
        quantile_nq = kriging_each_period[[nq]]
        interpolate_quantiles[nq] = quantile_nq[w,]
      }
      quantile.estimate[[period]] = interpolate_quantiles
    }
    quantile.estimate.each_station[[w]] = quantile.estimate
  }

  return(quantile.estimate.each_station)

}
