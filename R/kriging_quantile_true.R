#' @name kriging_quantile_true
#' @title Generate spatial interpolated estimates by ordinary kriging for
#' different quantiles of wind speed distribution.
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
#' @return a large list that contains the estimated quantiles at different locations of each period.
#' It has 6 sublists, each one represent a certain period;
#' Each sublist includes estimated quantiles from 0.01 to 1 at thousands of new locations.
#' @import tidyverse
#' @import gstat
#' @import sp
#' @importFrom dplyr tibble %>%
#' @importFrom lubridate month hour
#' @importFrom automap autoKrige
#' @export

kriging_quantile_true = function(official.windspeed, datetime.sequence,
                                 official.longitude, official.latitude, split.season = TRUE)
{
  stopifnot(is.data.frame(official.windspeed))
  stopifnot(lubridate::is.POSIXt(datetime.sequence))
  stopifnot(is.numeric(official.longitude), is.numeric(official.latitude))
  stopifnot(length(datetime.sequence) == nrow(official.windspeed))
  stopifnot(length(official.longitude) == ncol(official.windspeed),
            length(official.latitude) == ncol(official.windspeed))
  stopifnot(is.logical(split.season))


  ## split datetime by season and by day/night time
  datetime_split = tibble(datetime = datetime.sequence,
                          month = lubridate::month(datetime.sequence),
                          hour = lubridate::hour(datetime.sequence),
                          season = 'summer', # summer: 6,7,8,9; winter: 12,1,2,3; trans: 4,5,10,11;
                          daytime = 'night', # day: 7--18; night: 19--6;
                          class = 'day_s')
  datetime_split$season[ which( datetime_split$month %in% c(12,1,2,3) ) ] = 'winter'
  datetime_split$season[ which( datetime_split$month %in% c(4,5,10,11) ) ] = 'trans'
  datetime_split$daytime[ which( datetime_split$hour %in% c(7:18) ) ] = 'day'
  datetime_split$class = paste0(datetime_split$season,'_',datetime_split$daytime)
  split_class = unique(datetime_split$class)

  ## generate a list of six tables with 100 official quantiles
  official_speed_quantiles = c()
  for (period in 1:6) {
    official_speed_quantile = as.data.frame(matrix(NA,100,ncol(official.windspeed)))
    colnames(official_speed_quantile) = colnames(official.windspeed)
    label = which(datetime_split$class == split_class[period])
    for (k in 1:ncol(official.windspeed)) {
      official_speed_quantile[,k] =
        quantile(official.windspeed[label,k], probs = seq(0.01,1,0.01), na.rm = TRUE)
    }
    attr(official_speed_quantile, '6periods.split_class') = split_class[period]
    official_speed_quantiles[[period]] = official_speed_quantile
  }

  kriging_estimate.6periods = list()
  for (period in 1:6) {
    quantile_df = t(official_speed_quantiles[[period]])
    quantile_df.all = as.data.frame( cbind(official.latitude, official.longitude, quantile_df) )
    colnames(quantile_df.all) = c('lat','lon',paste0('quantile',1:100))
    ## convert lat/lon to cartesian coordinates
    quantile_xy = quantile_df.all
    coordinates(quantile_xy) = ~lon+lat
    proj4string(quantile_xy) = CRS("+proj=longlat +datum=WGS84")
    quantile_xy = spTransform(quantile_xy, CRS("+proj=utm +zone=51 ellps=WGS84"))

    kriging_each_period = list()
    for (nq in 1:100) {
      quantile_nq = paste0('quantile',nq)

      kriging_df <- automap::autoKrige(
        formula = quantile_xy[[quantile_nq]] ~ 1,
        input_data = quantile_xy,
        model = "Exp"
      ) %>%
        .$krige_output %>%
        as.data.frame() %>%
        dplyr::select(x = x1, y = x2, quantile = var1.pred)

      kriging_each_period[[nq]] = kriging_df
    }

    kriging_estimate.6periods[[period]] = kriging_each_period
  }

  return(kriging_estimate.6periods)
}
