#' @name station_quality
#' @title Check the quality of an individual station based on its observations and the outputs after persistence check.
#' @description For an indivifual station, if more than 35% of its raw observations records zero,
#' then it is a suspect station; if more than 5% of its nonzero observations failed the persist test, 
#' then it is a low-quality station; if there are very long periods of missing data, then it is also a
#' doubtful station.
#' We suggest manually check for such stations.
#' @param windspeed.df a data.frame, the output of the persist check.
#' @return a list of percentages to determine the station quality
#' @export

station_quality <- function(windspeed.df)
{
  require(lubridate)
  
  # check the percentage of zero wind speed observations in the new data after temporal consistency check
  zero_label = which(windspeed.df$windspeed_metrepersecond == 0)
  nonzero_label = which(windspeed.df$new_data_persist != 0)
  zero_percent = length(zero_label) / (length(zero_label) + length(nonzero_label)) * 100
  # check the percentage of failed nonzero wind speeds by persistence test.
  persist_fail = which(windspeed.df$flag_persist == "fail.persist")
  nonzero_persist_fail = setdiff(persist_fail, zero_label)
  nonzero_persist_fail_percent = length(nonzero_persist_fail) / (length(zero_label) + length(nonzero_label) + 
                                                                   length(nonzero_persist_fail)) * 100
  # check if there are long-duration missing data during activation period of the station
  days.time.range = as.numeric( ceiling( diff( range(windspeed.df$datetime) ) ) ) # the initial time range, number of days
  days.active.period = length( unique( lubridate::date(windspeed.df$datetime) ) ) # the actual number of days that report valid observations
  valid_percent = days.active.period / days.time.range * 100

  return(c(zero_percent, nonzero_persist_fail_percent, valid_percent))
}

