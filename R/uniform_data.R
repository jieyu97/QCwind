#' @name uniform_data
#' @title Converting observation data to uniform time series
#' @description Citizen weather observations do not have uniform standards, this function converts the time series of
#' observations by various citizen weather stations into the same unified form.
#' @param data a data.frame that includes observation data with datetime information.
#' @param data.column a character, the selected column name in the data.frame that
#' represents observation data to be checked.
#' @param datetime.column a character, the selected column name in the data.frame that
#' represents dates and times of observations to be checked.
#' @param timeseq a date and time vector, indicating the standard reporting timestamps.
#' `timeseq` must have the same length with `data`.
#' @param freq a number, the standard reporting interval time, the unit is "second".
#' We typically use 10 (min) for forecast use, and 60 (min) for analysis use.
#' @param method the matching method, choosing from "average", "maximum", "nearest", or "angle.average".
#' "average": the average value of observations in each time interval with length `freq`.
#' "maximum": the maximum value of observations in each time interval with length `freq`.
#' "nearest": the nearest observation to `timeseq` in each time interval with length `freq`.
#' "angle.average": the circular average value of angle observations
#' in each time interval with length `freq`.
#' @param position the time window considered to match the standard 10-min timestamps:
#' "centre" means the time window is centred at the standard timestamps;
#' "past" means the time window is the past 10-min interval of the standard timestamps.
#' @return a new matched time series, with standard reporting time every `interval` minutes.
#' @export
#' @examples
#' library(tidyverse)
#' datetime = as.POSIXlt(seq(0,30000,300), origin = "2017-02-03 08:00:00")
#' datetime_seq_standard = as.POSIXlt(seq(0,30000,600), origin = "2017-02-03 08:00:00")
#' test = tibble(datetime = datetime,
#'               windspeed = c(rep(10.2,5),seq(10,15,0.1),rep(9.3,45)))
#' standard_ts = uniform_data(data = test, data.column = 'windspeed',
#'                            datetime.column = 'datetime',
#'                            timeseq = datetime_seq_standard,
#'                            method = "average")
#' str(standard_ts)
#' head(standard_ts)

uniform_data = function(data, data.column, datetime.column,
                        timeseq, freq = 600, method = "average", position = "past")
{
  require(tidyverse)
  require(xts)
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data.column), data.column %in% colnames(data))
  stopifnot(is.character(datetime.column), datetime.column %in% colnames(data))
  stopifnot(lubridate::is.POSIXt(timeseq))
  stopifnot(freq > 0)
  stopifnot(is.character(method),
            method %in% c("average", "maximum", "nearest", "angle.average"))
  stopifnot(is.character(position),
            position %in% c("past", "centre"))
  
  # # for test
  # data = wow_test_each[[1]]
  # data.column = 'windspeed_metrepersecond'
  # datetime.column = 'datetime'
  # timeseq = datetime_seq_test
  # freq = 600

  data = as.data.frame(data)
  obs.data = data[[data.column]]    # data[,data.column]
  obs.datetime = data[[datetime.column]]    # data[,datetime.column]
  attr(obs.datetime, 'tzone') = "GMT"

  time_series = xts(x = obs.data, order.by = obs.datetime)
  if (position == "centre") {
    standard_dt_seq = tibble('start' = timeseq - (freq/2),
                             'end' = timeseq + (freq/2 - 1) )
  } else {
    standard_dt_seq = tibble('start' = timeseq - freq + 1,
                             'end' = timeseq )
  }
  standard_dt_seq = standard_dt_seq[order(standard_dt_seq$start),]
  standard_dt_label = str_c(standard_dt_seq$start,'/',standard_dt_seq$end)

  # output_xts = xts(x = rep(NA, length(timeseq)), order.by = timeseq)

  if (method == "average")
  {
    output.data = unlist( lapply(standard_dt_label, FUN = function(x){
      ts_interval = time_series[x]
      interval.data = ts_interval[!is.na(ts_interval)]
      average.value = ifelse( length(interval.data) > 0, mean(interval.data), NA)
      return(average.value)
    }) )
  }
  else if (method == "maximum")
  {
    output.data = unlist( lapply(standard_dt_label, FUN = function(x){
      ts_interval = time_series[x]
      interval.data = ts_interval[!is.na(ts_interval)]
      max.value = ifelse( length(interval.data) > 0, max(interval.data), NA)
      return(max.value)
    }) )
  }
  else if (method == "nearest")
  {
    time_series_intervals = lapply(standard_dt_label, FUN = function(x){
      time_series[x] } )
    output.data = unlist( lapply(1:length(time_series_intervals), FUN = function(i){
      ts_interval = time_series_intervals[[i]]
      ts_interval = ts_interval[!is.na(ts_interval)]
      nearest.value = ifelse( length(ts_interval) > 0,
                              coredata(ts_interval)[which.min( abs( index(ts_interval) - timeseq[i]) )],
                              NA)
      # nearest_label = which.min( abs( index(ts_interval) - timeseq[i]) )
      # nearest.value = coredata(ts_interval)[nearest_label]
      return(nearest.value)
    }) )
  }
  else if (method == "angle.average")
  {
    output.data = unlist( lapply(standard_dt_label, FUN = function(x){
      ts_interval = time_series[x]
      interval.data = ts_interval[!is.na(ts_interval)]
      average.angle = ifelse( length(interval.data) > 0, angle_average(interval.data), NA)
      return(average.angle)
    }) )
  }

  output_xts = xts(x = output.data, order.by = timeseq)
  attr(output_xts,"tzone") = "GMT"

  return(output_xts)
}
