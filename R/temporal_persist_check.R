#' @name temporal_persist_check
#' @title Temporal persistence check function
#' @description Check on a minimum required variability of an instantaneous value;
#' Observations should change more than a minimum required number `min.variation`,
#' during a time interval of length `persist.duration`;
#' Generating a new sequence of flags, and outputting a new sequence of observations after removing failed observations.
#' @param data a data.frame that includes observation data with information.
#' @param data.column a character, the selected column name in the data.frame that
#' represents observation data to be checked.
#' @param datetime.column a character, the selected column name in the data.frame that
#' represents the reporting dates and times of corresponding observations.
#' @param persist.duration a positive number, the interval time for persistence test,
#' the unit is 'second'.
#' @param min.variation a positive number, the minimum required variability of observations.
#' @param realtime a logical. `TRUE` means processing in realtime,
#' the attributes that includes pass percent would be removed in realtime case.
#' @return a new tbl_df that extends two new columns on the input data,
#' the first added column `flag_persist` represents the flags (`P` means pass, `fail.persist` means fail,
#' `isolated` means missing previous observations), the second added column `new_data_persist` represents
#' the observation data after removing failed observations.
#' @import xts
#' @import tidyverse
#' @importFrom zoo coredata index
#' @export
#' @examples
#' library(tidyverse)
#' datetime = as.POSIXlt(seq(0,60000,600), origin = "2017-02-03 08:00:00")
#' test_data = tibble(datetime = datetime,
#'               windspeed = c(rep(10.2,5),rep(11.3,15),seq(10,15,0.1),
#'                             rep(9.3,20),c(NA,NA,NA,2,NA,5,7,NA,2,4)))
#' test_persist_check = temporal_persist_check(test_data, data.column = 'windspeed',
#'                                             datetime.column = 'datetime',
#'                                             persist.duration = 3600,
#'                                             min.variation = 0.1)
#' attributes(test_persist_check)
#' test_persist_check

temporal_persist_check <- function(data, data.column, datetime.column,
                                persist.duration, min.variation)
{
  stopifnot(persist.duration > 0)
  stopifnot(min.variation > 0)
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data.column), data.column %in% colnames(data))
  stopifnot(is.character(datetime.column), datetime.column %in% colnames(data))

  Sys.setenv(TZ = "GMT")

  data = as_tibble(data)
  obs.data = data[[data.column]]    # data[,data.column]
  obs.datetime = data[[datetime.column]]    # data[,datetime.column]
  attr(obs.datetime, 'tzone') = "GMT"
  data = data[order(data[[datetime.column]]),]

  time_series = xts(x = obs.data, order.by = obs.datetime)
  persist_dt_seq = tibble(start = obs.datetime - persist.duration, end = obs.datetime + 0)
  persist_dt_seq = persist_dt_seq[order(persist_dt_seq$start),]
  persist_dt_label = str_c(persist_dt_seq$start,'/',persist_dt_seq$end)

  # choose a number `persist.duration/600/2`:
  # ensure when checking the persistence of data in the time window,
  # there are at least half valid (not NA) data to consider.
  variation.data = unlist( lapply(persist_dt_label, FUN = function(x){
    ts_interval = time_series[x]
    diff.value = ifelse(sum(!is.na(ts_interval)) > persist.duration/600/2,
                        max(ts_interval, na.rm = TRUE) - min(ts_interval, na.rm = TRUE), NA)
    return(diff.value)
  }) )

  center.data = as.numeric(coredata(time_series))

  lag.data = dplyr::lag(center.data)
  lead.data = dplyr::lead(center.data)

  # flag - NA, no observaions
  label.center.na = which(is.na(center.data))
  # flag - isolate, isolated observations
  start_label = str_c(data[[datetime.column]][1],'/',data[[datetime.column]][1] + persist.duration)
  ts_start = time_series[start_label]
  label.center.isolate = union( union(
    which(!is.na(center.data) & is.na(lag.data) & is.na(lead.data)),
    which(!is.na(center.data) & is.na(variation.data)) ),
    which(data[[datetime.column]] %in% index(ts_start)) )
  # flag - fail.persist, failed observations
  label.center.fail0 = which(variation.data <= min.variation)
  if (length(label.center.fail0) > 0) {
    label.center.fail1 = lapply(label.center.fail0, function(x) {
      ts_short = time_series[persist_dt_label[x]]
      ts_short_label = which(data[[datetime.column]] %in% index(ts_short))
      return(ts_short_label)
    })
    label.center.fail = c()
    for (i in 1:length(label.center.fail1)) {
      label.center.fail = union(label.center.fail, label.center.fail1[[i]])
    }
  } else {
    label.center.fail = NA
  }

  output_flag = data %>% mutate(flag_persist = "P")
  output_flag$flag_persist[label.center.fail] = "fail.persist"
  output_flag$flag_persist[label.center.isolate] = "isolated"
  output_flag$flag_persist[label.center.na] = "missing"

  output_data = output_flag %>%
    mutate(new_data_persist = ifelse( flag_persist == "fail.persist", NA, center.data ) )

  attr(output_data, 'persist_pass_percent') = sum(!is.na(output_data$new_data_persist)) /
      sum(!is.na(obs.data)) * 100

  return(output_data)
}
