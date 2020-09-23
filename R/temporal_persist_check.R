#' @name temporal_persist_check
#' @title Temporal persistence check function
#' @description Check on a minimum required variability of an instantaneous value;
#' Observations should change more than a minimum required number `min.variation`,
#' during a time interval of length `persist.duration`;
#' Generating a new sequence of flags, where a `P` means that observation passes the persistence check,
#' and a pre-defined flag name `fail.flag` means that observation fails the persistence check;
#' Also outputting a new sequence of observations after removing failed observations.
#' @param data a data.frame that includes observation data with information.
#' @param data.column a character, the selected column name in the data.frame that
#' represents observation data to be checked.
#' @param datetime.column a character, the selected column name in the data.frame that
#' represents the reporting dates and times of corresponding observations.
#' @param persist.duration a positive number, the interval time for persistence test,
#' the unit is 'second'.
#' @param min.variation a positive number, the minimum required variability of observations.
#' @param fail.flag a character/string that represents the name of flag where an
#' observation fails the persistence check.
#' @return a new tbl_df that extends two new columns on the input data,
#' the first added column `flag` represents the flags (`P` means pass, `fail.flag` means fail,
#' `isolated` means missing previous observations), the second added column `new_data` represents
#' the observation data after removing failed observations.
#' @export
#' @examples
# library(tidyverse)
# datetime = as.POSIXlt(seq(0,60000,600), origin = "2017-02-03 08:00:00")
# test = tibble(datetime = datetime,
#               windspeed = c(rep(10.2,5),seq(10,15,0.1),rep(9.3,45)))
# test_persist_check = temporal_persist_check(test, data.column = 'windspeed',
#                                             datetime.column = 'datetime',
#                                             persist.duration = 3600,
#                                             min.variation = 0.1, fail.flag = 'TS2')
# attributes(test_persist_check)
# test_persist_check

temporal_persist_check <- function(data, data.column, datetime.column,
                                persist.duration, min.variation, fail.flag)
{
  require(tidyverse)
  require(xts)
  stopifnot(persist.duration > 0)
  stopifnot(min.variation > 0)
  stopifnot(is.character(fail.flag))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data.column), data.column %in% colnames(data))
  stopifnot(is.character(datetime.column), datetime.column %in% colnames(data))

  Sys.setenv(TZ = "GMT")

  # # for test
  # data = wow_test_each[[1]]
  # data.column = 'windspeed_metrepersecond'
  # datetime.column = 'datetime'
  # persist.duration = 3600
  # min.variation = 0.1
  # fail.flag = 'TS2'

  data = as_tibble(data)
  obs.data = data[[data.column]]    # data[,data.column]
  obs.datetime = data[[datetime.column]]    # data[,datetime.column]
  attr(obs.datetime, 'tzone') = "GMT"
  data = data[order(data[[datetime.column]]),]

  time_series = xts(x = obs.data, order.by = obs.datetime)
  persist_dt_seq = tibble('start' = obs.datetime - persist.duration, 'end' = obs.datetime)
  persist_dt_seq = persist_dt_seq[order(persist_dt_seq$start),]
  persist_dt_label = str_c(persist_dt_seq$start,'/',persist_dt_seq$end)

  # variation.data = lapply(persist_dt_label, FUN = function(x){
  #   ts_interval = time_series[x]
  #   diff.value = ifelse(length(!is.na(ts_interval)) > 1,
  #                       max(ts_interval, na.rm = TRUE) - min(ts_interval, na.rm = TRUE), NA)
  #   return(diff.value)
  # })
  variation.data = unlist( lapply(persist_dt_label, FUN = function(x){
    ts_interval = time_series[x]
    diff.value = ifelse(length(!is.na(ts_interval)) > 1,
                        max(ts_interval, na.rm = TRUE) - min(ts_interval, na.rm = TRUE), NA)
    return(diff.value)
  }) )

  output_data = data %>%
    mutate(flag_persist = if_else( variation.data <= min.variation,
                          fail.flag, 'P', 'isolated') ) %>%
    mutate(new_data_persist = ifelse( flag_persist == fail.flag, NA, coredata(time_series) ) )

  # output_data = as_tibble(output_data)

  attr(output_data, 'input_valid_data_percentage') = sum(!is.na(obs.data)) / length(obs.data)
  attr(output_data, 'pass_percentage') = sum(!is.na(output_data[['new_data_persist']])) /
    sum(!is.na(obs.data))

  return(output_data)
}
