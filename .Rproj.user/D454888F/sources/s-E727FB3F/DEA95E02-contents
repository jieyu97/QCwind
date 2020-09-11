#' @name temporal_step_check
#' @title Temporal step check function
#' @description Check on a maximum allowed variability of an instantaneous value;
#' Observations should change less than a maximum allowed number `max.variation`,
#' during a time interval of length `step.duration`;
#' Generating a new sequence of flags, where a `P` means that observation passes the step check,
#' and a pre-defined flag name `fail.flag` means that observation fails the step check;
#' Also outputting a new sequence of observations after removing failed observations.
#' @param data a data.frame that includes observation data with information.
#' @param data.column a character, the selected column name in the data.frame that
#' represents observation data to be checked.
#' @param datetime.column a character, the selected column name in the data.frame that
#' represents the reporting dates and times of corresponding observations.
#' @param step.duration a positive number, the interval time for step test, the unit is 'second'.
#' @param max.variation a positive number, the maximum allowed variability of observations.
#' @param fail.flag a character/string that represents the name of flag where an
#' observation fails the step check.
#' @return a new tbl_df that extends two new columns on the input data,
#' the first added column `flag` represents the flags (`P` means pass, `fail.flag` means fail,
#' `isolated` means missing previous observations), the second added column `new_data` represents
#' the observation data after removing failed observations.
#' @export
#' @examples
#' wow_test_step_check = temporal_step_check(wow_test_each[[1]],
#' data.column = 'windspeed_metrepersecond', datetime.column = 'datetime',
#' step.duration = 720, max.variation = 5, fail.flag = 'TS1')
#' attributes(wow_test_step_check)

temporal_step_check <- function(data, data.column, datetime.column,
                                step.duration, max.variation, fail.flag)
{
  require(tidyverse)
  require(xts)
  stopifnot(step.duration > 0)
  stopifnot(max.variation > 0)
  stopifnot(is.character(fail.flag))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data.column), data.column %in% colnames(data))
  stopifnot(is.character(datetime.column), datetime.column %in% colnames(data))

  Sys.setenv(TZ = "GMT")

  # # for test
  # data = wow_test_each[[1]]
  # data.column = 'windspeed_metrepersecond'
  # datetime.column = 'datetime'
  # step.duration = 720
  # max.variation = 5
  # fail.flag = 'TS1'

  data = as.data.frame(data)
  obs.data = data[[data.column]]    # data[,data.column]
  obs.datetime = data[[datetime.column]]    # data[,datetime.column]
  attr(obs.datetime, 'tzone') = "GMT"

  time_series = xts(x = obs.data, order.by = obs.datetime)
  step_dt_seq = tibble('start' = obs.datetime - step.duration, 'end' = obs.datetime)
  step_dt_seq = step_dt_seq[order(step_dt_seq$start),]
  step_dt_label = str_c(step_dt_seq$start,'/',step_dt_seq$end)

  variation.data = unlist( lapply(step_dt_label, FUN = function(x){
    ts_interval = time_series[x]
    diff.value = ifelse(length(!is.na(ts_interval)) > 1,
           max(ts_interval, na.rm = TRUE) - min(ts_interval, na.rm = TRUE), NA)
    return(diff.value)
  }) )

  lag.data = dplyr::lag(coredata(time_series))
  # lead.data = dplyr::lead(coredata(time_series))

  lag.diff.data = abs(coredata(time_series) - lag.data)
  # lead.diff.data = abs(coredata(time_series) - lead.data)

  output_data = data[order(data[[datetime.column]]),] %>%
    mutate(flag = ifelse( (variation.data > max.variation &
                             lag.diff.data > max.variation ),
                          fail.flag, ifelse(!is.na(variation.data), 'P', 'isolated') ) ) %>%
    # mutate(flag = ifelse( (variation.data > max.variation &
    #                           (lag.diff.data > max.variation |
    #                           lead.diff.data > max.variation) ),
    #                        fail.flag, ifelse(!is.na(variation.data), 'P', 'isolated') ) ) %>%
    mutate(new_data = ifelse( flag == fail.flag, NA, coredata(time_series) ) )

  output_data = as_tibble(output_data)

  attr(output_data, 'input_missing_value_percentage') = sum(!is.na(obs.data)) / length(obs.data)
  attr(output_data, 'pass_percentage') = sum(!is.na(output_data[['new_data']])) / sum(!is.na(obs.data))

  return(output_data)
}
