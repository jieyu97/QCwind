#' @name temporal_step_check.improve
#' @title Temporal step check function (new version, can be implemented in realtime)
#' @description Same as the description in `temporal_step_check`.
#' @param data a data.frame that includes observation data with information.
#' @param data.column a character, the selected column name in the data.frame that
#' represents observation data to be checked.
#' @param datetime.column a character, the selected column name in the data.frame that
#' represents the reporting dates and times of corresponding observations.
#' @param step.duration a positive number, the interval time for step test, the unit is 'second'.
#' @param max.variation a positive number, the maximum allowed variability of observations.
#' @return a new tbl_df that extends two new columns on the input data,
#' the first added column `flag_step` represents the flags (`P` means pass, `fail.step` means fail,
#' `isolated` means missing previous observations), the second added column `new_data_step` represents
#' the observation data after removing failed observations in the step check.
#' @import xts
#' @import tidyverse
#' @importFrom dplyr tibble %>% mutate as_tibble
#' @importFrom stringr str_c
#' @importFrom zoo coredata index
#' @export
#' @examples
#' library(tidyverse)
#' datetime = as.POSIXct(seq(0,60000,600), origin = "2017-02-03 08:00:00")
#' test_data = tibble(datetime = datetime, windspeed = c(rep(10.2,5),rep(NA,5),rep(9.3,35),rep(20,5),
#'                                                 rep(3.5,40),c(1,3,NA,2,NA,5,6,7,NA,2,4)))
#' test_step_check = temporal_step_check.improve(test_data, data.column = 'windspeed',
#'                                       datetime.column = 'datetime',
#'                                       step.duration = 660,
#'                                       max.variation = 5)
#' attributes(test_step_check)
#' test_step_check

temporal_step_check.improve <- function(data, data.column, datetime.column,
                                step.duration, max.variation)
{
  stopifnot(step.duration > 0)
  stopifnot(max.variation > 0)
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data.column), data.column %in% colnames(data))
  stopifnot(is.character(datetime.column), datetime.column %in% colnames(data))

  Sys.setenv(TZ = "UTC")

  # ensure the right order of date-times in the data.frame
  data = as_tibble(data)
  obs.data = data[[data.column]]    # data[,data.column]
  obs.datetime = data[[datetime.column]]    # data[,datetime.column]
  attr(obs.datetime, 'tzone') = "UTC"
  data = data[order(data[[datetime.column]]),]

  time_series = xts(x = obs.data, order.by = obs.datetime)
  step_dt_seq = tibble(start = obs.datetime - step.duration, end = obs.datetime + step.duration)
  step_dt_seq = step_dt_seq[order(step_dt_seq$start),]
  step_dt_label = str_c(step_dt_seq$start,'/',step_dt_seq$end)

  variation.data = unlist( lapply(step_dt_label, FUN = function(x){
    ts_interval = time_series[x]
    diff.value = ifelse(sum(!is.na(ts_interval)) > 1,
                        max(ts_interval, na.rm = TRUE) - min(ts_interval, na.rm = TRUE),
                        NA)
    return(diff.value)
  }) )

  center.data = as.numeric(coredata(time_series))

  lag.data = dplyr::lag(center.data)
  lead.data = dplyr::lead(center.data)

  lag.diff.data = abs(center.data - lag.data)
  lead.diff.data = abs(center.data - lead.data)

  label.center.na = which(is.na(center.data)) # flag - NA, no observaions
  # label.center.isolate = which(!is.na(center.data) & is.na(lag.data) & is.na(lead.data))
  label.center.isolate = which(!is.na(center.data) & is.na(variation.data)) # flag - isolate, isolated observations
  label.center.fail.left = which(!is.na(lag.data) & !is.na(center.data) &
                                   variation.data > max.variation &
                                   center.data > lag.data &
                                   lag.diff.data > max.variation) # flag - fail.step, failed observations
  label.center.fail.right = which(!is.na(lead.data) & !is.na(center.data) &
                                    variation.data > max.variation &
                                    center.data > lead.data &
                                    lead.diff.data > max.variation)# flag - fail.step, failed observations

  # check data between left step failed and right step failed data;
  # to identify observations when a wind sensor is stucked and keeps report non-changeable high speed.
  label.center.fail.between.left = c()
  for (x.left in label.center.fail.left) {
    for (i in 1:min(300, length(lag.diff.data)-x.left) ) {
      # set 300 because the data to be checked is limited in the most recent times (approx 5 hours)
      if (lag.diff.data[x.left + i] < 0.1) {
        fail.between = x.left + i
      } else {
        break
      }
      label.center.fail.between.left = append(label.center.fail.between.left,fail.between)
    }
  }
  label.center.fail.between.right = c()
  for (x.right in setdiff(label.center.fail.right,1)) {
    for (i in 1:min(300, x.right-1) ) {
      # set 300 because the data to be checked is limited in the most recent times (approx 5 hours)
      if (lead.diff.data[x.right - i] < 0.1) {
        fail.between = x.right - i
      } else {
        break
      }
      label.center.fail.between.right = append(label.center.fail.between.right,fail.between)
    }
  }
  label.center.fail.between = union(label.center.fail.between.left,
                                    label.center.fail.between.right) # flag - fail.step, failed observations

  output_flag = data %>% mutate(flag_step = "P")
  output_flag$flag_step[label.center.isolate] = "isolated"
  output_flag$flag_step[label.center.fail.between] = "fail.step"
  output_flag$flag_step[label.center.fail.left] = "fail.step"
  output_flag$flag_step[label.center.fail.right] = "fail.step"
  output_flag$flag_step[label.center.na] = "missing"

  output_data = output_flag %>%
    mutate(new_data_step = ifelse( flag_step == "fail.step", NA, center.data ) )

  attr(output_data, 'step_pass_percent') = sum(!is.na(output_data$new_data_step)) /
    sum(!is.na(obs.data)) * 100

  return(output_data)
}
