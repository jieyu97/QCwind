#' @name temporal_step_check.new
#' @title Temporal step check function (fast version, for 10-min observations time series only)
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

temporal_step_check.new = function (data, data.column, datetime.column, step.duration,
                                    max.variation)
{
  stopifnot(step.duration == 600)
  stopifnot(max.variation > 0)
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data.column), data.column %in% colnames(data))
  stopifnot(is.character(datetime.column), datetime.column %in%
              colnames(data))

  data = as_tibble(data)
  obs.data = data[[data.column]]

  center.data = obs.data
  lag.data = dplyr::lag(center.data)
  lead.data = dplyr::lead(center.data)
  lag.diff.data = abs(center.data - lag.data)
  lead.diff.data = abs(center.data - lead.data)

  label.center.na = which(is.na(center.data))
  label.center.isolate = which(!is.na(center.data) & is.na(lag.data) & is.na(lead.data))
  label.center.fail.left = which(!is.na(lag.data) & !is.na(center.data) &
                                   center.data > lag.data &
                                   lag.diff.data > max.variation)
  label.center.fail.right = which(!is.na(lead.data) & !is.na(center.data) &
                                    center.data > lead.data &
                                    lead.diff.data > max.variation)
  label.center.fail.between.left = c()
  for (x.left in label.center.fail.left) {
    for (i in 1:min(300, length(lag.diff.data) - x.left)) {
      if ( !is.na(lag.diff.data[x.left + i]) & (lag.diff.data[x.left + i] < 0.1) ) {
        fail.between = x.left + i
      }
      else if ( !is.na(lag.diff.data[x.left + i]) & (lag.diff.data[x.left + i] >= 0.1) ) {
        break
      } else {
        fail.between = NULL
      }
      label.center.fail.between.left = append(label.center.fail.between.left,
                                              fail.between)
    }
  }
  label.center.fail.between.right = c()
  for (x.right in setdiff(label.center.fail.right, 1)) {
    for (i in 1:min(300, x.right - 1)) {
      if ( !is.na(lead.diff.data[x.right + i]) & (lead.diff.data[x.right + i] < 0.1) ) {
        fail.between = x.right - i
      }
      else if ( !is.na(lead.diff.data[x.right + i]) & (lead.diff.data[x.right + i] >= 0.1) ) {
        break
      } else {
        fail.between = NULL
      }
      label.center.fail.between.right = append(label.center.fail.between.right,
                                               fail.between)
    }
  }
  label.center.fail.between = union(label.center.fail.between.left,
                                    label.center.fail.between.right)
  output_flag = data %>% mutate(flag_step = "P")
  output_flag$flag_step[label.center.isolate] = "isolated"
  output_flag$flag_step[label.center.fail.between] = "fail.step"
  output_flag$flag_step[label.center.fail.left] = "fail.step"
  output_flag$flag_step[label.center.fail.right] = "fail.step"
  output_flag$flag_step[label.center.na] = "missing"
  output_data = output_flag %>% mutate(new_data_step =
                                         ifelse(flag_step == "fail.step", NA, center.data))
  attr(output_data, "step_pass_percent") =
    sum(!is.na(output_data$new_data_step)) / sum(!is.na(obs.data)) * 100

  return(output_data)
}
