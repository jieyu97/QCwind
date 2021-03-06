#' @name range_check
#' @title Range check function
#' @description Checking the plausible range of a sequence of observations;
#' The observations should be no less than the `lower.bound` and no more than the `upper.bound`;
#' Generating a new sequence of flags, and outputting a new sequence of observations after removing failed observations.
#' @param data a data.frame that includes observation data with information.
#' @param data.column a character, the selected column name in the data.frame that
#' represents observation data to be checked.
#' @param datetime.column a character, the selected column name in the data.frame that
#' represents the reporting dates and times of corresponding observations.
#' @param upper.bound a real number that controls the maximum allowed value of observations.
#' @param lower.bound a real number that controls the minimum required value of observations (default value = 0).
#' @param monthly.bounds a logical, if `TRUE`, then the range check is performed with different monthly upper bounds,
#' in this case the `upper.bound` and `lower.bound` must be a numeric vector of length 12,
#' if `lower.bound` use default value, then there is no need to take actions.
#' @return a new tbl_df that extends two new columns on the input data,
#' the first added column `flag_range` represents the flags (`P` means pass, `fail.range` means fail,
#' `missing` means missing values of observations), the second added column `new_data_range` represents
#' the observation data after removing failed observations.
#' @import tidyverse
#' @importFrom dplyr tibble %>% mutate if_else as_tibble
#' @importFrom lubridate month
#' @export
#' @examples
#' library(tidyverse)
#' datetime = as.POSIXct(seq(0,60000,600), origin = "2017-02-03 08:00:00")
#' test_data = tibble(datetime = datetime,
#'               windspeed = c(0:20,20:1,1:20,20:1,1:20))
#' test_range_check = range_check(test_data, data.column = 'windspeed',
#'                                datetime.column = 'datetime', upper.bound = c(11:22))
#' attributes(test_range_check)
#' test_range_check

range_check <- function(data, data.column, datetime.column,
                        upper.bound, lower.bound = 0, monthly.bounds = TRUE)
{
  stopifnot(is.numeric(upper.bound), is.numeric(lower.bound))
  stopifnot(upper.bound > lower.bound)
  stopifnot(is.logical(monthly.bounds))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(data.column), data.column %in% colnames(data))
  stopifnot(is.character(datetime.column), datetime.column %in% colnames(data))

  data = as.data.frame(data)
  obs.data = data[[data.column]]    # obs.data = data[,data.column]

  if (isTRUE(monthly.bounds)) {
    # pre check
    stopifnot(length(upper.bound) == 12)
    if (lower.bound == 0) {
      lower.bound = rep(0,12)
    }
    stopifnot(length(lower.bound) == 12)
    ##
    obs.month = lubridate::month(data[[datetime.column]])
    output_data0 = data %>%
      mutate(flag_range = NA )
    for (month in 1:12) {
      month.label = which(obs.month == month)
      if (length(month.label) > 0) {
        output_data0$flag_range[month.label] = if_else( (obs.data[month.label] <= upper.bound[month] &
                                                          obs.data[month.label] >= lower.bound[month]),
                                                       'P', 'fail.range', 'missing')
      }
    }
    output_data = output_data0 %>%
      mutate(new_data_range = ifelse( flag_range == 'fail.range', NA, obs.data) )
  } else {
    # pre check
    stopifnot(length(upper.bound) == 1)
    stopifnot(length(lower.bound) == 1)
    ##
    output_data = data %>%
      mutate(flag_range = if_else( (obs.data <= upper.bound & obs.data >= lower.bound), 'P', 'fail.range', 'missing') ) %>%
      mutate(new_data_range = ifelse( flag_range == 'fail.range', NA, obs.data) )
  }

  output_data = as_tibble(output_data)

  attr(output_data, 'range_pass_percent') = sum(!is.na(output_data$new_data_range)) /
    sum(!is.na(obs.data)) * 100

  return(output_data)
}
