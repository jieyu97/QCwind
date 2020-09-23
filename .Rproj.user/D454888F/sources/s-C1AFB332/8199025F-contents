#' @name range_check
#' @title Range check function
#' @description Checking the plausible range of a sequence of observations;
#' The observations should be no less than the `lower.bound` and no more than the `upper.bound`;
#' Generating a new sequence of flags, where a `P` means that observation passes the range check,
#' and a pre-defined flag name `fail.flag` means that observation fails the range check;
#' Also outputting a new sequence of observations after removing failed observations.
#' @param data a data.frame that includes observation data with information.
#' @param column a character, the selected column name in the data.frame that
#' represents observation data to be checked.
#' @param upper.bound a real number that controls the maximum allowed value of observations.
#' @param lower.bound a real number that controls the minimum required value of observations (default value = 0).
#' @param fail.flag a character/string that represents the name of flag where an observation fails the range check.
#' @return a new tbl_df that extends two new columns on the input data,
#' the first added column `flag` represents the flags (`P` means pass, `fail.flag` means fail,
#' `missing` means missing values of observations), the second added column `new_data` represents
#' the observation data after removing failed observations.
#' @export
#' @examples
# library(tidyverse)
# datetime = as.POSIXlt(seq(0,60000,600), origin = "2017-02-03 08:00:00")
# test = tibble(datetime = datetime,
#               windspeed = c(0:20,20:1,1:20,20:1,1:20))
# test_range_check = range_check(test, column = 'windspeed', upper.bound = 18, fail.flag = 'RS')
# attributes(test_range_check)
# test_range_check

range_check <- function(data, column, upper.bound, lower.bound = 0, fail.flag)
{
  require(tidyverse)
  stopifnot(upper.bound > lower.bound)
  stopifnot(is.character(fail.flag))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(column), column %in% colnames(data))

  # # for test
  # data = wow_test
  # column = 'windspeed_metrepersecond'
  # upper.bound = 25
  # lower.bound = 0
  # fail.flag = 'RS'

  data = as.data.frame(data)
  obs.data = data[[column]]    # obs.data = data[,column]
  output_data = data %>%
    mutate(flag_range = if_else( (obs.data <= upper.bound & obs.data >= lower.bound), 'P', fail.flag, 'missing') ) %>%
    mutate(new_data_range = ifelse( (obs.data <= upper.bound & obs.data >= lower.bound), obs.data, NA) )

  output_data = as_tibble(output_data)

  attr(output_data, 'input_valid_data_percentage') = sum(!is.na(obs.data)) / length(obs.data)
  attr(output_data, 'pass_percentage') = sum(!is.na(output_data[['new_data_range']])) /
    sum(!is.na(output_data[[column]]))

  return(output_data)
}
