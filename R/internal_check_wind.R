#' @name internal_check_wind
#' @title Internal consistency check function (specifically for wind observations)
#' @description Checking the internal consistency of wind data, wind speed should be no larger than wind gust;
#' Generating a new sequence of flags, where a `P` means that observation passes the internal check,
#' and a pre-defined flag name `fail.flag` means that observation fails the internal check;
#' Also outputting two new sequences of wind speed and gust observations after removing failed observations.
#' @param data a data.frame that includes observation data with information.
#' @param speed.column a character, the selected column name in the data.frame that represents wind speed data.
#' @param gust.column a character, the selected column name in the data.frame that represents wind gust data.
#' @param fail.flag a character/string that represents the name of flag where an observation fails the internal check.
#' @return a new tbl_df that extends three new columns on the input data,
#' the first added column `flag` represents the flags (`P` means pass, `fail.flag` means fail,
#' `missing` means missing values of observations), the second added column `new_windspeed` represents
#' the wind speed observations after removing failed observations, the third added column
#' `new_windgust` represents the wind gust observations after removing failed observations.
#' @import tidyverse
#' @importFrom dplyr tibble %>% mutate if_else as_tibble
#' @export
#' @examples
#' library(tidyverse)
#' datetime = as.POSIXlt(seq(0,60000,600), origin = "2017-02-03 08:00:00")
#' test = tibble(datetime = datetime,
#'               windspeed = c(0:20,20:1,1:20,20:1,1:20),
#'               windgust = c(1:50,50:0))
#' test_internal_check = internal_check_wind(test, speed.column = 'windspeed',
#'                                           gust.column = 'windgust', fail.flag = 'IN')
#' attributes(test_internal_check)
#' test_internal_check

internal_check_wind <- function(data, speed.column, gust.column, fail.flag)
{
  stopifnot(is.character(fail.flag))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(speed.column), speed.column %in% colnames(data))
  stopifnot(is.character(gust.column), gust.column %in% colnames(data))

  # # for test
  # data = wow_test
  # speed.column = 'windspeed_metrepersecond'
  # gust.column = 'windgust_metrepersecond'
  # fail.flag = 'IN'

  data = as.data.frame(data)
  speed.data = data[[speed.column]]    # data[,speed.column]
  gust.data = data[[gust.column]]    # data[,gust.column]
  output_data = data %>%
    mutate(flag_internal = if_else( speed.data <= gust.data, 'P', fail.flag, 'missing') ) %>%
    mutate(new_windspeed = ifelse( speed.data <= gust.data, speed.data, NA) ) %>%
    mutate(new_windgust = ifelse( speed.data <= gust.data, gust.data, NA) )

  output_data = as_tibble(output_data)

  attr(output_data, 'input_valid_data_percentage') = sum( !is.na(speed.data) &
                                                               !is.na(gust.data) ) / length(speed.data)
  attr(output_data, 'pass_percentage') = sum(!is.na(output_data[['new_windspeed']])) /
    sum( !is.na(speed.data) & !is.na(gust.data) )

  return(output_data)

  # ### Internal consistency check (flag_I1)
  # flag_I1 = mapply(function(a,b) {
  #   ifelse(!is.na(b) & !is.na(a) & b > a, 1, ifelse(!is.na(b), 0, NA))
  # }, windgust, windspeed )
  # rownames(flag_I1) = rownames(windspeed)
  # flag_I1 = as.data.frame(flag_I1)
  # windspeed_after_I1 = mapply(function(a,b) {
  #   ifelse(!is.na(b) & !is.na(a) & b > a, NA, b)
  # }, windgust, windspeed )
  # rownames(windspeed_after_I1) = rownames(windspeed)
  # windspeed_after_I1 = as.data.frame(windspeed_after_I1)
  # windgust_after_I1 = mapply(function(a,b) {
  #   ifelse(!is.na(b) & !is.na(a) & b > a, NA, a)
  # }, windgust, windspeed )
  # rownames(windgust_after_I1) = rownames(windgust)
  # windgust_after_I1 = as.data.frame(windgust_after_I1)
  # # pass percentage:
  # pass_percentage = apply(flag_I1, 2, function(b) {
  #   100 - sum(b, na.rm = TRUE) / sum(!is.na(b)) * 100
  # } )
  # return(list(flag_I1, windspeed_after_I1, windgust_after_I1, pass_percentage))
}








