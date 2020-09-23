#' @name bias_correction
#' @title Calibrating the bias of observed data
#' @description Using different methods to calibrate the systematic bias between observed
#' and true data.
#' @param train.obs a numeric vector of training observed data to build the bias correction model.
#' @param train.true a numeric vector of training true data to build the bias correction model.
#' @param test.obs a numeric vector of test observed data to be calibrated.
#' @param method a character indicating the choosen bias correction method;
#' `linear_scaling`: the linear scaling method, fitting the relationship between observed and
#' true data by a linear model;
#' `empirical_quantile`: the empirical quantile mapping method, mapping the quantiles between
#' the empirical cumulative density functions of observed and true data.
#' @return a numeric vector that contains the corrected `test.obs` data.
#' @export
#' @examples
# bc_train_obs = c(0:8,9,9,9,9)
# bc_test_obs = c(1:9,10,10,10,10)
# bc_train_true = c(3:11,15,15,15,15)
# bc_test_corrected = bias_correction(bc_train_obs, bc_train_true, bc_test_obs)

bias_correction <- function(train.obs, train.true, test.obs, method = 'empirical_quantile')
{
  require(tidyverse)
  stopifnot(is.numeric(train.obs), is.numeric(train.true), length(train.obs) == length(train.true) )
  stopifnot(is.character(method), method %in% c('linear_scaling', 'empirical_quantile') )

  # # for test
  # train.obs = bc_wow_test
  # train.true = bc_knmi_test
  # method = 'empirical_quantile'

  noNA_label = !is.na(train.obs) & !is.na(train.true)
  obs_data = train.obs[noNA_label]
  true_data = train.true[noNA_label]

  if (method == "linear_scaling")
  {
    mu.obs = mean(obs_data, na.rm = TRUE)
    mu.true = mean(true_data, na.rm = TRUE)
    sigma.obs = sd(obs_data, na.rm = TRUE)
    sigma.true = sd(true_data, na.rm = TRUE)

    bc.function = function(x) {
      (sigma.true/sigma.obs) * (x - mu.obs) + mu.true
    }
  }
  else if (method == "empirical_quantile")
  {
    ecdf.obs = ecdf(obs_data)
    # ecdf.true = ecdf(true_data)
    # ecdf_middle = ecdf.obs(obs_data)
    # wow_test_ecdf = quantile(true_data, ecdf_middle, na.rm = TRUE, names = TRUE, type = 2)

    bc.function = function(x) {
      quantile(true_data, ecdf.obs(x), na.rm = TRUE, names = TRUE, type = 2)
    }
  }

  test.obs.corrected = bc.function(test.obs)
  test.obs.corrected = as.numeric(test.obs.corrected)
  return(test.obs.corrected)
}
