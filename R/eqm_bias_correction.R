#' @name eqm_bias_correction
#' @title Calibrating the bias of observed data by empirical quantile mapping method
#' @description Performing empirical quantile distribution mapping to correct bias in the observation data,
#' the "truth" quantiles are given as reference data.
#' @param train.obs a numeric vector of training observed data to build the bias correction model.
#' @param train.datetime a sequence of timestamps in `train.obs`.
#' @param true.quantiles a list of six numeric vectors of the "truth" quantiles to build the
#' bias correction model, the corresponding cumulative probability stamps must be evenly spreaded
#' between 0 and 1. If not satisfied,
#' a pseudo "truth" observation numeric vector satisfying the quantiles should be provided.
#' We suggest using quantiles with cumulative probability from 0.01 to 1 with interval length 0.01
#' (100 quantiles in total).
#' We use Weibull distribution with maximum likelihood estimation to fit these quantiles
#' (in R package fitdistrplus).
#' @param test.obs a numeric vector of test observed data to be calibrated, could be the same as `train.obs`.
#' @param test.datetime a sequence of timestamps in `test.obs`.
#' @return a numeric vector that contains the corrected `test_after_bc` for the test data.
#' @import stats
#' @import tidyverse
#' @importFrom dplyr tibble
#' @importFrom lubridate month hour
#' @importFrom fitdistrplus fitdist
#' @export

eqm_bias_correction <- function(train.obs, train.datetime,
                            test.obs, test.datetime, true.quantiles)
{
  stopifnot(is.numeric(train.obs), is.numeric(test.obs))

  ## split datetime by season and by day/night time
  train.datetime_split = tibble(datetime = train.datetime,
                          month = month(train.datetime),
                          hour = hour(train.datetime),
                          season = 'summer', # summer: 6,7,8,9; winter: 12,1,2,3; trans: 4,5,10,11;
                          daytime = 'night', # day: 7--18; night: 19--6;
                          class = 'day_s')
  train.datetime_split$season[ which( train.datetime_split$month %in% c(12,1,2,3) ) ] = 'winter'
  train.datetime_split$season[ which( train.datetime_split$month %in% c(4,5,10,11) ) ] = 'trans'
  train.datetime_split$daytime[ which( train.datetime_split$hour %in% c(7:18) ) ] = 'day'
  train.datetime_split$class = paste0(train.datetime_split$season,'_',train.datetime_split$daytime)

  test.datetime_split = tibble(datetime = test.datetime,
                                month = month(test.datetime),
                                hour = hour(test.datetime),
                                season = 'summer', # summer: 6,7,8,9; winter: 12,1,2,3; trans: 4,5,10,11;
                                daytime = 'night', # day: 7--18; night: 19--6;
                                class = 'day_s')
  test.datetime_split$season[ which( test.datetime_split$month %in% c(12,1,2,3) ) ] = 'winter'
  test.datetime_split$season[ which( test.datetime_split$month %in% c(4,5,10,11) ) ] = 'trans'
  test.datetime_split$daytime[ which( test.datetime_split$hour %in% c(7:18) ) ] = 'day'
  test.datetime_split$class = paste0(test.datetime_split$season,'_',test.datetime_split$daytime)

  correct.test.obs = rep(NA, length(test.obs))
  zero_label = which(test.obs == 0)
  split_class = c("winter_night","winter_day","trans_night","trans_day","summer_night","summer_day")

  for (period in 1:6) {
    estimate_true_quantile = true.quantiles[[period]]

    obs_train.period = train.obs[which(train.datetime_split$class == split_class[period])]
    ecdf_train.period = ecdf(obs_train.period)

    obs_test.period = test.obs[which(test.datetime_split$class == split_class[period])]
    obs_cumu_prob.period = ecdf_train.period(obs_test.period)

    kriging_smooth_weibull = fitdist(estimate_true_quantile,'weibull',method = "mle")
    kriging_correct_model = quantile(kriging_smooth_weibull, probs = obs_cumu_prob.period)

    correct.obs_test.period = as.numeric(t(kriging_correct_model$quantiles))
    correct.obs_test.period[which(correct.obs_test.period == Inf)] = max(estimate_true_quantile)
    correct.test.obs[which(test.datetime_split$class == split_class[period])] = correct.obs_test.period
  }

  correct.test.obs[zero_label] = 0

  return(correct.test.obs)
}
