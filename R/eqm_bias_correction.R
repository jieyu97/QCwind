#' @name eqm_bias_correction
#' @title Calibrating the bias of observed data by empirical quantile mapping method
#' @description Performing empirical quantile distribution mapping to correct bias in the observation data, 
#' the "truth" quantiles are given as reference data.
#' @param train.obs a numeric vector of training observed data to build the bias correction model.
#' @param true.quantiles a numeric vector of the "truth" quantiles to build the bias correction model,
#' the corresponding cumulative probability stamps must be evenly spreaded between 0 and 1. If not satisfied,
#' a pseudo "truth" observation numeric vector satisfying the quantiles should be provided.
#' We suggest using quantiles with cumulative probability from 0.01 to 1 with interval length 0.01 
#' (100 quantiles in total). 
#' We use Weibull distribution with maximum likelihood estimation to fit these quantiles (in R package fitdistrplus).
#' @param test.obs a numeric vector of test observed data to be calibrated, could be the same as `train.obs`.
#' @return a numeric vector that contains the corrected `test_after_bc` for the test data.
#' @export
#' @examples
#' bc_train_obs = c(0:8,0,1,2,3,4,5,0,2,3,1,9)
#' bc_true_quantiles = c(0.1,1,3,5,7,8,9,9.1,9.2,9.3)
#' bc_test_obs = c(5,6,7)
#' bc_test_corrected = bias_correction(bc_train_obs, bc_test_obs, bc_true_quantiles)

bias_correction <- function(train.obs, test.obs, true.quantiles)
{
  require(fitdistrplus)
  stopifnot(is.numeric(train.obs), is.numeric(test.obs), is.numeric(true.quantiles))

  train_ecdf = ecdf(train.obs)
  test_cumu.prob = train_ecdf(test.obs)
  true_smooth_weibull = fitdistrplus::fitdist(true.quantiles,'weibull',method = "mle")
  correct_model = quantile(true_smooth_weibull, probs = test_cumu.prob)
  test_after_bc = t(correct_model$quantiles)
  
  return(test_after_bc)
}
