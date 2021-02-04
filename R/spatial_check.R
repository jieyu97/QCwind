#' @name spatial_check
#' @title Spatial consistency check using inverse Earth mover's distance weighting method
#' @description Flag and reject inconsistent observations in space.
#' @param test.station.windspeed.nonzero a data.frame that includes observation data of all test
#' stations, with no zeros.
#' @param official.station.windspeed a data.frame that includes observation data of official stations.
#' @param test.nbhd_spatial.info output of function `spatial_neighbors`.
#' @param order_spatial.test_station output of function `spatial_order`.
#' @return a large list of three data.frames:
#' new_windspeed: observations after filtering spatially inconsistent ones;
#' flags: flag of each observation;
#' iemdw_estimates: inverse Earth mover's distance weighting estimates of each observation.
#' @importFrom truncnorm qtruncnorm
#' @import tidyverse
#' @importFrom dplyr tibble
#' @export

spatial_check = function(test.station.windspeed.nonzero, official.station.windspeed,
                         test.nbhd_spatial.info, order_spatial.test_station)
{

  test_windspeed.afterSpatialQC.nonzero = test.station.windspeed.nonzero
  test_flags.afterSpatialQC.nonzero = test.station.windspeed.nonzero
  test_estimates.afterSpatialQC.nonzero = test.station.windspeed.nonzero

  for (w in order_spatial.test_station) {

    test_single.windspeed.nonzero = test.station.windspeed.nonzero[,w]
    test_nbhd.all = test.nbhd_spatial.info[[w]]

    nbhd.label.test = test_nbhd.all$column_number[which(test_nbhd.all$type == 'test')]
    nbhd.windspeed.test = test.station.windspeed.nonzero[,nbhd.label.test]
    nbhd.label.official = test_nbhd.all$column_number[which(test_nbhd.all$type == 'official')]
    nbhd.windspeed.official = official.station.windspeed[,nbhd.label.official]

    nbhd.data.nonzero = cbind(nbhd.windspeed.test, nbhd.windspeed.official)

    # estimation - nonzero
    estimate.idw.nonzero = apply(nbhd.data.nonzero, 1, function(x){
      sum( x * test_nbhd.all$iemd.weight, na.rm = TRUE ) /
        sum( test_nbhd.all$iemd.weight[!is.na(x)] )
    })

    # SD of nbhd observations - nonzero
    sd.idw.nonzero = apply(nbhd.data.nonzero, 1, function(x){
      sd(x,na.rm = TRUE)
    })

    # isolated flags: less than two nbhd observations
    label.isolated = which(is.na(sd.idw.nonzero))

    estimate.idw.nonzero[label.isolated] = NA

    # estimate CI
    estimate.truncnorm.ci = tibble(
      lower.bound.95ci = qtruncnorm(0.025, a=0, b=Inf, mean = estimate.idw.nonzero, sd = sd.idw.nonzero),
      upper.bound.95ci = qtruncnorm(0.975, a=0, b=Inf, mean = estimate.idw.nonzero, sd = sd.idw.nonzero),
      lower.bound.99ci = qtruncnorm(0.005, a=0, b=Inf, mean = estimate.idw.nonzero, sd = sd.idw.nonzero),
      upper.bound.99ci = qtruncnorm(0.995, a=0, b=Inf, mean = estimate.idw.nonzero, sd = sd.idw.nonzero)
    )
    estimate.truncnorm.ci$lower.bound.95ci[label.isolated] = NA
    estimate.truncnorm.ci$upper.bound.95ci[label.isolated] = NA
    estimate.truncnorm.ci$lower.bound.99ci[label.isolated] = NA
    estimate.truncnorm.ci$upper.bound.99ci[label.isolated] = NA

    # new data and flags after spatial qc
    test_single.windspeed.after = test_single.windspeed.nonzero
    test_single.flags.after = rep(NA,length(test_single.windspeed.nonzero))

    nonzero.label = which(!is.na(test_single.windspeed.nonzero)) # valid.label
    test_single.flags.after[nonzero.label] = 'pass'

    # failed observations
    fail.left = which(test_single.windspeed.nonzero < estimate.truncnorm.ci$lower.bound.99ci)
    fail.right = which(test_single.windspeed.nonzero > estimate.truncnorm.ci$upper.bound.99ci)
    fail.test = union(fail.left,fail.right)

    fail.left.95ci = which(test_single.windspeed.nonzero < estimate.truncnorm.ci$lower.bound.95ci)
    fail.right.95ci = which(test_single.windspeed.nonzero > estimate.truncnorm.ci$upper.bound.95ci)
    fail.95ci = union(fail.left.95ci,fail.right.95ci)

    test_single.windspeed.after[fail.test] = NA

    test_single.flags.after[intersect(nonzero.label, fail.95ci)] = 'fail.spatial.95ci'
    test_single.flags.after[intersect(nonzero.label, fail.test)] = 'fail.spatial.99ci'
    test_single.flags.after[intersect(nonzero.label, label.isolated)] = 'isolated'

    # wrap all
    test_windspeed.afterSpatialQC.nonzero[,w] = test_single.windspeed.after
    test_flags.afterSpatialQC.nonzero[,w] = test_single.flags.after
    test_estimates.afterSpatialQC.nonzero[,w] = estimate.idw.nonzero

    # update test windspeed data
    test.station.windspeed.nonzero[,w] = test_single.windspeed.after

  }

  # combine the three data.frames
  test_station.afterSpatialQC = list(new_windspeed = test_windspeed.afterSpatialQC.nonzero,
                                     flags = test_flags.afterSpatialQC.nonzero,
                                     iemdw_estimates = test_estimates.afterSpatialQC.nonzero)

  return(test_station.afterSpatialQC)
}


