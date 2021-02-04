#' @name estimate_zeros
#' @title Estimating zeros
#' @description Provide estimates of zero observations cause by systematic under-estimation, using
#' inverse Earth mover's distance weighting method for interpolations.
#' @param test_windspeed.afterSpatialQC.nonzero a data.frame that includes observation data after spatial
#' consistency check, with no zeros.
#' @param test.station.windspeed a data.frame that includes observation data of test stations before
#' spatial consistency check.
#' @param official.station.windspeed a data.frame that includes observation data of official stations.
#' @param test.nbhd_spatial.info output of function `spatial_neighbors`.
#' @param official.data.estimate a logical variable that determines whether to include CWS data in
#' estimating zeros.
#' We set the default as `TRUE`, meaning that we use only official data in the estimation.
#' If there are not enough neighboring official stations, we could then set `official.data.estimate = FALSE`
#' to use both official and CWS data for the estimation.
#' @return a new data.frame that includes wind speed observations after estimating zeros.
#' @import tidyverse
#' @importFrom dplyr %>%
#' @export

estimate_zeros = function(test_windspeed.afterSpatialQC.nonzero,
                          test.station.windspeed,
                          official.station.windspeed,
                          test.nbhd_spatial.info,
                          official.data.estimate = TRUE)
{
  stopifnot(nrow(test_windspeed.afterSpatialQC.nonzero) == nrow(test.station.windspeed))
  stopifnot(ncol(test_windspeed.afterSpatialQC.nonzero) == ncol(test.station.windspeed))
  stopifnot(nrow(test_windspeed.afterSpatialQC.nonzero) == nrow(official.station.windspeed))
  stopifnot(ncol(test_windspeed.afterSpatialQC.nonzero) == length(test.nbhd_spatial.info))

  test_windspeed.zeroEstimate = test_windspeed.afterSpatialQC.nonzero


  if(official.data.estimate) {
    # use official data only for the interpolation

    for (w in 1:ncol(test_windspeed.zeroEstimate)) {

      zero.label = which(test.station.windspeed[,w] == 0)

      test_nbhd.all = test.nbhd_spatial.info[[w]]

      nbhd.label.official = test_nbhd.all$column_number[which(test_nbhd.all$type == 'official')]

      if(length(nbhd.label.official) > 1) {
        # if there are more than one official neighbor station, then estimate zeros by interpolation
        nbhd.windspeed.official = official.station.windspeed[,nbhd.label.official]

        # estimation of zero, use only official data
        estimate.zero = apply(nbhd.windspeed.official, 1, function(x){
          if (sum(!is.na(x)) > 2) {
            official.weights = test_nbhd.all$iemd.weight[which(test_nbhd.all$type == 'official')]
            estimate = sum( x * official.weights, na.rm = TRUE ) /
              sum( official.weights[!is.na(x)] )
          } else {
            estimate = NA
          }
          return(estimate)
        })
      } else {
        # if there is only one official neighbor station, then the estimate cannot be made, return NA instead
        estimate.zero = rep(NA,nrow(test_windspeed.zeroEstimate))
      }

      test_single_before = test.station.windspeed[,w]
      test_nonzero_label = which(test_single_before != 0)
      BCestimate.zero.upper_bound = ceiling(min(test_single_before[test_nonzero_label]))
      good.estimate.zero.label = which(estimate.zero[zero.label] <= BCestimate.zero.upper_bound)

      test_windspeed.zeroEstimate[zero.label,w] = NA
      test_windspeed.zeroEstimate[zero.label[good.estimate.zero.label],w] =
        estimate.zero[zero.label[good.estimate.zero.label]]

    }

  } else {
    # use both official and CWS data for the interpolation

    for (w in 1:ncol(test_windspeed.zeroEstimate)) {

      zero.label = which(test.station.windspeed[,w] == 0)

      test_nbhd.all = test.nbhd_spatial.info[[w]]

      nbhd.label.official = test_nbhd.all$column_number[which(test_nbhd.all$type == 'official')]
      nbhd.label.test = test_nbhd.all$column_number[which(test_nbhd.all$type == 'test')]

      nbhd.windspeed.official = official.station.windspeed[,nbhd.label.official]
      nbhd.windspeed.test = test_windspeed.afterSpatialQC.nonzero[,nbhd.label.test]

      # estimation of zero by interpolation, use both official and CWS data
      nbhd.windspeed = cbind(nbhd.windspeed.official, nbhd.windspeed.test) %>% as.data.frame()

      estimate.zero = apply(nbhd.windspeed, 1, function(x){
        if (sum(!is.na(x)) > 2) {
          official.weights = test_nbhd.all$iemd.weight[which(test_nbhd.all$type == 'official')]
          test.weights = test_nbhd.all$iemd.weight[which(test_nbhd.all$type == 'test')]
          weights = c(official.weights, test.weights)

          estimate = sum( x * weights, na.rm = TRUE ) /
            sum( weights[!is.na(x)] )
        } else {
          estimate = NA
        }
        return(estimate)
      })


      test_single_before = test.station.windspeed[,w]
      test_nonzero_label = which(test_single_before != 0)
      BCestimate.zero.upper_bound = ceiling(min(test_single_before[test_nonzero_label]))
      good.estimate.zero.label = which(estimate.zero[zero.label] <= BCestimate.zero.upper_bound)

      test_windspeed.zeroEstimate[zero.label,w] = NA
      test_windspeed.zeroEstimate[zero.label[good.estimate.zero.label],w] =
        estimate.zero[zero.label[good.estimate.zero.label]]

    }

  }

  return(test_windspeed.zeroEstimate)
}

