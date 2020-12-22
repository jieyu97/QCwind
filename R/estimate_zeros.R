####### function 4: correct zeros

# input
test.station.windspeed = wow_windspeed.zero.final
official.station.windspeed = knmi_windspeed
test_windspeed.afterSpatialQC.nonzero = wow_windspeed.afterSpatialQC.nonzero

estimate_zeros = function(test_windspeed.afterSpatialQC.nonzero,
                          test.station.windspeed,
                          official.station.windspeed,
                          test.nbhd_spatial.info)
{

  test_windspeed.zeroEstimate = test_windspeed.afterSpatialQC.nonzero

  for (w in 1:ncol(test_windspeed.zeroEstimate)) {

    zero.label = which(test.station.windspeed[,w] == 0)

    test_nbhd.all = test.nbhd_spatial.info[[w]]

    nbhd.label.official = test_nbhd.all$column_number[which(test_nbhd.all$type == 'official')]
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

    test_single_before = test.station.windspeed[,w]
    test_nonzero_label = which(test_single_before != 0)
    BCestimate.zero.upper_bound = ceiling(min(test_single_before[test_nonzero_label]))
    good.estimate.zero.label = which(estimate.zero[zero.label] <= BCestimate.zero.upper_bound)

    test_windspeed.zeroEstimate[zero.label,w] = NA
    test_windspeed.zeroEstimate[zero.label[good.estimate.zero.label],w] =
      estimate.zero[zero.label[good.estimate.zero.label]]

  }

  return(test_windspeed.zeroEstimate)
}

