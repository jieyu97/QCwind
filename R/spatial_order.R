#' @name spatial_order
#' @title Implementing order of test stations
#' @description Find an optimal order of test stations when performing the spatial consistency check.
#' @param test_station.scores a data.frame that includes the quality score of test stations.
#' @param test.nbhd_spatial.info output of function `spatial_neighbors`.
#' @return a numeric sequence that shows the order of test stations in their column numbers.
#' @export

spatial_order = function(test_station.scores, test.nbhd_spatial.info)
{

  # function body: get the order to implement spatial qc
  test_station.scores_update = test_station.scores$score

  order_spatial.test_station = c()
  for (order in 1:length(test.nbhd_spatial.info)) {

    test_station.nbhd_score = c()
    for (w in 1:length(test.nbhd_spatial.info)) {
      test_station.nbhd_info = test.nbhd_spatial.info[[w]]
      nbhd.official.number = length(which(test_station.nbhd_info$type == 'official'))
      nbhd.test.column_number = test_station.nbhd_info$column_number[which(test_station.nbhd_info$type == 'test')]
      nbhd.test.score = test_station.scores_update[nbhd.test.column_number]

      nbhd.score.average = mean(c(nbhd.test.score, rep(2,nbhd.official.number)))

      test_station.nbhd_score[w] = nbhd.score.average
    }

    rest_test_station.label = setdiff(c(1:length(test.nbhd_spatial.info)), order_spatial.test_station)

    max.test.label0 = which.max(test_station.nbhd_score[rest_test_station.label])
    max.test.label = rest_test_station.label[max.test.label0]

    test_station.scores_update[max.test.label] = 2

    order_spatial.test_station[order] = max.test.label
  }

  return(order_spatial.test_station)
}

