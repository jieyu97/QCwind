#' @name spatial_neighbors
#' @title Selecting neighbors for each test station
#' @description Selecting neighbors for each test station according to geographical distance,
#' correlation, Earth mover's distance.
#' @param test.station.windspeed a data.frame that includes observation data of all test stations.
#' @param official.station.windspeed a data.frame that includes observation data of official stations.
#' @param test.station.info a tbl_df that includes 'station_id', 'longitude', 'latitude' of test stations,
#' corresponding to their column number in `test.station.windspeed`.
#' @param official.station.info a tbl_df that includes 'location', 'longitude', 'latitude' of official
#' stations, corresponding to their column number in `official.station.windspeed`.
#' @param emd_radius the maximum radius of Earth mover's distance, setting 1 as default.
#' @return a large list, each element is a data.frame that contains information of selected neighbors.
#' @import tidyverse
#' @importFrom dplyr tibble
#' @importFrom transport wasserstein1d
#' @importFrom geosphere distm
#' @export

spatial_neighbors = function(test.station.windspeed, official.station.windspeed,
                             test.station.info, official.station.info,
                             emd_radius = 1)
{
  stopifnot(is.data.frame(test.station.windspeed), is.data.frame(official.station.windspeed))
  stopifnot(nrow(test.station.windspeed) == nrow(official.station.windspeed))
  stopifnot(ncol(test.station.windspeed) == nrow(test.station.info))
  stopifnot(ncol(official.station.windspeed) == nrow(official.station.info))
  stopifnot("longitude" %in% colnames(test.station.info), "latitude" %in% colnames(test.station.info))
  stopifnot("longitude" %in% colnames(official.station.info), "latitude" %in% colnames(official.station.info))

  # function body
  test.nbhd_spatial.info = c()

  for (w in 1:ncol(test.station.windspeed)) {

    geo_distance.official = distm(x = cbind(test.station.info$longitude[w],
                                            test.station.info$latitude[w]),
                                  y = cbind(official.station.info$longitude,
                                            official.station.info$latitude), fun = distGeo)
    geo_distance.test = distm(x = cbind(test.station.info$longitude[w],
                                        test.station.info$latitude[w]),
                              y = cbind(test.station.info$longitude,
                                        test.station.info$latitude), fun = distGeo)
    label.nearby_official = which(geo_distance.official <= 75000)
    label.nearby_test = which(geo_distance.test <= 75000 & geo_distance.test != 0)

    correlation.nearby_official = cor(x = test.station.windspeed[,w], y = official.station.windspeed[,label.nearby_official],
                                      use = 'pairwise.complete.obs', method = "pearson")
    correlation.nearby_test = cor(x = test.station.windspeed[,w], y = test.station.windspeed[,label.nearby_test],
                                  use = 'pairwise.complete.obs', method = "pearson")

    corr7.nearby_official = which(correlation.nearby_official >= 0.7)
    corr6.nearby_official = which(correlation.nearby_official >= 0.6)
    corr5.nearby_official = which(correlation.nearby_official >= 0.5)
    corr7.nearby_test = which(correlation.nearby_test >= 0.7)
    corr6.nearby_test = which(correlation.nearby_test >= 0.6)
    corr5.nearby_test = which(correlation.nearby_test >= 0.5)
    corr7.number = length(corr7.nearby_official) + length(corr7.nearby_test)
    corr6.number = length(corr6.nearby_official) + length(corr6.nearby_test)
    corr5.number = length(corr5.nearby_official) + length(corr5.nearby_test)

    test.single = test.station.windspeed[,w]
    valid.test.single = which(!is.na(test.single) )
    vector.test.single = test.single[valid.test.single]

    if (corr7.number >= 10) {
      nbhd.official.label = label.nearby_official[corr7.nearby_official]
      nbhd.test.label = label.nearby_test[corr7.nearby_test]

      nbhd.official = tibble(column_number = nbhd.official.label,
                             type = 'official',
                             correlation = correlation.nearby_official[corr7.nearby_official],
                             emd = NA,
                             iemd.weight = NA)
      nbhd.test = tibble(column_number = nbhd.test.label,
                         type = 'test',
                         correlation = correlation.nearby_test[corr7.nearby_test],
                         emd = NA,
                         iemd.weight = NA)

    } else if (corr6.number >= 10) {
      nbhd.official.label = label.nearby_official[corr6.nearby_official]
      nbhd.test.label = label.nearby_test[corr6.nearby_test]

      nbhd.official = tibble(column_number = nbhd.official.label,
                             type = 'official',
                             correlation = correlation.nearby_official[corr6.nearby_official],
                             emd = NA,
                             iemd.weight = NA)
      nbhd.test = tibble(column_number = nbhd.test.label,
                         type = 'test',
                         correlation = correlation.nearby_test[corr6.nearby_test],
                         emd = NA,
                         iemd.weight = NA)

    } else if (corr5.number >= 10) {
      nbhd.official.label = label.nearby_official[corr5.nearby_official]
      nbhd.test.label = label.nearby_test[corr5.nearby_test]

      nbhd.official = tibble(column_number = nbhd.official.label,
                             type = 'official',
                             correlation = correlation.nearby_official[corr5.nearby_official],
                             emd = NA,
                             iemd.weight = NA)
      nbhd.test = tibble(column_number = nbhd.test.label,
                         type = 'test',
                         correlation = correlation.nearby_test[corr5.nearby_test],
                         emd = NA,
                         iemd.weight = NA)

    } else {
      # this is a bad wow station
      cat("This WOW station is too bad for analysis:",as.character(test.station.info$station_id[w]),"#",w,".\n")

      nbhd.official.label = NULL
      nbhd.test.label = NULL

      nbhd.official = NULL
      nbhd.test = NULL
    }

    # official stations:
    if(length(nbhd.official.label) > 0){

      # calculate earth mover's distance
      for (n in 1:length(nbhd.official.label)) {
        nbhd.official.single = official.station.windspeed[,nbhd.official.label[n]]
        valid.nbhd.single = which(!is.na(nbhd.official.single) )
        vector.nbhd.single = nbhd.official.single[valid.nbhd.single]

        nbhd.official$emd[n] = wasserstein1d(vector.test.single, vector.nbhd.single)
      }

      # choose emd < 1 (emd_radius)
      emd1.nearby_official = which(nbhd.official$emd < emd_radius)

      final_nbhd.official = nbhd.official[emd1.nearby_official,]

      # get inverse emd weights (Cressman method of inverse weights)
      final_nbhd.official$iemd.weight = ( emd_radius^2 - ( final_nbhd.official$emd )^2 ) /
        ( emd_radius^2 + ( final_nbhd.official$emd )^2 )

    } else {
      final_nbhd.official = NULL
    }

    # test stations:
    if(length(nbhd.test.label) > 0){

      # calculate earth mover's distance
      for (n in 1:length(nbhd.test.label)) {
        nbhd.test.single = test.station.windspeed[,nbhd.test.label[n]]
        valid.nbhd.single = which(!is.na(nbhd.test.single) )
        vector.nbhd.single = nbhd.test.single[valid.nbhd.single]

        nbhd.test$emd[n] = wasserstein1d(vector.test.single, vector.nbhd.single)
      }

      # choose emd < 1 (emd_radius)
      emd1.nearby_test = which(nbhd.test$emd < emd_radius)

      final_nbhd.test = nbhd.test[emd1.nearby_test,]

      # get inverse emd weights (Cressman method of inverse weights)
      final_nbhd.test$iemd.weight = ( emd_radius^2 - ( final_nbhd.test$emd )^2 ) /
        ( emd_radius^2 + ( final_nbhd.test$emd )^2 )

    } else {
      final_nbhd.test = NULL
    }


    final_nbhd.info = rbind(final_nbhd.test,final_nbhd.official)

    test.nbhd_spatial.info[[w]] = final_nbhd.info
  } # takes about 10min in total

  return(test.nbhd_spatial.info)
}


