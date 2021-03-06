#' @name neighbors_selection
#' @title Select suitable neighboring stations to do spatial quality control
#' @description Choosing neighboring stations mainly based on the similarities
#' (both Pearson correlation and Earth mover's distance0) between wind speed
#' distributions of the input data; Also get the optimal order of candidate
#' stations to do spatial quality control.
#' @param data a data.frame of observations by different stations, each
#' column represents a station, and the rows represent standard recording datetimes.
#' @param ref.data a data.frame that has the same settings with `data`; the stations
#' in `ref.data` are official stations used for reference.
#' @param geo.distance `TURE`/`FALSE`; The default is `FALSE`, which means the neighbors
#' selection function do not consider geographical distances between stations; If setting
#' `TRUE`, the longitude and latitude information of stations in `data` and `ref.data`
#' should be provided to calculate the geographical distances.
#' @return a list and a vector; In the list, each element contains the six choosen
#' neighboring stations from either `data` or `ref.data` for a candidate station in `data`;
#' The vector indicated the best order of stations in `data` to do spatial quality control,
#' a station that has more official stations in the neighboring list
#' for reference would be checked first in the spatial quality control.
#' @export
#' @examples
#' wow_nbhd = neighbors_selection(wow_ws, knmi_ws)[[1]]
#' wow_order = neighbors_selection(wow_ws, knmi_ws)[[2]]

neighbors_selection = function(data, ref.data, geo.distance = FALSE)
{


  ########### find nbhd stations by distance
  spatial_distance = distm(x = cbind(spatial_information$longitude,
                                     spatial_information$latitude), fun = distGeo)
  spatial_information = cbind(spatial_information,1,1)
  colnames(spatial_information) = c('longitude','latitude','station_no','w0k1')
  spatial_information$station_no = c(1:39,1:12)
  spatial_information$w0k1[1:39] = 0

  nbhd_geo = which(spatial_distance[,15] <= 35000 & spatial_distance[,15] != 0) # test

  ### select neighboring stations
  radius = 50000 # such that each WOW has at least 10 nbhd stations, including both WOW and KNMI.
  spatial_nbhd_info = matrix(NA, nrow = 39, ncol = 4)
  colnames(spatial_nbhd_info) = c('nbhd_numbers','nbhd_knmi_numbers','WOW_id','order_score')
  spatial_nbhd_info[,3] = 1:39
  spatial_corr = cor(as.matrix(spatial_ws), use = 'pairwise.complete.obs', method = "pearson")
  corr_min = 0.55
  which(spatial_corr[,15] >= corr_min)
  for (i in 1:39 ) {
    # label_nbhd_dist = which(spatial_distance[,i] <= radius & spatial_distance[,i] != 0)
    label_nbhd_corr = which(spatial_corr[,i] > corr_min & spatial_corr[,i] != 1)
    # label_nbhd = intersect(label_nbhd_dist,label_nbhd_corr)
    spatial_nbhd_info[i,1] = length(label_nbhd_corr)
    spatial_nbhd_info[i,2] = sum(spatial_information$w0k1[label_nbhd_corr])
    spatial_nbhd_info[i,4] = spatial_nbhd_info[i,2]
  }
  # spatial_nbhd_info_sort = spatial_nbhd_info[order(spatial_nbhd_info[,2], decreasing = TRUE), ]
  spatial_nbhd_info_sort = spatial_nbhd_info[order(spatial_nbhd_info[,1], decreasing = TRUE), ]
  spatial_nbhd_info_order = spatial_nbhd_info_sort[order(spatial_nbhd_info_sort[,2], decreasing = TRUE), ]

  ####
  ## get wow_station_order, the right order for spatial QC.
  wow_station_order = 1:39
  wow_station_order[1] = spatial_nbhd_info_order[1,3]
  for (i in 1:(39-2) ) {
    spatial_nbhd_new_order = spatial_nbhd_info_order[2:nrow(spatial_nbhd_info_order),]
    station_this_round = wow_station_order[i]
    ### This round spatial QC, candidate station is No. station_this_round
    ### After finish this round:
    new_order_nbhd = which(spatial_distance[1:25, station_this_round] <= radius &
                             spatial_distance[1:25, station_this_round] != 0)
    new_order_no = which(spatial_nbhd_new_order[,3] %in% new_order_nbhd)
    spatial_nbhd_new_order[new_order_no,4] = spatial_nbhd_new_order[new_order_no,4] + 1

    spatial_nbhd_info_order = spatial_nbhd_new_order[order(spatial_nbhd_new_order[,1], decreasing = TRUE), ]
    wow_station_order[i+1] = spatial_nbhd_info_order[1,3]
  }
  wow_station_order[39] = spatial_nbhd_info_order[2,3]
  # get order:
  # wow_station_order:  18  1  4 23 25  2 24 31 12 39 32 19 21 13 34  6  7 26 22 28
  # 17  8  9 37 30 20 33 16 38 27  5 36 15  3 29 11 14 10 35

  # test/check neighbors:
  for (i in wow_station_order ) {
    print(i)
    a = sum( !is.na(spatial_ws[,i]) )
    print(a/157825)
  }

  ####
  # find neighbors by WOW order:
  wow_spatial_nbhd = c()
  for (k in 1:38 ) {
    i = wow_station_order[k]
    label_nbhd = which(spatial_distance[,i] <= radius & spatial_distance[,i] != 0)
    # completeness of nbhd observations
    for (j in 1:length(label_nbhd) ) {
      p = label_nbhd[j]
      a = sum( !is.na(spatial_ws[,i]) )
      b = sum( !is.na(spatial_ws[,i]) & !is.na(spatial_ws[,p]) )
      if (b/a < 0.8) { label_nbhd[j] = NA }
    }
    label_nbhd = label_nbhd[!is.na(label_nbhd)]
    # similarity, nbhd ordering
    similar_nbhd_info = matrix(NA, nrow = length(label_nbhd), ncol = 4)
    colnames(similar_nbhd_info) = c('nbhd_label','w0k1','emd','correlation')
    similar_nbhd_info[,1] = label_nbhd
    similar_nbhd_info[,2] = spatial_information$w0k1[label_nbhd]
    for (j in 1:length(label_nbhd) ) {
      laws = which(!is.na(spatial_ws[,i]) & !is.na(spatial_ws[,j]))
      similar_nbhd_info[j,3] = sum( abs( sort(spatial_ws[laws,i]) - sort(spatial_ws[laws,j]) ) ) / length(laws)
      # similar_nbhd_info[j,3] = sqrt(mean((spatial_ws[,i] - spatial_ws[,label_nbhd[j]])^2, na.rm = TRUE))
      similar_nbhd_info[j,4] = cor(spatial_ws[,i], spatial_ws[,label_nbhd[j]],
                                   use = 'pairwise.complete.obs', method = 'pearson')
    }
    similar_nbhd_info_sort = similar_nbhd_info[which(similar_nbhd_info[,4] >= 0.5),]
    similar_nbhd_info_order = similar_nbhd_info_sort[order(similar_nbhd_info_sort[,3]),]
    wow_spatial_nbhd[[k]] = similar_nbhd_info_order[1:6,1]
  }

  return(spatial_nbhd)
}


