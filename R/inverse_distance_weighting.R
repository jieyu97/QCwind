
















# inverse_distance_weighting = function(obs.data, ref.data, weights)
# {
#   lapply(1:length(obs.data), FUN = function(t){
#     nbhd_infomation = ref.data[t,]
#     nbhd_weights = weights[t,]
#     if ( sum(!is.na(nbhd_infomation)) < 3 ) {
#       estimation_idw_emd[t] = NA
#       sd_idw_emd[t] = NA
#     } else {
#       estimation_idw_emd[t] = sum( nbhd_infomation * nbhd_weights, na.rm = TRUE ) /
#         sum( nbhd_weights[!is.na(nbhd_infomation)] )
#       sd_idw_emd[t] = sd( nbhd_infomation, na.rm = TRUE )
#     }
#   })
#
#
#   flag_after_emd = ifelse( observation > (estimation_idw_emd + 2*sd_idw_emd) |
#                              observation < (estimation_idw_emd - 2*sd_idw_emd),
#                            1, 0 )
#   obs_after_emd = ifelse( observation > (estimation_idw_emd + 2*sd_idw_emd) |
#                             observation < (estimation_idw_emd - 2*sd_idw_emd),
#                           NA, observation )
# }
