

interpolate_quantiles = function(kriging.reference, station.locat.xy)
{
  # season_knmi_kriging = knmi_kriging_all.season[[season]]
  season_knmi_kriging = kriging.reference
  interpolate_quantiles = tibble(interpolate_x = 1:100,
                                 interpolate_y = 1:100,
                                 quantile_value = NA)
  wow_bc39_locate_xy_df = station.locat.xy
  
  quantile_estimate_wow_xy = function(target.coord_x, target.coord_y, kriging_locat)
  {
    # n = 40,34.  test: south 391 - 40; cabauw 348 - 34;
    all_distance_sq = (kriging_locat$x - target.coord_x)^2 + (kriging_locat$y - target.coord_y)^2
    label_min = which.min(all_distance_sq)
    value = kriging_locat[label_min,]
    return(value)
  }
  
  for (nq in 1:100) {
    interpolate_quantiles[nq,] = quantile_estimate_wow_xy(wow_bc39_locate_xy_df$x, 
                                                          wow_bc39_locate_xy_df$y,
                                                          season_knmi_kriging[[nq]])
    # interpolate_quantiles[nq,] = quantile_estimate_wow_xy(wow_bc39_locate_xy_df$x[wn],
    #                                                       wow_bc39_locate_xy_df$y[wn],
    #                                                       season_knmi_kriging[[nq]])
  }
  interpolate_quantile_values = interpolate_quantiles$quantile_value
  return(interpolate_quantile_values)
}
