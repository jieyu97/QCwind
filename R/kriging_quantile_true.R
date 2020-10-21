

kriging_quantile_true = function(period.number, true_quantiles, true_locat.info)
{
  require(gstat)
  require(sp)
  require(automap)
  
  # cl = 4
  c1 = period.number #1
  # choose class: "winter_night" "winter_day"   "trans_night"  "trans_day"    "summer_night" "summer_day"
  season_label = which(datetime_split$class == split_class[cl])
  ## get quantiles and locations data of KNMI stations (except the test one)
  # quantile_df = knmi_speed_quantiles[[cl]]
  quantile_df = true_quantiles #2
  quantile_df_t = t(quantile_df)
  knmi_wind_official_info = true_locat.info #3
  rownames(quantile_df_t) == knmi_wind_official_info$location
  colnames(knmi_wspd) == knmi_wind_official_info$location
  quantile_df_t = as.data.frame( cbind(knmi_wind_official_info$latitude, 
                                       knmi_wind_official_info$longitude, quantile_df_t) )
  colnames(quantile_df_t) = c('lat','lon',paste0('quantile',1:100))
  ## convert lat/lon to cartesian coordinates
  quantile_xy = quantile_df_t
  coordinates(quantile_xy) = ~lon+lat
  proj4string(quantile_xy) = CRS("+proj=longlat +datum=WGS84")
  knmi_all_quantile_xy <- sp::spTransform(quantile_xy, CRS("+proj=utm +zone=51 ellps=WGS84"))
  
  period_knmi_kriging = c()
  for (nq in 1:100) {
    quantile_nq = paste0('quantile',nq)
    
    kriging_df <- automap::autoKrige(      # using {automap}
      formula = knmi_all_quantile_xy[[quantile_nq]] ~ 1,                 # The interface is similar to {gstat} but
      input_data = knmi_all_quantile_xy,
      model = "Exp"
    ) %>%
      .$krige_output %>%
      as.data.frame() %>%
      dplyr::select(x = x1, y = x2, quantile = var1.pred)  
    
    period_knmi_kriging[[nq]] = kriging_df
  }

  return(period_knmi_kriging)
}
