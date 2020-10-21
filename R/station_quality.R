#' @name station_quality
#' @title Check the quality of an individual station based on its observations and the outputs after persistence check.
#' @description For an indivifual station, if more than 35% of its raw observations records zero,
#' then it is a suspect station; if more than 10% of its nonzero observations failed the persist test, 
#' then it is a low-quality station; if both happens, the station is surely a bad station.
#' @param windspeed.df a data.frame, the output of the persist check.
#' @return a logical, `FALSE` means the station is not reliable and should be removed before spatial quality control,
#' `TRUE` means the station has acceptable quality for further analysis.
#' @export

station_quality <- function(windspeed.df)
{
  zero_label = which(windspeed.df$windspeed_metrepersecond == 0)
  nonzero_label = which(windspeed.df$windspeed_metrepersecond != 0)
  zero_percent = length(zero_label) / sum(!is.na(windspeed.df$windspeed_metrepersecond))
  
  persist_fail = which(windspeed.df$flag_persist == "fail.persist")
  nonzero_persist_fail = intersect(nonzero_label, persist_fail)
  nonzero_persist_fail_percent = length(nonzero_persist_fail) / length(nonzero_label)
  
  if (zero_percent < 0.35 & nonzero_persist_fail_percent < 0.1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# for (i in 1:39) {
#   k = final_label[i]
#   print(k)
#   print(station_quality(persist_wow39[[k]]))
# }
