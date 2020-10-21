#' @name season_split_wind
#' @title Split wind observations by different seasons and times of a day.
#' @description Given a datatime sequence, we split them into 6 periods (3 seasons and day/night times):
#' winter - 12,1,2,3; summer - 6,7,8,9; trans (transition periods between winter and summer) - 4,5,10,11;
#' day - 7:00~18:00; night - 19:00~6:00;
#' Based on our investigation, this is a suitable split methods for wind speed observations in the Netherlands.
#' We output the labels of different periods in the datetime sequence.
#' @param dt.seq a POSIXct vector of date-time sequence, usually we use the official 10-min datetime stamps.
#' @return a list of 6 elements, each one includes labels for a season.
#' @export

season_split_wind <- function(dt.seq)
{
  require(lubridate)
  

}
