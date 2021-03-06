% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kriging_quantiles.new.R
\name{kriging_quantiles.new}
\alias{kriging_quantiles.new}
\title{Generate spatial interpolated estimates at specified locations by ordinary
kriging for different quantiles of wind speed distribution.}
\usage{
kriging_quantiles.new(
  official.windspeed,
  datetime.sequence,
  official.longitude,
  official.latitude,
  split.season = TRUE,
  station.id,
  station.longitude,
  station.latitude
)
}
\arguments{
\item{official.windspeed}{a data.frame of official/reference wind speed observations,
each column represents an individual station, each row represents a timestamp.}

\item{datetime.sequence}{the date time stamps/sequence of the data \code{official.windspeed},
should indicate the row names of \code{official.windspeed}.}

\item{official.longitude}{a numeric vector, contains longitude information of official stations,
should relate to the column names (station IDs) of \code{official.windspeed}.}

\item{official.latitude}{a numeric vector, contains latitude information of official stations,
should relate to the column names (station IDs) of \code{official.windspeed}.}

\item{split.season}{a logical, if \code{TRUE} we perform the spatial interpolation by six different periods
(winter/summer/transition season, day/night time) respectively;
The six periods are determined by months and hours in a day:
winter - 12,1,2,3; summer - 6,7,8,9; trans (transition periods between winter and summer) - 4,5,10,11;
day - 7:00~18:00; night - 19:00~6:00;
6 periods: winter_night, winter_day, trans_night, trans_day, summer_night, summer_day.}

\item{station.id}{a list of test station IDs, relating to \code{station.longitude} and \code{station.latitude}.}

\item{station.longitude}{a numeric vector, contains longitude information of test stations,
should relate to the column names (station IDs) of \code{official.windspeed}.}

\item{station.latitude}{a numeric vector, contains latitude information of test stations,
should relate to the column names (station IDs) of \code{official.windspeed}.}
}
\value{
a large list of interpolated quantiles for different test station, each sublist includes
estimate quantiles (0.01-1.00) during each of the six periods.
}
\description{
For the input official wind speed data with locations, first find their quantiles from
0.01 to 1 with step interval of 0.01 (100 in total), then for quantiles at each level using
ordinary kriging to interpolate estimated quantiles at other locations.
}
