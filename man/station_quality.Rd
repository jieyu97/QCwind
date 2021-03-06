% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/station_quality.R
\name{station_quality}
\alias{station_quality}
\title{Check the quality of an individual station based on its observations and the outputs after persistence check.}
\usage{
station_quality(windspeed.df)
}
\arguments{
\item{windspeed.df}{a data.frame, the output of the persist check.}
}
\value{
a list of percentages to determine the station quality
}
\description{
For an indivifual station, if more than 35\% of its raw observations records zero,
then it is a suspect station; if more than 5\% of its nonzero observations failed the persist test,
then it is a low-quality station; if there are very long periods of missing data, then it is also a
doubtful station.
We suggest manually check for such stations.
}
