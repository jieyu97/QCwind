% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolate_quantiles.R
\name{interpolate_quantiles}
\alias{interpolate_quantiles}
\title{Find the kriging estimated "truth" quantiles of a given test station.}
\usage{
interpolate_quantiles(
  kriging.reference.quantiles,
  station.id,
  station.longitude,
  station.latitude
)
}
\arguments{
\item{kriging.reference.quantiles}{a large list, output kriging quantiles of \code{kriging_quantile_true}.}

\item{station.id}{a list of test station IDs, relating to \code{station.longitude} and \code{station.latitude}.}

\item{station.longitude}{a numeric vector, contains longitude information of test stations,
should relate to the column names (station IDs) of \code{official.windspeed}.}

\item{station.latitude}{a numeric vector, contains latitude information of test stations,
should relate to the column names (station IDs) of \code{official.windspeed}.}
}
\value{
a large list of interpolated quantiles for different test station, each sublist includes
estimate quantiles during each of the six periods.
}
\description{
Ordinary kriging.
}
