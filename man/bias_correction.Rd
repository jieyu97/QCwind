% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bias_correction.R
\name{bias_correction}
\alias{bias_correction}
\title{Calibrating the bias of observed data}
\usage{
bias_correction(train.obs, train.true, test.obs, method = "empirical_quantile")
}
\arguments{
\item{train.obs}{a numeric vector of training observed data to build the bias correction model.}

\item{train.true}{a numeric vector of training true data to build the bias correction model.}

\item{test.obs}{a numeric vector of test observed data to be calibrated.}

\item{method}{a character indicating the chosen bias correction method;
\code{linear_scaling}: the linear scaling method, fitting the relationship between observed and
true data by a linear model;
\code{empirical_quantile}: the empirical quantile mapping method, mapping the quantiles between
the empirical cumulative density functions of observed and true data.}
}
\value{
a numeric vector that contains the corrected \code{test.obs} data.
}
\description{
Using different methods to calibrate the systematic bias between observed
and true data.
}
\examples{
bc_train_obs = c(0:8,9,9,9,9)
bc_test_obs = c(1:9,10,10,10,10)
bc_train_true = c(3:11,15,15,15,15)
bc_test_corrected = bias_correction(bc_train_obs, bc_train_true, bc_test_obs)
}
