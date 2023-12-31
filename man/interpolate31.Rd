% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolate.R
\name{interpolate31}
\alias{interpolate31}
\title{Interpolator}
\usage{
interpolate31(series, interpolator = "CUBIC_SPLINE")
}
\arguments{
\item{series}{time series}

\item{interpolator}{Either "CUBIC_SPLINE" or "NONE"}
}
\description{
Interpolates values for non-existent dates e.g. 29.02.2000
}
\author{
Thomas Witthohn
}
