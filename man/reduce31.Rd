% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolate.R
\name{reduce31}
\alias{reduce31}
\title{Invert interpolation}
\usage{
reduce31(originalSeries, interpolatedVector)
}
\arguments{
\item{originalSeries}{reference series}

\item{interpolatedVector}{series including interpolated values}
}
\description{
Removes interpolated values for non-existent dates e.g. 29.02.2000
}
\author{
Thomas Witthohn
}
