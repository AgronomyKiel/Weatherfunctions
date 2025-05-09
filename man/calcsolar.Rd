% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WeatherFunctions.R
\name{calcsolar}
\alias{calcsolar}
\title{calcsolar: calculation of daily of maximum solar radiation}
\usage{
calcsolar(dayofyear, latitude)
}
\arguments{
\item{dayofyear}{Julian day of the year.}

\item{latitude}{Latitude in °.}
}
\value{
a list of 3 values/arrays angot.value = Daily maximum global radiation in [MJ.m-2.d-1], declination of sun in [rad], daylength in [h]
}
\description{
calcsolar: calculation of daily of maximum solar radiation
}
