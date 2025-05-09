% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WeatherFunctions.R
\name{makeplot3}
\alias{makeplot3}
\title{makeplot3 a function to plot scenarios with smoothed variation ranges}
\usage{
makeplot3(
  dfweather,
  parameter,
  BaseSize = 18,
  ylabel = "",
  xlabel = "Datum",
  SelYear = 0,
  IsLegend = TRUE
)
}
\arguments{
\item{dfweather}{data frame with weather data}

\item{parameter}{Column to be plotted}

\item{BaseSize}{Base char size for the plot}

\item{ylabel}{The y label string}

\item{xlabel}{The x label string}

\item{SelYear}{The year to be highlighted}

\item{IsLegend}{Add a legend to the plot?}
}
\value{
A ggplot object
}
\description{
makeplot3 a function to plot scenarios with smoothed variation ranges
}
