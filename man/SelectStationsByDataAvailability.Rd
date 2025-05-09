% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WeatherFunctions.R
\name{SelectStationsByDataAvailability}
\alias{SelectStationsByDataAvailability}
\title{SelectStationsByDataAvailability selects the  weather stations from a list of stations
by data availability, more stations are selected until a minimum number
of stations is reached which contain the required data}
\usage{
SelectStationsByDataAvailability(
  lat,
  long,
  height_loc = 100,
  stationlist,
  station_pars,
  minstations = 3,
  MinRedundancy = 3,
  max_stations = 20,
  radius = 70000,
  startdate,
  max.Height.Distance_m = 100
)
}
\arguments{
\item{lat}{Latitude of location}

\item{long}{Longitude of location}

\item{height_loc}{Height of the location}

\item{stationlist}{List of stations from which the stations should be selected}

\item{station_pars}{List of stations and their measured parameters}

\item{minstations}{minimum number of stations to be selected}

\item{MinRedundancy}{Minimum redundancy which has to be reached for all parameters}

\item{radius}{Maximum radius where the station should be selected from}

\item{startdate}{date from which weather data should be available}

\item{max.Height.Distance_m}{maximum height}
}
\value{
data frame with DWD station info for selected stations
}
\description{
SelectStationsByDataAvailability selects the  weather stations from a list of stations
by data availability, more stations are selected until a minimum number
of stations is reached which contain the required data
}
