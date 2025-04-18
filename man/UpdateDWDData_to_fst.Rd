% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WeatherFunctions.R
\name{UpdateDWDData_to_fst}
\alias{UpdateDWDData_to_fst}
\title{UpdateDWDData_to_fst updates the DWD weather data to a local fst file}
\usage{
UpdateDWDData_to_fst(
  dataperiod = "recent",
  startdate = "1990-01-01",
  isloadnew = T,
  DWD_content = NULL,
  MinDataset = FALSE
)
}
\arguments{
\item{dataperiod}{choose recent or historical weather data}

\item{startdate}{start date for the weather data}

\item{isloadnew}{option to load data from DWD ftp server}

\item{DWD_content}{list with stationlist, ziplist, zipID}

\item{MinDataset}{option to load only the minimum dataset}
}
\value{
Nothing, a fst file is written to the local directory
}
\description{
UpdateDWDData_to_fst updates the DWD weather data to a local fst file
}
