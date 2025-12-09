# GetWeatherData_selection gets the weather data for the selected stations by downloading the data from the DWD ftp server

GetWeatherData_selection gets the weather data for the selected stations
by downloading the data from the DWD ftp server

## Usage

``` r
GetWeatherData_selection(
  stations_selected,
  DWD_content,
  repository = DWD_ftp_historical,
  startdate = "1990-01-01"
)
```

## Arguments

- stations_selected:

  selected stations

- DWD_content:

  list structure of available DWD stations, must be either
  DWD_content\$recent or DWD_content\$historical

- startdate:

  start date for selection of data

## Value

data frame with weather data for selected stations
