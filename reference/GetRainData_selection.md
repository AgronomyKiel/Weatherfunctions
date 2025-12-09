# GetRainData_selection gets the rain data for the selected stations by downloading the data from the DWD ftp server

GetRainData_selection gets the rain data for the selected stations by
downloading the data from the DWD ftp server

## Usage

``` r
GetRainData_selection(
  stations_selected,
  DWDRain_content,
  repository = DWDRain_ftp_historical,
  startdate = "1990-01-01"
)
```

## Arguments

- stations_selected:

  selected stations

- DWDRain_content:

  list structure of available DWD additional rain data stations, must be
  either DWDRain_content\$recent or DWDRain_content\$historical

- startdate:

  start date for selection of data

## Value

data frame with weather data for selected stations
