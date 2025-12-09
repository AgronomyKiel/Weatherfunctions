# GetWeatherData_selection_fst

GetWeatherData_selection_fst

## Usage

``` r
GetWeatherData_selection_fst(
  stations_selected,
  DWD_content,
  repository = DWD_ftp_recent,
  startdate = "1990-01-01"
)
```

## Arguments

- stations_selected:

  selected stations

- DWD_content:

  list structure of available DWD stations

- startdate:

  start date for selection of data

## Value

data frame with weather data for selected stations in fst format
