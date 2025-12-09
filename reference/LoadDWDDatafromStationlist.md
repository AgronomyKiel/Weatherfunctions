# LoadDWDDatafromStationlist

LoadDWDDatafromStationlist

## Usage

``` r
LoadDWDDatafromStationlist(
  station_selected,
  RainStation_selected = NULL,
  DWD_content = NULL,
  DWDRain_content = NULL,
  weather_historic = NULL,
  weather_recent = NULL,
  local = F,
  StartYear = 1990,
  maxError = 2,
  Max_Height_diff = 100
)
```

## Arguments

- station_selected:

  \# a data frame with selected stations

- RainStation_selected:

  \# a data frame with selected rain stations

- weather_historic:

  \# a data frame with historical weather data

- weather_recent:

  \# a data frame with recent weather data

- StartYear:

  \# start year for data selection

- maxError:

  \# maximum deviation of monthly parameter value from average of
  stations as multiples of standard deviation

- Max_Height_diff:

  \# maximum difference from location to station to which stations are
  included

## Value

interpolated weather data in HUME format including radiation data
