# Title InterpolateToFST Interpolates weather data for a list of locations

Title InterpolateToFST Interpolates weather data for a list of locations

## Usage

``` r
InterpolateToFST(
  core_weather,
  df.locations,
  DWD_content,
  DWDRain_content,
  startdate,
  recent = TRUE
)
```

## Arguments

- core_weather:

  data frame with the core DWD data

- df.locations:

  data frame with the locations for which the weather data should be
  interpolated

- DWD_content:

  Meta data for the DWD weather stations

- DWDRain_content:

  Meta data for the DWD rain stations

- startdate:

  start date for the selection of the weather data

- recent:

  logical, if TRUE recent data are used, otherwise historical data

## Value

a data frame with the interpolated weather data
