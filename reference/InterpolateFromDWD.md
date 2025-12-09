# Title InterpolateFromDWD Interpolation of weather data using the 3 nearest DWD stations in between a defined difference of altitude

Title InterpolateFromDWD Interpolation of weather data using the 3
nearest DWD stations in between a defined difference of altitude

## Usage

``` r
InterpolateFromDWD(
  df_DWD_core,
  stationlist,
  geoBreite,
  geoLaenge,
  max.Height.Distance_m = 100,
  Hoehe_m = 100,
  df_Rain = NULL,
  startdate = "1990-01-01"
)
```

## Arguments

- df_DWD_core:

  data frame with DWD weather data

- stationlist:

  data frame with station information

- geoBreite:

  Latitude of location

- geoLaenge:

  Longitude of location

## Value

interpolated weather data in HUME format including radiation data
