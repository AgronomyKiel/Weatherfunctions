# InterpolateWeatherData interpolates the weather data from the selected station to the location

InterpolateWeatherData interpolates the weather data from the selected
station to the location

## Usage

``` r
InterpolateWeatherData(
  station_selected,
  RainStation_selected = NULL,
  weather_dat,
  RainData = NULL,
  maxError = 2,
  startdate = "1990-01-01"
)
```

## Arguments

- weather_dat:

  weather data from the selected station in DWD format including the
  distance and height difference to the location for which to
  interpolate the data

- maxError:

  maximum deviation for a parameter from the average of the station to
  be included in the interpolation

## Value

interpolated data in the HUME formate
