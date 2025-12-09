# SelectStations selects the nearest weather stations from a list of stations

SelectStations selects the nearest weather stations from a list of
stations

## Usage

``` r
SelectStations(
  lat,
  long,
  height_loc = 100,
  stationlist,
  minstations = 7,
  max_stations = 20,
  radius = 70000,
  startdate,
  max.Height.Distance_m = 100
)
```

## Arguments

- lat:

  Latitude of location

- long:

  Longitude of location

- height_loc:

  Height of the location

- stationlist:

  List of stations from which the stations should be selected

- minstations:

  minimum number of stations to be selected

- radius:

  Maximum radius where the station should be selected from

- startdate:

  date from which weather data should be available

- max.Height.Distance_m:

  maximum height

## Value

data frame with DWD station info for selected stations
