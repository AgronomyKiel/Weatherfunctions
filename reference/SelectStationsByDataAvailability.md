# SelectStationsByDataAvailability selects the weather stations from a list of stations by data availability, more stations are selected until a minimum number of stations is reached which contain the required data

SelectStationsByDataAvailability selects the weather stations from a
list of stations by data availability, more stations are selected until
a minimum number of stations is reached which contain the required data

## Usage

``` r
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

- station_pars:

  List of stations and their measured parameters

- minstations:

  minimum number of stations to be selected

- MinRedundancy:

  Minimum redundancy which has to be reached for all parameters

- radius:

  Maximum radius where the station should be selected from

- startdate:

  date from which weather data should be available

- max.Height.Distance_m:

  maximum height

## Value

data frame with DWD station info for selected stations
