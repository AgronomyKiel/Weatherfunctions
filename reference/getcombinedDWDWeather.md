# getcombinedDWDWeather gets the combined (historic & recent) weather data for a DWD station

getcombinedDWDWeather gets the combined (historic & recent) weather data
for a DWD station

## Usage

``` r
getcombinedDWDWeather(
  DWD_content,
  station,
  local = F,
  recent_repository,
  historical_repository
)
```

## Arguments

- DWD_content:

  A list structure of the DWD stations

- station:

  The ID of the station in 5 digits but as character

- local:

  Option to use local stored zip-files

- recent_repository:

  The ftp address of the recent zip files or the local directory

- historical_repository:

  The ftp address of the historical zip files or the local directory

## Value

data frame with DWD observations historical + recent
