# getcombinedDWDRain gets the combined (historic & recent) rain data for a DWD station

getcombinedDWDRain gets the combined (historic & recent) rain data for a
DWD station

## Usage

``` r
getcombinedDWDRain(
  DWDRain_content,
  station,
  local = F,
  recent_repository,
  historical_repository
)
```

## Arguments

- station:

  The ID of the station in 5 digits but as character

- local:

  Option to use local stored zip-files

- recent_repository:

  The ftp address of the recent zip files or the local directory

- historical_repository:

  The ftp address of the historical zip files or the local directory

- DWD_content:

  A list of the DWD station

## Value

data frame with DWD observations historical + recent
