# Title Get_ZipLists_Station_ids

Title Get_ZipLists_Station_ids

## Usage

``` r
Get_ZipLists_Station_ids(
  dataperiod = "recent",
  loadnew = T,
  localStationdata_fn = "stationdata.RData"
)
```

## Arguments

- dataperiod:

  "recent", "historical", "both"

- loadnew:

  either true or false

- localStationdata_fn:

  name of the local file with the station meta data

## Value

list with stationlist, ziplist, histziplist, recentziplist
