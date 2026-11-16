# UpdateDWDData_to_fst updates the DWD weather data to a local fst file

UpdateDWDData_to_fst updates the DWD weather data to a local fst file

## Usage

``` r
UpdateDWDData_to_fst(
  dataperiod = "recent",
  startdate = "1990-01-01",
  isloadnew = TRUE,
  DWD_content = NULL,
  MinDataset = FALSE
)
```

## Arguments

- dataperiod:

  choose recent or historical weather data

- startdate:

  start date for the weather data; the complete date becomes part of the
  output filename

- isloadnew:

  option to load data from DWD ftp server

- DWD_content:

  list with stationlist, ziplist, zipID

- MinDataset:

  option to load only the minimum dataset

## Value

The path of the written FST file, invisibly.
