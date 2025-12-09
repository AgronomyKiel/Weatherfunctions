# getDWDRainContent gets the content of the DWD ftp repository for additional rain data

getDWDRainContent gets the content of the DWD ftp repository for
additional rain data

## Usage

``` r
getDWDRainContent(repository, quiet = T)
```

## Arguments

- repository:

  ftp address of DWD repository for additional rain data

- quiet:

  option to echo off

## Value

List with 1 data frame and one array of ZIPIDs stationlist = data frame
with the columns: "Stations_id" "von_datum" "bis_datum" "Stationshoehe"
"geoBreite" "geoLaenge" "Stationsname" "Bundesland" filelist: all
filenames of the not zipped files repository ziplist: names of the zip
files in the repository zipID: 5 digit ZipIDs as characer vector
