# getDWDContent gets the content (filenames etc.) of a DWD ftp repository

getDWDContent gets the content (filenames etc.) of a DWD ftp repository

## Usage

``` r
getDWDContent(repository, quiet = T, LoadMetaData = F)
```

## Arguments

- repository:

  ftp address of repository with DWD weather data

- quiet:

  Give echo or not

## Value

List with 1 data frame and one array of ZIPIDs stationlist = data frame
with the columns: "Stations_id" "von_datum" "bis_datum" "Stationshoehe"
"geoBreite" "geoLaenge" "Stationsname" "Bundesland" filelist: all
filenames of the not zipped files repository ziplist: names of the zip
files in the repository zipID: 5 digit ZipIDs
