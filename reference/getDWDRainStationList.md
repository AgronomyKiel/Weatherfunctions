# getDWDRainStationList gets a station list from the DWD ftp repository without ziplists and addtionally returns historical and recent stationlist (metadata) as well as filelists, ziplists and zipIDs

getDWDRainStationList gets a station list from the DWD ftp repository
without ziplists and addtionally returns historical and recent
stationlist (metadata) as well as filelists, ziplists and zipIDs

## Usage

``` r
getDWDRainStationList(historical, recent)
```

## Arguments

- historical:

  ftp address of historical rain data

- recent:

  ftp address of recent rain data

## Value

list of stations, recent, historical objects stations: merged data frame
of all weather stations with columns "Stations_id" "von_datum"
"bis_datum" "Stationshoehe" "geoBreite" "geoLaenge" "Stationsname"
"Bundesland" recent and historical: lists with stationlist = data frame
with the columns: "Stations_id" "von_datum" "bis_datum" "Stationshoehe"
"geoBreite" "geoLaenge" "Stationsname" "Bundesland" filelist: all
filenames of the not zipped files repository ziplist: names of the zip
files in the repository zipID: 5 digit ZipIDs
