# Title getsingleDWDWeather loads data for a single weather station

Title getsingleDWDWeather loads data for a single weather station

## Usage

``` r
getsingleDWDWeather(station, ziplist, repository, quiet = T)
```

## Arguments

- station:

  5 digit ID of the weather station as character

- ziplist:

  Character array with the ZIP-Filenames

- repository:

  ftp Repository address or local directory

- quiet:

  echo on/off

## Value

data frame with weather data from DWD for either historical or recent
zip files

## Details

This is a function which loads data for a single weather station either
for recent or historical weather data. The data are retrieved in the DWD
format. The measurement height for wind speed is taken from the meta
data of the station and added to the data frame and wind speed is
corrected to a standard height of 10 m.

## Examples

``` r
getsingleDWDWeather("00044", ziplist, DWD_ftp_recent, local=F, quiet=T)
#> Error in getsingleDWDWeather("00044", ziplist, DWD_ftp_recent, local = F,     quiet = T): unused argument (local = F)
```
