# getsingleDWDRain loads rainfall data for a single weather station

getsingleDWDRain loads rainfall data for a single weather station

## Usage

``` r
getsingleDWDRain(station, ziplist, repository, quiet = T)
```

## Arguments

- station:

  5 digit string ID of the weather station as character

- ziplist:

  Character array with the ZIP-Filenames

- repository:

  ftp Repository address or local directory

- quiet:

  echo on/off

- local:

  Option to use local copy or ftp data

## Value

data frame with rain data from DWD for either historical or recent zip
files
