# Construct a DWD FST filename

Creates the filename used by both FST writers and readers. Historical
filenames include the complete start date so datasets beginning on
different dates cannot silently overwrite one another. Recent
observations always use the stable filename `recent_weather_dat.fst`,
because that file is refreshed in place.

## Usage

``` r
dwd_fst_filename(
  dataperiod = c("historical", "recent"),
  startdate = "1990-01-01"
)
```

## Arguments

- dataperiod:

  Character scalar; either `"historical"` or `"recent"`.

- startdate:

  A single value coercible to `Date` giving the first date included in
  the dataset. It affects the historical filename; recent data use a
  stable filename without a date suffix.

## Value

A character scalar containing a filename, without a directory.

## Examples

``` r
dwd_fst_filename("historical", "1990-01-01")
#> [1] "weather_dat_1990-01-01.fst"
dwd_fst_filename("recent", as.Date("2025-07-01"))
#> [1] "recent_weather_dat.fst"
```
