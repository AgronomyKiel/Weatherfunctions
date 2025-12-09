# render_Wetter_Analysis

## Usage

``` r
render_Wetter_Analysis(
  Standort,
  geoBreite,
  geoLaenge,
  Hoehe_m,
  radius,
  minstations,
  startdate,
  maxError,
  set_title,
  out_file
)
```

## Arguments

- Standort:

  name of the site for which weather data should be estimated

- geoBreite:

  latitude of the site

- geoLaenge:

  longitude of the site

- Hoehe_m:

  height above sea level of the site

- radius:

  radius from which DWD weather stations should be included

- minstations:

  minimum number of stations to be included

- startdate:

  start date (string, format "1990-01-01")

- maxError:

  maximum deviation (

  set_titletitle of the output file

  out_filename of the output file

none, a html file is generated render_Wetter_Analysis
render_Wetter_Analysis(Standort, geoBreite, geoLaenge, Hoehe_m, radius,
minstations, startdate, maxError, set_title, out_file)
