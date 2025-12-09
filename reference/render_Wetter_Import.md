# render_Wetter_Import

## Usage

``` r
render_Wetter_Import(
  Standort,
  geoBreite,
  geoLaenge,
  Hoehe_m,
  radius,
  minstations,
  startyear,
  endyear,
  maxError,
  set_title
)
```

## Arguments

- Standort:

  name of the site for which weather data should be estimated (string)

- geoBreite:

  latitude of the site (numeric)

- geoLaenge:

  longitude of the site (numeric)

- Hoehe_m:

  height above sea level of the site (numeric)

- radius:

  radius from which DWD weather stations should be included (numeric)

- minstations:

  minimum number of stations to be included (numeric)

- maxError:

  maximum deviation (

  set_titletitle of the output file

  startdatestart date (string, format "1990-01-01")

  out_filename of the output file

none, a html file is generated render_Wetter_Import
render_Wetter_Import(Standort, geoBreite, geoLaenge, Hoehe_m, radius,
minstations, startdate, maxError, set_title, out_file)
