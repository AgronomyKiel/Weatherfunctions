# RelRad: calculation of daily solar radiation from relative sunshine the model has been fitted with observations from the DWD (relative sunshine and global radiation)

RelRad: calculation of daily solar radiation from relative sunshine the
model has been fitted with observations from the DWD (relative sunshine
and global radiation)

## Usage

``` r
RelRad(RelSun = 0.8, Month = 6)
```

## Arguments

- RelSun:

  Relative sunshine duration of a particular day of year

- Month:

  Month of the year

## Value

Relative Global radiation \[0..1\]

## Details

New function for calculation of daily solar radiation from relative
sunshine hours \#### the function uses directly the parameters from
"mod.Angstroem"
