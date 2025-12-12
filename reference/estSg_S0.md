# estSg_S0: calculation of daily solar radiation from relative sunshine

estSg_S0: calculation of daily solar radiation from relative sunshine

## Usage

``` r
estSg_S0(RelSun, Month)
```

## Arguments

- RelSun:

  Relative sunshine duration of a particular day of year

- Month:

  Month of the year

## Value

solar radiation per day \[MJ/m2/d\]

## Details

function for calculation of daily solar radiation from relative sunshine
hours \#### the function uses an empirical model calibrated from DWD
data "mod.Angstroem"

## Examples

``` r
estSg_S0(0.8, 6)
#> Error in estSg_S0(0.8, 6): could not find function "estSg_S0"
```
