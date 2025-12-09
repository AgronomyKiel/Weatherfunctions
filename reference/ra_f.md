# ra_f function for calculating aerodynamic resistance

ra_f function for calculating aerodynamic resistance

## Usage

``` r
ra_f(wind_speed, crop_height, measure_height, f_ra_funct = "Monteith-Unsworth")
```

## Arguments

- wind_speed:

  \[m\]

- crop_height:

  \[m\]

- measure_height:

  \[m\]

- f_ra_funct:

  \[character\] "Monteith-Unsworth" or 'Thom-Oliver'

## Value

aerodynamic resistance \[s/m\]

## Examples

``` r
ra_f(1, 0.1, 2, "Monteith-Unsworth")
#> [1] 199.0831
```
