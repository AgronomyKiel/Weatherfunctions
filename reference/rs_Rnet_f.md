# Title rs_Rnet_f function for the contribution of net radiation to stomatal resistance according to Neukam et al. (2018)

Title rs_Rnet_f function for the contribution of net radiation to
stomatal resistance according to Neukam et al. (2018)

## Usage

``` r
rs_Rnet_f(Rnet, rs_min)
```

## Arguments

- Rnet:

  net radiation \[W/m2\]

- rs_min:

  minimum stomatal conductance \[s/m\]

## Value

rs_Rnet_f \[s/m\]

## Examples

``` r
rs_Rnet_f(100, 10)
#> [1] 103.7445
```
