# Title rs_f function for the stomatal resistance as influenced by air temperature, net radiation, vapour pressure deficit and soil water potential according to Neukam et al. (2018)

Title rs_f function for the stomatal resistance as influenced by air
temperature, net radiation, vapour pressure deficit and soil water
potential according to Neukam et al. (2018)

## Usage

``` r
rs_f(Ta, Rnet, VPD, psi_r, rs_min)
```

## Arguments

- Ta:

  air temperature \[°C\]

- Rnet:

  net radiation \[W/m2\]

- VPD:

  vapour pressure deficit \[hPa\]

- psi_r:

  weighted average soil water potential in rooted soil \[MPa\]

- rs_min:

  minimum stomatal conductance \[s/m\]

## Value

actual stomata resistance rs_f \[s/m\]

## Examples

``` r
rs_f(20, 100, 10, 0.1, 10)
```
