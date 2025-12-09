# Title rs_VPD_psi_r_f function for the contribution of VPD and soil water potential to stomatal resistance according to Neukam et al. (2018)

Title rs_VPD_psi_r_f function for the contribution of VPD and soil water
potential to stomatal resistance according to Neukam et al. (2018)

## Usage

``` r
rs_VPD_psi_r_f(VPD, psi_r, rs_min)
```

## Arguments

- VPD:

  vapour pressure deficit \[hPa\]

- psi_r:

  weighted average soil water potential in rooted soil \[MPa\]

- rs_min:

  minimum stomatal conductance \[s/m\]

## Value

rs_VPD_psi_r_f \[s/m\]

## Examples

``` r
rs_VPD_psi_r_f(10, 0.1, 10)
```
