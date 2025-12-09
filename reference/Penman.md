# Penman function for calculating potential evapotranspiration

Penman function for calculating potential evapotranspiration

## Usage

``` r
Penman(Temp, Sat_def, Net_beam, delta, gamma, l_h_v_water, ra, rc)
```

## Arguments

- Temp:

  \[°C\]

- Sat_def:

  \[mbar\] or \[hPa\]

- Net_beam:

  \[J/m2\*s\]

- delta:

  \[mbar/K\]

- gamma:

  \[mbar/K\]

- l_h_v_water:

  \[J/Kg\]

- ra:

  aerodynamic resistance \[s/m\]

- rc:

  bulk-canopy resistance \[s/m\]

## Value

potential evapotranspiration \[mm/d\]

## Examples

``` r
Penman(20, 10, 100, 0.1, 0.1, 2.477 * 1E6, 10, 10)
#> [1] 141.9565
```
