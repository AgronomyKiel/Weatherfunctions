# Long wave net radiation for daily time steps

Long wave net radiation for daily time steps

## Usage

``` r
R_nl(
  Tmax_k,
  Tmin_k,
  ea,
  Rs,
  Rs0,
  sigma = 4.903e-09,
  a1 = 0.34,
  a2 = 0.14,
  ac = 1.35,
  bc = 0.35
)
```

## Arguments

- Tmax_k:

  maximum temperature \[K\]

- Tmin_k:

  minimum temperature \[K\]

- ea:

  actual vapor pressure \[kPa\]

- Rs:

  global radiation \[MJ/m2/d\]

- Rs0:

  clear sky radiation \[MJ/m2/d\]

- sigma:

  Stefan-Boltzmann constant \[MJ/(m2\*K4\*d)\]

## Value

long wave backward radiation \[MJ/m2/d\]
