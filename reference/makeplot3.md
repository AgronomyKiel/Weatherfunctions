# makeplot3 a function to plot scenarios with smoothed variation ranges

makeplot3 a function to plot scenarios with smoothed variation ranges

## Usage

``` r
makeplot3(
  dfweather,
  parameter,
  BaseSize = 18,
  ylabel = "",
  xlabel = "Datum",
  SelYear = 0,
  IsLegend = TRUE
)
```

## Arguments

- dfweather:

  data frame with weather data

- parameter:

  Column to be plotted

- BaseSize:

  Base char size for the plot

- ylabel:

  The y label string

- xlabel:

  The x label string

- SelYear:

  The year to be highlighted

- IsLegend:

  Add a legend to the plot?

## Value

A ggplot object
