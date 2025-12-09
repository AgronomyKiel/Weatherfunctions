# makeplot makes time series plots from a data frame with Time in the first column as Excel date and the parameter to be plotted in another selected column

makeplot makes time series plots from a data frame with Time in the
first column as Excel date and the parameter to be plotted in another
selected column

## Usage

``` r
makeplot(
  df,
  parameter,
  BaseSize = 18,
  ylabel = "",
  SelYear = 0,
  ShiftYears = F,
  StartMonth = 9,
  smoothing = F,
  span = 0.5,
  plotly = F
)
```

## Arguments

- df:

  data frame with weather/simulation data

- parameter:

  Column to be plotted

- BaseSize:

  Base char size for the plot

- ylabel:

  The y label string

- SelYear:

  The year to be highlighted

- ShiftYears:

  Shift the years for autumn sown crops

- StartMonth:

  The start month for the cultivation year

- smoothing:

  Smooth the historic data with a loess function

- span:

  The span for the loess function

- plotly:

  Use plotly for interactive plots

## Value

A ggplot object
