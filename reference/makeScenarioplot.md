# makeScenarioplot makes time series plots from a data frame with Time in the first column as Excel date and the parameter to be plotted in another selected column

makeScenarioplot makes time series plots from a data frame with Time in
the first column as Excel date and the parameter to be plotted in
another selected column

## Usage

``` r
makeScenarioplot(
  df_hist,
  df_scen,
  parameter,
  BaseSize = 18,
  ylabel = "",
  SelYear = 0,
  ShiftYears = F,
  StartMonth = 9,
  StartScenarioDate = NULL,
  smoothing = F,
  span = 0.5,
  plotly = F
)
```

## Arguments

- df_hist:

  data frame with historic weather/simulation data

- df_scen:

  data frame with scenario weather/simulation data

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

- StartScenarioDate:

  The start date for the scenario, i.e. the first day with unkown
  weather

- smoothing:

  Smooth the historic data with a loess function

- span:

  The span for the loess function

- plotly:

  Use plotly for interactive plots

## Value

A ggplot object
