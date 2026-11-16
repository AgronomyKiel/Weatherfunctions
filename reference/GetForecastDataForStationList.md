# Title

Title

## Usage

``` r
GetForecastDataForStationList(
  fn,
  forecast_days = 7L,
  past_days = 2L,
  latest_recent_date = NULL,
  model = "generic",
  timezone = "Europe/Berlin"
)
```

## Arguments

- fn:

  file name with station data

- forecast_days:

  number of days to forecast

- past_days:

  number of days to backcast

- latest_recent_date:

  last available measurement date

- model:

  forecast model to be used

- timezone:

  time zone of interest

## Value

a data fram with weather data in HUME format
