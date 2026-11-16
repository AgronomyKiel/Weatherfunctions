# Download daily numeric forecast variables from Open-Meteo.
# Dependencies: httr2, dplyr, tidyr.
# datetime and reference_datetime are Dates (daily values in the requested timezone).
# reference_datetime denotes forecast day zero, not the model initialization time.
get_daily_forecast <- function(
    latitude, longitude, site_id = NULL, forecast_days = 5L,
    past_days = 0L, model = "generic",
    variables = c("temperature_2m_max", "temperature_2m_min"),
    timezone = "Europe/Berlin") {
  endpoints <- c(
    generic = "forecast", metno = "metno", dwd = "dwd", gfs = "gfs",
    meteofrance = "meteofrance", ecmwf = "ecmwf", jma = "jma", gem = "gem"
  )
  model <- match.arg(model, names(endpoints))
  valid_number <- function(x, lower, upper, integer = FALSE) {
    is.numeric(x) && length(x) == 1L && is.finite(x) &&
      x >= lower && x <= upper && (!integer || x == floor(x))
  }
  if (!valid_number(latitude, -90, 90)) stop("Invalid latitude.")
  if (!valid_number(longitude, -180, 360)) stop("Invalid longitude.")
  if (longitude > 180) longitude <- longitude - 360
  if (!valid_number(forecast_days, 1, 16, TRUE)) {
    stop("forecast_days must be an integer from 1 to 16; model limits may be lower.")
  }
  if (!valid_number(past_days, 0, 92, TRUE)) {
    stop("past_days must be an integer from 0 to 92.")
  }
  if (!is.character(variables) || !length(variables) ||
      anyNA(variables) || any(!nzchar(variables))) {
    stop("variables must contain daily numeric variable names.")
  }
  variables <- unique(variables)
  if (is.null(site_id)) site_id <- paste(latitude, longitude, sep = "_")
  if (length(site_id) != 1L || is.na(site_id)) stop("Supply one site_id.")

  response <- httr2::request(paste0(
    "https://api.open-meteo.com/v1/", endpoints[[model]]
  )) |>
    httr2::req_url_query(
      latitude = latitude, longitude = longitude,
      daily = paste(variables, collapse = ","),
      wind_speed_unit = "ms", forecast_days = forecast_days,
      past_days = past_days, timezone = timezone
    ) |>
    httr2::req_timeout(60) |>
    httr2::req_perform()
  # Keep JSON nulls as NA inside daily vectors.
  v <- httr2::resp_body_json(response, simplifyVector = TRUE)
  if (isTRUE(v$error)) stop(v$reason, call. = FALSE)
  if (is.null(v$daily$time) || !all(variables %in% names(v$daily))) {
    stop("API response is missing the requested daily data.")
  }
  daily <- dplyr::as_tibble(v$daily)
  daily$time <- as.Date(daily$time)
  if (nrow(daily) <= past_days || anyNA(daily$time)) {
    stop("API response has missing or insufficient daily dates.")
  }
  reference_date <- daily$time[past_days + 1L]
  units <- dplyr::tibble(
    variable = variables,
    unit = vapply(variables, function(variable) {
      unit <- v$daily_units[[variable]]
      if (is.null(unit)) NA_character_ else as.character(unit)
    }, character(1))
  )
  daily |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable", values_to = "prediction"
    ) |>
    dplyr::rename(datetime = time) |>
    dplyr::mutate(
      reference_datetime = reference_date,
      site_id = as.character(.env$site_id), model_id = .env$model
    ) |>
    dplyr::left_join(units, by = "variable") |>
    dplyr::select(datetime, reference_datetime, site_id, model_id,
                  variable, prediction, unit)
}

# Example (run explicitly):
# myVariables <- c("temperature_2m_max", "temperature_2m_min",
#                  "precipitation_sum", "shortwave_radiation_sum",
#                  "sunshine_duration")
# df <- get_daily_forecast(52, 10, forecast_days = 5, past_days = 1,
#                          variables = myVariables)
