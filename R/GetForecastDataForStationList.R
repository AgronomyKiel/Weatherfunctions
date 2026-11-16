# Requires get_daily_forecast.R to have been sourced first.
# Daily means replace the old hourly aggregation. VP and Sat_def are estimated
# from daily mean temperature and relative humidity, in hPa (mbar).
# forecast_days includes today. latest_recent_date, when supplied, excludes
# dates already covered by observations; it must be within past_days coverage.

#' Title
#'
#' @param fn file name with station data
#' @param forecast_days number of days to forecast
#' @param past_days number of days to backcast
#' @param latest_recent_date last available measurement date
#' @param model forecast model to be used
#' @param timezone time zone of interest
#'
#' @returns a data fram with weather data in HUME format
#' @export
#'
#' @examples
GetForecastDataForStationList <- function(
    fn, forecast_days = 7L, past_days = 2L,
    latest_recent_date = NULL, model = "generic",
    timezone = "Europe/Berlin") {
  locations <- readr::read_csv(
    fn, col_types = readr::cols(ID = readr::col_character()),
    show_col_types = FALSE
  )
  required <- c("ID", "Latitude", "Longitude")
  if (!all(required %in% names(locations))) {
    stop("Location file must contain ID, Latitude and Longitude.")
  }
  if (!nrow(locations) || anyNA(locations[required]) ||
      anyDuplicated(locations$ID) || any(!nzchar(locations$ID))) {
    stop("Supply at least one location, unique nonempty IDs and complete coordinates.")
  }
  if (!is.null(latest_recent_date)) {
    latest_recent_date <- as.Date(latest_recent_date)
    if (length(latest_recent_date) != 1L || is.na(latest_recent_date)) {
      stop("latest_recent_date must be one valid Date or ISO date string.")
    }
  }
  variables <- c(
    "temperature_2m_max", "temperature_2m_min", "temperature_2m_mean",
    "precipitation_sum", "shortwave_radiation_sum",
    "relative_humidity_2m_mean", "wind_speed_10m_mean"
  )
  results <- vector("list", nrow(locations))
  total_start <- Sys.time()
  for (i in seq_len(nrow(locations))) {
    site <- locations$ID[i]
    latitude <- locations$Latitude[i]
    longitude <- locations$Longitude[i]
    message(site, " Site Nr. ", i, "/", nrow(locations))
    site_start <- Sys.time()
    forecast <- get_daily_forecast(
      latitude = latitude, longitude = longitude, site_id = site,
      forecast_days = forecast_days, past_days = past_days,
      model = model, variables = variables, timezone = timezone
    )
    daily <- forecast |>
      dplyr::select(datetime, variable, prediction) |>
      tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
      dplyr::arrange(datetime)
    if (!is.null(latest_recent_date)) {
      if (min(daily$datetime) > latest_recent_date + 1L) {
        stop("Forecast would leave a date gap for ", site,
             "; increase past_days to cover the end of the recent observations.")
      }
      daily <- dplyr::filter(daily, datetime > latest_recent_date)
      if (!nrow(daily)) stop("No forecast dates remain after recent observations for ", site)
    }
    daily <- daily |>
      dplyr::transmute(
        Time = as.numeric(datetime - as.Date("1899-12-30")),
        TMPM = temperature_2m_mean,
        Rain = precipitation_sum,
        LF = relative_humidity_2m_mean,
        # Saturation vapour pressure is multiplied by RH to obtain actual VP.
        VP = 6.1078 * exp(17.269 * TMPM / (TMPM + 237.3)) * LF / 100,
        Sat_def = 6.1078 * exp(17.269 * TMPM / (TMPM + 237.3)) * (1 - LF / 100),
        Rad_Int = shortwave_radiation_sum * 1e6 / 86400,
        GlobRad = shortwave_radiation_sum,
        Wind = wind_speed_10m_mean,
        TMPMN = temperature_2m_min,
        TMPMX = temperature_2m_max,
        ID = .env$site,
        Longitude = .env$longitude,
        Latitude = .env$latitude
      )
    results[[i]] <- daily
    message("Time for ", site, ": ",
            round(as.numeric(difftime(Sys.time(), site_start, units = "secs")), 2),
            " seconds")
  }
  message("Time for gathering forecast data: ",
          round(as.numeric(difftime(Sys.time(), total_start, units = "secs")), 2),
          " seconds")
  dplyr::bind_rows(results)
}
