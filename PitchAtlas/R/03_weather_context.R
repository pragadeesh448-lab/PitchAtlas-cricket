# Historical weather context from the NASA POWER hourly API.
# The variables are gridded atmospheric estimates, not measurements of dew on a cricket pitch.

fetch_nasa_power_weather <- function(latitude, longitude, start_date, end_date) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite first.")
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || end_date < start_date) {
    stop("Please choose a valid date range.")
  }
  if (as.integer(end_date - start_date) > 366) {
    stop("For the first version, request at most 367 calendar days per download to keep the response manageable.")
  }
  if (is.na(latitude) || is.na(longitude) || latitude < -90 || latitude > 90 || longitude < -180 || longitude > 180) {
    stop("Invalid ground coordinates.")
  }
  parameters <- "T2M,T2MDEW,RH2M,WS10M,PRECTOT"
  url <- sprintf(
    paste0("https://power.larc.nasa.gov/api/temporal/hourly/point?parameters=%s",
           "&community=SB&longitude=%.5f&latitude=%.5f&start=%s&end=%s",
           "&format=JSON&time-standard=UTC"),
    parameters, longitude, latitude,
    format(start_date, "%Y%m%d"), format(end_date, "%Y%m%d")
  )
  old_timeout <- getOption("timeout")
  options(timeout = max(120, old_timeout %||% 60))
  on.exit(options(timeout = old_timeout), add = TRUE)
  response <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  params <- response$properties$parameter
  if (is.null(params) || !length(params)) stop("NASA POWER returned no parameter data for that location and date range.")
  available <- names(params)
  required <- c("T2M", "T2MDEW", "RH2M")
  missing <- setdiff(required, available)
  if (length(missing)) stop("NASA POWER response is missing expected parameter(s): ", paste(missing, collapse = ", "))
  first <- params[[required[[1]]]]
  time_keys <- names(first)
  if (!length(time_keys)) stop("NASA POWER response contained no hourly time keys.")
  out <- data.frame(
    datetime_utc = as.POSIXct(strptime(time_keys, format = "%Y%m%d%H", tz = "UTC")),
    stringsAsFactors = FALSE
  )
  for (nm in available) {
    vals <- unlist(params[[nm]][time_keys], use.names = FALSE)
    vals <- suppressWarnings(as.numeric(vals))
    vals[!is.na(vals) & vals <= -999] <- NA_real_
    out[[nm]] <- vals
  }
  out$dew_point_depression_c <- out$T2M - out$T2MDEW
  out$source <- "NASA POWER hourly point API"
  out$interpretation_note <- "Gridded hourly weather context; not a direct measurement of dew on the pitch."
  out
}

summarise_nasa_power_daily <- function(weather_df) {
  if (is.null(weather_df) || nrow(weather_df) == 0) return(data.frame())
  weather_df |>
    dplyr::mutate(date = as.Date(datetime_utc)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      temperature_c = mean(T2M, na.rm = TRUE),
      dew_point_c = mean(T2MDEW, na.rm = TRUE),
      relative_humidity_pct = mean(RH2M, na.rm = TRUE),
      dew_point_depression_c = mean(dew_point_depression_c, na.rm = TRUE),
      wind_speed_m_s = if ("WS10M" %in% names(weather_df)) mean(WS10M, na.rm = TRUE) else NA_real_,
      daily_precipitation_mm = if ("PRECTOT" %in% names(weather_df)) sum(PRECTOT, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
}
