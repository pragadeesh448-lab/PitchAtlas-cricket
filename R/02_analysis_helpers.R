# Shared PitchAtlas summaries and normalizers.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

format_label <- function(x) {
  raw <- as.character(x)
  z <- toupper(trimws(raw))
  dplyr::case_when(
    z %in% c("TEST", "MDM", "WTEST", "TEST MATCH") ~ "Test",
    z %in% c("ODI", "ODM", "WODI") ~ "ODI",
    z %in% c("T20", "IT20", "T20I", "WT20", "WIT20") ~ "T20",
    TRUE ~ raw
  )
}

summarise_innings_by_venue <- function(innings_df) {
  if (is.null(innings_df) || nrow(innings_df) == 0) return(data.frame())
  innings_df |>
    dplyr::mutate(format_group = format_label(match_type), year = lubridate::year(match_date)) |>
    dplyr::group_by(venue, format_group, innings_number) |>
    dplyr::summarise(
      avg_score = mean(innings_runs, na.rm = TRUE),
      innings_count = dplyr::n(),
      matches_count = dplyr::n_distinct(match_id),
      median_score = stats::median(innings_runs, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(venue, format_group, innings_number)
}

summarise_venue_year <- function(innings_df) {
  if (is.null(innings_df) || nrow(innings_df) == 0) return(data.frame())
  innings_df |>
    dplyr::mutate(format_group = format_label(match_type), year = lubridate::year(match_date)) |>
    dplyr::group_by(venue, format_group, year, innings_number) |>
    dplyr::summarise(
      avg_score = mean(innings_runs, na.rm = TRUE),
      innings_count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(venue, format_group, year, innings_number)
}

normalise_ball_length <- function(x) {
  z <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    is.na(z) | z == "" ~ NA_character_,
    # Keep the source categories that are not actual pitched landing zones.
    stringr::str_detect(z, "full toss|full-toss") ~ "Full toss",
    stringr::str_detect(z, "yorker") ~ "Yorker",
    # Do not misclassify "short of a good length" as "good length".
    stringr::str_detect(z, "short of (a )?good length|back of (a )?length|back-of-a-length") ~ "Short of good length",
    stringr::str_detect(z, "hard length|hard") ~ "Hard length",
    stringr::str_detect(z, "good length|^good$") ~ "Good length",
    stringr::str_detect(z, "short|bouncer") ~ "Short",
    stringr::str_detect(z, "fuller|full length|full-pitched|full pitched|^full$") ~ "Fuller",
    TRUE ~ NA_character_
  )
}

normalise_wicket_flag <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  z <- tolower(trimws(as.character(x)))
  z %in% c("true", "1", "yes", "y", "wicket", "out")
}

prepare_length_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) stop("The uploaded file has no rows.")
  # Normalize headers so variants such as "shot direction" and "runs off bat"
  # become stable names used by the app.
  names(df) <- gsub("[^a-z0-9]+", "_", tolower(trimws(names(df))))
  names(df) <- gsub("^_|_$", "", names(df))
  if (!"venue" %in% names(df)) stop("Required column not found: venue")
  if (!"ball_length" %in% names(df)) stop("Required column not found: ball_length")
  if (!"wicket" %in% names(df) && !"wicket_type" %in% names(df)) {
    stop("Need a wicket or wicket_type column to calculate wicket rate.")
  }

  out <- df |>
    dplyr::mutate(
      venue = as.character(.data$venue),
      length_group = normalise_ball_length(.data$ball_length),
      is_wicket = if ("wicket" %in% names(df) && any(!is.na(df$wicket) & nzchar(trimws(as.character(df$wicket))))) {
        normalise_wicket_flag(.data$wicket)
      } else if ("wicket_type" %in% names(df)) {
        !is.na(.data$wicket_type) & nzchar(trimws(as.character(.data$wicket_type)))
      } else {
        rep(FALSE, nrow(df))
      }
    )

  if ("format" %in% names(out)) out$format_group <- format_label(out$format)
  else if ("match_type" %in% names(out)) out$format_group <- format_label(out$match_type)
  else out$format_group <- "All formats / not supplied"

  if ("season" %in% names(out)) out$year <- suppressWarnings(as.integer(substr(as.character(out$season), 1, 4)))
  else if ("start_date" %in% names(out)) out$year <- suppressWarnings(lubridate::year(as.Date(out$start_date)))
  else if ("date" %in% names(out)) out$year <- suppressWarnings(lubridate::year(as.Date(out$date)))
  else out$year <- NA_integer_

  # Shot-direction charts use batter runs only. Do not assign extras to a
  # ground sector because those runs may not have come from a struck shot.
  direction_col <- intersect(c("shot_direction", "shot_direction_group", "direction_of_shot"), names(out))
  if (length(direction_col)) {
    out$shot_direction <- trimws(as.character(out[[direction_col[[1]]]]))
    out$shot_direction[!nzchar(out$shot_direction)] <- NA_character_
  } else {
    out$shot_direction <- NA_character_
  }

  run_col <- intersect(c("runs_off_bat", "runs_batter", "batter_runs", "runs_scored_off_bat"), names(out))
  if (length(run_col)) {
    out$runs_for_direction <- suppressWarnings(as.numeric(out[[run_col[[1]]]]))
  } else {
    out$runs_for_direction <- NA_real_
  }
  out$shot_sector <- normalise_shot_sector(out$shot_direction)

  out
}

normalise_shot_sector <- function(x) {
  # Keep the source's recorded shot-direction label rather than collapsing
  # distinct locations such as point, third man, and sweeper cover.
  z <- stringr::str_squish(as.character(x))
  z[is.na(z) | !nzchar(z) | tolower(z) %in% c("nan", "none", "n/a", "na", "null")] <- NA_character_
  stringr::str_to_title(z)
}

summarise_length_wickets <- function(length_df) {
  if (is.null(length_df) || nrow(length_df) == 0) return(data.frame())
  length_df |>
    dplyr::filter(!is.na(length_group)) |>
    dplyr::group_by(venue, format_group, length_group) |>
    dplyr::summarise(
      labelled_deliveries = dplyr::n(),
      wicket_deliveries = sum(is_wicket, na.rm = TRUE),
      wicket_rate_pct = 100 * wicket_deliveries / labelled_deliveries,
      .groups = "drop"
    ) |>
    dplyr::arrange(venue, format_group, factor(length_group, levels = c("Full toss", "Yorker", "Fuller", "Good length", "Short of good length", "Hard length", "Short")))
}
