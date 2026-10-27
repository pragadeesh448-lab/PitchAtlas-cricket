# PitchAtlas one-time enriched-data preparation
# Run from the PitchAtlas project root with the Shiny app STOPPED:
# source("scripts/05_prepare_enriched_data.R")

options(width = 120)
message("PitchAtlas: preparing enriched cricket data. This runs once; the Shiny app will use the cached RDS afterwards.")

if (!requireNamespace("data.table", quietly = TRUE)) {
  message("Installing the fast CSV reader package data.table...")
  install.packages("data.table", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package data.table is required. Run install.packages('data.table') and retry.")
}
if (!requireNamespace("dplyr", quietly = TRUE) || !requireNamespace("stringr", quietly = TRUE)) {
  stop("Required packages are missing. Run the PitchAtlas package setup first.")
}
source(file.path("R", "02_analysis_helpers.R"))

find_source <- function(filename) {
  user_profile <- Sys.getenv("USERPROFILE", unset = "")
  candidates <- c(
    if (nzchar(user_profile)) file.path(user_profile, "Downloads", "archive", filename),
    "C:/Users/Pragadeesh/Downloads/archive/ball_by_ball_data.csv",
    file.path("data", "raw", "enriched", filename),
    file.path("data", "raw", filename),
    file.path("archive", filename),
    file.path("..", "archive", filename),
    file.path("..", "..", "archive", filename)
  )
  # For fixed-name candidates only, keep each basename aligned with the requested filename.
  candidates <- unique(candidates[!is.na(candidates)])
  candidates <- candidates[basename(candidates) == filename]
  hit <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (length(hit)) normalizePath(hit[[1]], winslash = "/", mustWork = TRUE) else NA_character_
}

base_path <- find_source("ball_by_ball_data.csv")
tracking_path <- find_source("cricinfo_tracking.csv")
matches_path <- find_source("matches.csv")
if (is.na(base_path)) stop("Cannot find Downloads/archive/ball_by_ball_data.csv. Keep the extracted archive at C:/Users/Pragadeesh/Downloads/archive.")
if (is.na(tracking_path)) stop("Cannot find Downloads/archive/cricinfo_tracking.csv.")
if (is.na(matches_path)) stop("Cannot find Downloads/archive/matches.csv.")

# Column names are inspected without reading all rows. Select only fields the dashboard needs.
message("[1/6] Source files found:")
message("  ball-by-ball: ", base_path)
message("  tracking:     ", tracking_path)
message("  matches:      ", matches_path)
message("[2/6] Reading required delivery columns from the 1.68 GB base file...")
base_cols <- c("match_id", "season", "start_date", "innings", "delivery_number",
               "runs_off_bat", "wicket_type", "wicket", "format", "type", "event",
               "extras", "non_boundary")
base_classes <- list(character = c("match_id", "innings", "delivery_number", "season", "start_date", "format", "type"))
balls <- data.table::fread(base_path, select = base_cols, colClasses = base_classes, showProgress = TRUE, nThread = max(1L, min(4L, data.table::getDTthreads())))
if (!all(c("match_id", "innings", "delivery_number", "runs_off_bat", "wicket") %in% names(balls))) {
  stop("Base CSV is missing one or more required columns: match_id, innings, delivery_number, runs_off_bat, wicket.")
}
normalise_key <- function(x, strip_integer_decimal = FALSE) {
  v <- trimws(as.character(x))
  if (strip_integer_decimal) v <- sub("\\.0+$", "", v)
  v[!nzchar(v)] <- NA_character_
  v
}
balls[, match_id := normalise_key(match_id, TRUE)]
balls[, innings := normalise_key(innings, TRUE)]
# delivery_number stays a character key; do not coerce it to numeric.
balls[, delivery_number := normalise_key(delivery_number, FALSE)]
message("  Read ", format(nrow(balls), big.mark = ","), " base deliveries.")

message("[3/6] Reading match-to-venue lookup...")
meta <- data.table::fread(matches_path, select = c("match_id", "venue"),
                          colClasses = list(character = "match_id"), showProgress = TRUE)
meta[, match_id := normalise_key(match_id, TRUE)]
meta[, venue := trimws(as.character(venue))]
meta[venue == "", venue := NA_character_]
meta <- meta[!is.na(match_id)]
# Prefer a nonblank venue when the source has more than one match metadata row.
meta[, has_venue := !is.na(venue)]
data.table::setorder(meta, match_id, -has_venue)
meta <- unique(meta, by = "match_id")
meta[, has_venue := NULL]
if (anyDuplicated(balls, by = c("match_id", "innings", "delivery_number"))) {
  stop("The base CSV has duplicate match/innings/delivery keys. To avoid inflating runs, preparation stopped.")
}
balls <- merge(balls, meta, by = "match_id", all.x = TRUE, sort = FALSE)

message("[4/6] Reading tracking columns (lengths and recorded shot directions)...")
track_cols <- c("match_id", "innings", "delivery_number", "ball_length", "ball_line", "shot_played",
                "shot_control", "wagon_x", "wagon_y", "wagon_zone", "wagon_angle", "wagon_distance",
                "wagon_in_ring", "shot_direction", "ball_timestamp", "win_probability", "predicted_score", "ball_events")
track_classes <- list(character = c("match_id", "innings", "delivery_number", "ball_length", "shot_direction"))
tracking <- data.table::fread(tracking_path, select = track_cols, colClasses = track_classes,
                              showProgress = TRUE, nThread = max(1L, min(4L, data.table::getDTthreads())))
tracking[, match_id := normalise_key(match_id, TRUE)]
tracking[, innings := normalise_key(innings, TRUE)]
tracking[, delivery_number := normalise_key(delivery_number, FALSE)]
join_keys <- c("match_id", "innings", "delivery_number")
if (anyDuplicated(tracking, by = join_keys)) {
  dup_count <- sum(duplicated(tracking, by = join_keys))
  stop("Cricinfo tracking has ", format(dup_count, big.mark = ","), " duplicate delivery keys. Stopped to avoid assigning incorrect shot positions.")
}
tracking <- tracking[!is.na(match_id) & !is.na(innings) & !is.na(delivery_number)]
balls <- merge(balls, tracking, by = join_keys, all.x = TRUE, sort = FALSE)
message("  Tracking rows matched with a recorded length: ", format(sum(!is.na(balls$ball_length) & nzchar(balls$ball_length)), big.mark = ","))
message("[5/6] Building venue, format, wicket and shot-direction measures...")
balls <- as.data.frame(balls)
# Make start_date/date usable for the shared helper; year mainly comes from season.
if ("start_date" %in% names(balls)) balls$start_date <- as.character(balls$start_date)
d <- prepare_length_data(balls)
missing_venue <- sum(is.na(d$venue) | !nzchar(trimws(as.character(d$venue))))
d <- d[!is.na(d$venue) & nzchar(trimws(as.character(d$venue))), , drop = FALSE]
if (!nrow(d)) stop("No delivery rows have a valid venue after matching matches.csv.")
# Keep true source direction labels in the chart fields; shot_sector is ancillary only.
cache_columns <- c("venue", "match_id", "innings", "delivery_number", "format_group", "year",
                   "ball_length", "length_group", "is_wicket", "shot_direction", "shot_sector",
                   "runs_for_direction", "wagon_angle", "wagon_distance", "wagon_x", "wagon_y",
                   "wagon_zone", "wagon_in_ring", "ball_line", "shot_played", "shot_control")
d <- dplyr::select(d, dplyr::any_of(cache_columns))
attr(d, "pitchatlas_cache_version") <- 5L
attr(d, "pitchatlas_missing_venue_rows") <- as.integer(missing_venue)
attr(d, "pitchatlas_tracking_joined") <- any(!is.na(d$ball_length) | !is.na(d$shot_direction) | !is.na(d$wagon_angle) | !is.na(d$wagon_distance))

message("[6/6] Saving prepared cache (uncompressed for quick app startup)...")
cache_path <- file.path("data", "processed", "enriched_ball_data.rds")
dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(d, cache_path, compress = FALSE)
message("PREPARATION COMPLETE")
message("  Deliveries saved:         ", format(nrow(d), big.mark = ","))
message("  Actual venue names:       ", format(length(unique(d$venue)), big.mark = ","))
message("  Length-labelled records:  ", format(sum(!is.na(d$length_group)), big.mark = ","))
message("  Rows with shot direction: ", format(sum(!is.na(d$shot_direction) & nzchar(d$shot_direction)), big.mark = ","))
message("  Cached file:              ", normalizePath(cache_path, winslash = "/", mustWork = TRUE))
message("Now run shiny::runApp(). The app will read the prepared cache and will not scan the large CSVs at startup.")
