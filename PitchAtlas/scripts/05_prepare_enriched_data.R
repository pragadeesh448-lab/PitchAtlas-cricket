# PitchAtlas enriched-data preparation (one-time or when source files change)
# Run from project root with Shiny STOPPED:
# source("scripts/05_prepare_enriched_data.R")
# This script reads only the required columns from the enriched archive and saves
# a compact, compressed RDS used by scripts/06_build_dashboard_cache.R.

options(width = 120)
message("PitchAtlas: preparing the enriched archive. This may take several minutes and runs only when source data changes.")

required_packages <- c("data.table", "dplyr", "stringr", "readr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install required packages first: install.packages(c(", paste(shQuote(missing_packages), collapse = ", "), ")).")
source(file.path("R", "02_analysis_helpers.R"))

normalise_header <- function(x) {
  x <- gsub("[^a-z0-9]+", "_", tolower(trimws(x)))
  gsub("^_|_$", "", x)
}

find_source <- function(filename) {
  profile <- Sys.getenv("USERPROFILE", unset = "")
  roots <- c("data/raw/enriched", "data/raw", "data/enriched", "../../archive", "../archive", "archive")
  if (nzchar(profile)) roots <- c(roots, file.path(profile, "Downloads", "archive"))
  candidates <- unique(file.path(roots, filename))
  hit <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (length(hit)) normalizePath(hit[[1]], winslash = "/", mustWork = TRUE) else NA_character_
}

read_selected_dt <- function(path, wanted, required = character()) {
  header <- names(data.table::fread(path, nrows = 0, showProgress = FALSE))
  norm <- normalise_header(header)
  absent <- setdiff(required, norm)
  if (length(absent)) stop("Source file ", basename(path), " is missing required field(s): ", paste(absent, collapse = ", "))
  keep <- which(norm %in% wanted)
  if (!length(keep)) stop("None of the requested columns are present in ", basename(path), ".")
  chosen <- header[keep]
  key_norms <- c("match_id", "innings", "delivery_number", "over", "ball_in_over", "season", "start_date",
                 "format", "type", "batting_team", "bowling_team", "bowling_style_bowler", "bowling_style_striker",
                 "ball_length", "ball_line", "shot_direction", "winner", "team_1", "team_2", "toss_winner",
                 "toss_decision", "outcome", "result_method")
  char_cols <- chosen[norm[keep] %in% key_norms]
  col_classes <- if (length(char_cols)) list(character = char_cols) else NULL
  z <- data.table::fread(path, select = chosen, colClasses = col_classes,
                         showProgress = TRUE, nThread = max(1L, min(4L, data.table::getDTthreads())))
  data.table::setnames(z, norm[keep])
  z
}

base_path <- find_source("ball_by_ball_data.csv")
tracking_path <- find_source("cricinfo_tracking.csv")
matches_path <- find_source("matches.csv")
if (is.na(base_path)) stop("Cannot find Downloads/archive/ball_by_ball_data.csv. Keep the extracted archive folder under Downloads/archive.")
if (is.na(tracking_path)) stop("Cannot find Downloads/archive/cricinfo_tracking.csv.")
if (is.na(matches_path)) stop("Cannot find Downloads/archive/matches.csv.")

message("[1/7] Source files found:")
message("  Base:     ", base_path)
message("  Tracking: ", tracking_path)
message("  Matches:  ", matches_path)

base_cols <- c("match_id", "season", "start_date", "innings", "delivery_number", "over", "ball_in_over",
               "is_legal_delivery", "batting_team", "bowling_team", "runs_off_bat", "extras", "wides", "noballs",
               "byes", "legbyes", "penalty", "wicket_type", "wicket", "format", "type", "event",
               "bowling_style_bowler", "bowling_style_striker")
message("[2/7] Reading selected delivery columns from the large base CSV...")
balls <- read_selected_dt(base_path, base_cols, required = c("match_id", "innings", "delivery_number", "runs_off_bat", "wicket"))
normalise_key <- function(x, integer_like = FALSE) {
  v <- trimws(as.character(x))
  if (integer_like) v <- sub("\\.0+$", "", v)
  v[is.na(v) | !nzchar(v) | tolower(v) %in% c("na", "nan", "null")] <- NA_character_
  v
}
balls[, match_id := normalise_key(match_id, TRUE)]
balls[, innings := normalise_key(innings, TRUE)]
balls[, delivery_number := normalise_key(delivery_number, FALSE)]
if (anyDuplicated(balls, by = c("match_id", "innings", "delivery_number"))) {
  stop("The base CSV has duplicate match/innings/delivery keys; the join was stopped to avoid double-counting.")
}
message("  Base delivery rows: ", format(nrow(balls), big.mark = ","))

message("[3/7] Reading venue, match result, toss and team metadata...")
meta_wanted <- c("match_id", "venue", "winner", "team_1", "team_2", "toss_winner", "toss_decision",
                 "scheduled_overs", "outcome", "result_method", "format", "type", "season", "start_date", "event")
meta <- read_selected_dt(matches_path, meta_wanted, required = c("match_id", "venue"))
meta[, match_id := normalise_key(match_id, TRUE)]
if ("venue" %in% names(meta)) { meta[, venue := trimws(as.character(venue))]; meta[!is.na(venue) & venue == "", venue := NA_character_] }
for (nm in intersect(c("winner", "team_1", "team_2", "toss_winner", "toss_decision", "outcome", "result_method"), names(meta))) {
  meta[, (nm) := trimws(as.character(get(nm)))]
  meta[get(nm) == "", (nm) := NA_character_]
}
if ("scheduled_overs" %in% names(meta)) meta[, scheduled_overs := suppressWarnings(as.numeric(scheduled_overs))]
meta <- meta[!is.na(match_id)]
# Prefer a row with result and venue metadata when a source exports duplicates.
meta[, .quality := as.integer(!is.na(venue)) + as.integer(!is.na(winner))]
data.table::setorder(meta, match_id, -.quality)
meta <- unique(meta, by = "match_id")
meta[, .quality := NULL]
# Avoid collisions with the delivery-level format/date/event columns.
meta_keep <- intersect(c("match_id", "venue", "winner", "team_1", "team_2", "toss_winner", "toss_decision",
                         "scheduled_overs", "outcome", "result_method"), names(meta))
meta <- meta[, ..meta_keep]
if ("winner" %in% names(meta)) data.table::setnames(meta, "winner", "match_winner")
balls <- merge(balls, meta, by = "match_id", all.x = TRUE, sort = FALSE)

message("[4/7] Reading and joining recorded tracking fields...")
track_cols <- c("match_id", "innings", "delivery_number", "ball_length", "ball_line", "shot_played", "shot_control",
                "wagon_x", "wagon_y", "wagon_zone", "wagon_angle", "wagon_distance", "wagon_in_ring",
                "shot_direction", "ball_timestamp", "ball_events")
tracking <- read_selected_dt(tracking_path, track_cols, required = c("match_id", "innings", "delivery_number", "ball_length"))
tracking[, match_id := normalise_key(match_id, TRUE)]
tracking[, innings := normalise_key(innings, TRUE)]
tracking[, delivery_number := normalise_key(delivery_number, FALSE)]
join_keys <- c("match_id", "innings", "delivery_number")
if (anyDuplicated(tracking, by = join_keys)) {
  dup_n <- sum(duplicated(tracking, by = join_keys))
  stop("Tracking file contains ", format(dup_n, big.mark = ","), " duplicate delivery keys. No unsafe join was performed.")
}
tracking <- tracking[!is.na(match_id) & !is.na(innings) & !is.na(delivery_number)]
balls <- merge(balls, tracking, by = join_keys, all.x = TRUE, sort = FALSE)
message("  Length-labelled deliveries: ", format(sum(!is.na(balls$ball_length) & nzchar(balls$ball_length)), big.mark = ","))

message("[5/7] Normalizing metrics and labels...")
d <- as.data.frame(balls)
names(d) <- normalise_header(names(d))
# Helper creates source-preserving shot_direction, length_group and is_wicket.
d <- prepare_length_data(d)
num_or_zero <- function(x) { z <- suppressWarnings(as.numeric(as.character(x))); z[!is.finite(z)] <- 0; z }
d$runs_off_bat <- num_or_zero(d$runs_off_bat)
d$extras <- if ("extras" %in% names(d)) num_or_zero(d$extras) else 0
d$total_runs <- d$runs_off_bat + d$extras
d$over_num <- if ("over" %in% names(d)) suppressWarnings(as.numeric(as.character(d$over))) else NA_real_
d$ball_in_over_num <- if ("ball_in_over" %in% names(d)) suppressWarnings(as.numeric(as.character(d$ball_in_over))) else NA_real_
d$legal_ball <- if ("is_legal_delivery" %in% names(d)) {
  z <- tolower(trimws(as.character(d$is_legal_delivery)))
  z %in% c("true", "1", "yes", "y", "legal")
} else {
  wides <- if ("wides" %in% names(d)) num_or_zero(d$wides) else 0
  noballs <- if ("noballs" %in% names(d)) num_or_zero(d$noballs) else 0
  (wides == 0 & noballs == 0)
}
d$match_winner <- if ("match_winner" %in% names(d)) trimws(as.character(d$match_winner)) else NA_character_
d$match_winner[is.na(d$match_winner) | !nzchar(d$match_winner)] <- NA_character_
d$team_1 <- if ("team_1" %in% names(d)) as.character(d$team_1) else NA_character_
d$team_2 <- if ("team_2" %in% names(d)) as.character(d$team_2) else NA_character_
d$toss_winner <- if ("toss_winner" %in% names(d)) as.character(d$toss_winner) else NA_character_
d$toss_decision <- if ("toss_decision" %in% names(d)) as.character(d$toss_decision) else NA_character_
d$scheduled_overs <- if ("scheduled_overs" %in% names(d)) suppressWarnings(as.numeric(d$scheduled_overs)) else NA_real_
d$bowling_style_bowler <- if ("bowling_style_bowler" %in% names(d)) as.character(d$bowling_style_bowler) else NA_character_
style <- stringr::str_to_lower(d$bowling_style_bowler)
d$bowler_type <- dplyr::case_when(
  stringr::str_detect(style, "spin|off.?break|leg.?break|orthodox|chinaman|wrist.?spin|slow left arm") ~ "Spin",
  stringr::str_detect(style, "fast|medium|seam|pace") ~ "Pace",
  TRUE ~ "Unknown"
)
d$match_date <- if ("start_date" %in% names(d)) suppressWarnings(as.Date(as.character(d$start_date))) else as.Date(NA)
d$format_group <- as.character(d$format_group)
d$year <- if ("season" %in% names(d)) suppressWarnings(as.integer(substr(as.character(d$season), 1, 4))) else suppressWarnings(as.integer(format(d$match_date, "%Y")))

cache_columns <- c("venue", "match_id", "innings", "delivery_number", "over", "over_num", "ball_in_over", "ball_in_over_num",
  "legal_ball", "batting_team", "bowling_team", "season", "start_date", "match_date", "year", "format", "type", "format_group", "event",
  "match_winner", "team_1", "team_2", "toss_winner", "toss_decision", "scheduled_overs", "outcome", "result_method",
  "runs_off_bat", "extras", "wides", "noballs", "byes", "legbyes", "penalty", "total_runs", "wicket_type", "wicket", "is_wicket", "ball_length", "length_group",
  "shot_direction", "shot_sector", "runs_for_direction", "wagon_angle", "wagon_distance", "wagon_x", "wagon_y", "wagon_zone",
  "wagon_in_ring", "ball_line", "shot_played", "shot_control", "bowling_style_bowler", "bowler_type")
d <- dplyr::select(d, dplyr::any_of(cache_columns))
d <- d[!is.na(d$venue) & nzchar(trimws(as.character(d$venue))), , drop = FALSE]
if (!nrow(d)) stop("No delivery rows have a valid venue after matching matches.csv.")
attr(d, "pitchatlas_cache_version") <- 6L
attr(d, "pitchatlas_tracking_joined") <- any(!is.na(d$ball_length) | !is.na(d$shot_direction) | !is.na(d$wagon_angle) | !is.na(d$wagon_distance))

message("[6/7] Saving the prepared cache with gzip compression...")
out_path <- file.path("data", "processed", "enriched_ball_data.rds")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
tmp_path <- paste0(out_path, ".tmp")
saveRDS(d, tmp_path, compress = "gzip")
if (file.exists(out_path)) file.remove(out_path)
if (!file.rename(tmp_path, out_path)) stop("Could not replace the prepared cache. Check free disk space and file permissions.")
message("[7/7] Preparation complete.")
message("  Delivery rows: ", format(nrow(d), big.mark = ","))
message("  Venues: ", format(length(unique(d$venue)), big.mark = ","))
message("  Winner-known rows: ", format(sum(!is.na(d$match_winner)), big.mark = ","))
message("  Length-labelled rows: ", format(sum(!is.na(d$length_group)), big.mark = ","))
message("  Recorded shot directions: ", format(sum(!is.na(d$shot_direction) & is.finite(d$runs_for_direction)), big.mark = ","))
message("  Saved cache: ", normalizePath(out_path, winslash = "/", mustWork = TRUE))
message("Next: stop any running app and run source(\"scripts/06_build_dashboard_cache.R\") once to build the fast summaries and prediction caches.")
