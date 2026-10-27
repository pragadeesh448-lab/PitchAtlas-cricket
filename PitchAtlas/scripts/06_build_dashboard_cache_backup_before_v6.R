# Build compact caches for the PitchAtlas Bowling Length page.
# Run once with the Shiny app STOPPED:
# source("scripts/06_build_dashboard_cache.R")
# This reads the existing enriched_ball_data.rds cache; it does NOT re-read the 1.68 GB CSVs.

options(width = 120)
message("PitchAtlas: building compact dashboard summaries from the existing enriched RDS...")

required_packages <- c("dplyr", "stringr", "readr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install required packages first: install.packages(c(", paste(shQuote(missing_packages), collapse = ", "), ")).")
}

source_path <- file.path("data", "processed", "enriched_ball_data.rds")
if (!file.exists(source_path)) {
  stop("Cannot find data/processed/enriched_ball_data.rds. Build the enriched cache first.")
}

message("[1/5] Reading the already-prepared cache (no source CSV join)...")
d <- readRDS(source_path)
if (!is.data.frame(d) || !nrow(d)) stop("The prepared enriched RDS is empty or not a data frame.")
required <- c("venue", "format_group", "length_group", "is_wicket", "shot_direction", "runs_for_direction")
missing <- setdiff(required, names(d))
if (length(missing)) stop("Prepared data is missing: ", paste(missing, collapse = ", "))

message("[2/5] Normalizing summary fields...")
d$venue <- trimws(as.character(d$venue))
d$format_group <- as.character(d$format_group)
d$length_group <- as.character(d$length_group)
d$is_wicket <- if (is.logical(d$is_wicket)) d$is_wicket else tolower(as.character(d$is_wicket)) %in% c("true", "1", "yes", "y", "wicket", "out")
d$shot_direction <- trimws(as.character(d$shot_direction))
d$shot_direction[is.na(d$shot_direction) | !nzchar(d$shot_direction)] <- NA_character_
d$runs_for_direction <- suppressWarnings(as.numeric(as.character(d$runs_for_direction)))
if (!"wagon_angle" %in% names(d)) d$wagon_angle <- NA_real_
if (!"wagon_distance" %in% names(d)) d$wagon_distance <- NA_real_
d$wagon_angle <- suppressWarnings(as.numeric(as.character(d$wagon_angle)))
d$wagon_distance <- suppressWarnings(as.numeric(as.character(d$wagon_distance)))
d <- d[!is.na(d$venue) & nzchar(d$venue) & !is.na(d$format_group) & nzchar(d$format_group), , drop = FALSE]
if (!nrow(d)) stop("No deliveries with a valid venue and format are available.")

message("[3/5] Summarizing labelled bowling lengths...")
valid_length <- !is.na(d$length_group) & nzchar(d$length_group)
length_summary <- d[valid_length, c("venue", "format_group", "length_group", "is_wicket"), drop = FALSE] |>
  dplyr::group_by(.data$venue, .data$format_group, .data$length_group) |>
  dplyr::summarise(
    labelled_deliveries = dplyr::n(),
    wicket_deliveries = sum(.data$is_wicket %in% TRUE, na.rm = TRUE),
    wicket_rate_pct = if (dplyr::n() > 0) 100 * sum(.data$is_wicket %in% TRUE, na.rm = TRUE) / dplyr::n() else NA_real_,
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$venue, .data$format_group, .data$length_group)

message("[4/5] Summarizing direction totals and sampling wagon-wheel shots...")
valid_direction <- !is.na(d$shot_direction) & nzchar(d$shot_direction) & is.finite(d$runs_for_direction)
direction_summary <- d[valid_direction, c("venue", "format_group", "shot_direction", "runs_for_direction"), drop = FALSE] |>
  dplyr::group_by(.data$venue, .data$format_group, .data$shot_direction) |>
  dplyr::summarise(
    batter_runs = sum(.data$runs_for_direction, na.rm = TRUE),
    scoring_deliveries = sum(.data$runs_for_direction > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$venue, .data$format_group, dplyr::desc(.data$batter_runs))

valid_wagon <- valid_direction & is.finite(d$wagon_angle) & is.finite(d$wagon_distance)
wagon_full <- d[valid_wagon, c("venue", "format_group", "shot_direction", "runs_for_direction", "wagon_angle", "wagon_distance"), drop = FALSE]
if (nrow(wagon_full)) {
  names(wagon_full)[names(wagon_full) == "wagon_angle"] <- "angle_num"
  names(wagon_full)[names(wagon_full) == "wagon_distance"] <- "distance_num"
  # Stratified sampling: keep up to 400 points per ground/format so the chart is
  # responsive while the pie chart still uses all recorded-direction totals.
  key <- interaction(wagon_full$venue, wagon_full$format_group, drop = TRUE, lex.order = TRUE)
  groups <- split(seq_len(nrow(wagon_full)), key)
  set.seed(20261019)
  keep <- unlist(lapply(groups, function(ix) {
    if (length(ix) > 400L) sample(ix, size = 400L, replace = FALSE) else ix
  }), use.names = FALSE)
  wagon_sample <- wagon_full[keep, , drop = FALSE]
  rownames(wagon_sample) <- NULL
} else {
  wagon_sample <- data.frame(venue = character(), format_group = character(), shot_direction = character(),
                             runs_for_direction = numeric(), angle_num = numeric(), distance_num = numeric())
}

coverage <- d |>
  dplyr::mutate(
    .has_length = !is.na(.data$length_group) & nzchar(.data$length_group),
    .has_direction = !is.na(.data$shot_direction) & nzchar(.data$shot_direction) & is.finite(.data$runs_for_direction),
    .has_wagon = .has_direction & is.finite(.data$wagon_angle) & is.finite(.data$wagon_distance)
  ) |>
  dplyr::group_by(.data$venue, .data$format_group) |>
  dplyr::summarise(
    total_deliveries = dplyr::n(),
    labelled_deliveries = sum(.data$.has_length, na.rm = TRUE),
    direction_rows = sum(.data$.has_direction, na.rm = TRUE),
    wagon_rows = sum(.data$.has_wagon, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$venue, .data$format_group)

out_dir <- file.path("data", "processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
message("[5/5] Saving compact compressed dashboard caches...")
saveRDS(length_summary, file.path(out_dir, "enriched_length_summary.rds"), compress = "gzip")
saveRDS(direction_summary, file.path(out_dir, "enriched_direction_summary.rds"), compress = "gzip")
saveRDS(wagon_sample, file.path(out_dir, "enriched_wagon_sample.rds"), compress = "gzip")
saveRDS(coverage, file.path(out_dir, "enriched_coverage_summary.rds"), compress = "gzip")

message("PREPARATION COMPLETE")
message("  Ground names:              ", format(length(unique(coverage$venue)), big.mark = ","))
message("  Venue/format summaries:    ", format(nrow(coverage), big.mark = ","))
message("  Length summary rows:       ", format(nrow(length_summary), big.mark = ","))
message("  Shot-direction summary rows:", format(nrow(direction_summary), big.mark = ","))
message("  Wagon-wheel display points:", format(nrow(wagon_sample), big.mark = ","))
message("  Output: data/processed/enriched_*_summary.rds and related compact caches")
message("Now run shiny::runApp(). Shiny will load only compact summaries, not the multi-million-row delivery cache.")
