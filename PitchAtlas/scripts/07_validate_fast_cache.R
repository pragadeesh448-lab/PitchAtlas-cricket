# Fast validation for PitchAtlas compact caches. Run after scripts/06_build_dashboard_cache.R.
cache_dir <- "data/processed"
expected <- c(
  "enriched_length_summary.rds", "enriched_direction_summary.rds",
  "enriched_wagon_sample.rds", "enriched_coverage_summary.rds",
  "enriched_innings_final.rds", "enriched_match_results.rds",
  "prediction_score_snapshots.rds", "prediction_chase_snapshots.rds",
  "prediction_venue_baselines.rds", "bowling_style_summary.rds",
  "pace_length_summary.rds"
)
missing <- expected[!file.exists(file.path(cache_dir, expected))]
if (length(missing)) stop("Missing cache file(s): ", paste(missing, collapse = ", "))
read_cache <- function(f) tryCatch(readRDS(file.path(cache_dir, f)), error = function(e) stop("Cannot read ", f, ": ", conditionMessage(e)))
lengths <- read_cache("enriched_length_summary.rds")
directions <- read_cache("enriched_direction_summary.rds")
coverage <- read_cache("enriched_coverage_summary.rds")
match_results <- read_cache("enriched_match_results.rds")
score_states <- read_cache("prediction_score_snapshots.rds")
if (!is.data.frame(lengths) || !all(c("venue", "format_group", "length_group", "labelled_deliveries", "wicket_rate_pct") %in% names(lengths))) stop("Length summary has an unexpected schema.")
if (!is.data.frame(directions) || !all(c("venue", "format_group", "shot_direction", "batter_runs") %in% names(directions))) stop("Direction summary has an unexpected schema.")
if (!is.data.frame(coverage) || !all(c("venue", "format_group", "total_deliveries") %in% names(coverage))) stop("Coverage summary has an unexpected schema.")
cat("PITCHATLAS FAST CACHE VALIDATION PASSED\n")
cat("Length summary rows:", format(nrow(lengths), big.mark = ","), "\n")
cat("Shot-direction summary rows:", format(nrow(directions), big.mark = ","), "\n")
cat("Venues:", length(unique(coverage$venue)), "\n")
cat("Completed decision matches:", if (is.data.frame(match_results)) format(nrow(match_results), big.mark = ",") else "—", "\n")
cat("Score prediction states:", if (is.data.frame(score_states)) format(nrow(score_states), big.mark = ",") else "—", "\n")
cat("Ready to start Shiny.\n")
