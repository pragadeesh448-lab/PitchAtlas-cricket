# Build compact visualisation, predictive and recommendation caches from the
# prepared enriched RDS. Run once after R/05_prepare_enriched_data.R, with Shiny stopped.
# This script never reads the original multi-GB CSV files.

options(width = 120)
required_packages <- c("data.table", "dplyr", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install required packages first: install.packages(c(", paste(shQuote(missing_packages), collapse = ", "), ")).")

in_file <- file.path("data", "processed", "enriched_ball_data.rds")
if (!file.exists(in_file)) stop("Cannot find data/processed/enriched_ball_data.rds. Run source(\"R/05_prepare_enriched_data.R\") first.")
message("[1/8] Reading the prepared enriched RDS. No source CSVs will be re-read...")
d <- readRDS(in_file)
needed <- c("venue", "match_id", "innings", "format_group", "length_group", "is_wicket", "total_runs",
            "legal_ball", "batting_team", "match_winner", "bowler_type", "over_num", "ball_in_over_num")
missing <- setdiff(needed, names(d))
if (length(missing) || !is.data.frame(d) || !nrow(d) || !identical(attr(d, "pitchatlas_cache_version"), 6L)) {
  stop("Prepared cache is not the full PitchAtlas version 6 schema. Missing fields: ",
       if (length(missing)) paste(missing, collapse = ", ") else "none",
       ". Stop Shiny and run source(\"R/05_prepare_enriched_data.R\") once, then rerun this script.")
}
DT <- data.table::as.data.table(d) # by-reference once; avoid copies of the 5M-row table
rm(d); invisible(gc())
DT[, venue := trimws(as.character(venue))]
DT[, match_id := trimws(as.character(match_id))]
DT[, innings_no := suppressWarnings(as.integer(as.character(innings)))]
DT[, year_num := suppressWarnings(as.integer(year))]
DT[, format_group := as.character(format_group)]
DT[, total_runs_num := suppressWarnings(as.numeric(total_runs))]
DT[, byes_num := if ("byes" %in% names(DT)) suppressWarnings(as.numeric(byes)) else 0]
DT[, legbyes_num := if ("legbyes" %in% names(DT)) suppressWarnings(as.numeric(legbyes)) else 0]
DT[!is.finite(byes_num), byes_num := 0]
DT[!is.finite(legbyes_num), legbyes_num := 0]
DT[!is.finite(total_runs_num), total_runs_num := 0]
DT[, wicket_num := as.integer(is_wicket %in% TRUE)]
DT[, wicket_type_lower := if ("wicket_type" %in% names(DT)) stringr::str_to_lower(as.character(wicket_type)) else ""]
DT[, bowler_credit_num := as.integer(wicket_num == 1L & !stringr::str_detect(wicket_type_lower,
  "run.?out|retired hurt|retired out|obstructing the field|hit the ball twice|timed out|absent hurt|retired"))]
DT[, legal_num := as.integer(legal_ball %in% TRUE)]
DT[, delivery_order_num := suppressWarnings(as.numeric(delivery_number))]
DT[, over_order_num := suppressWarnings(as.numeric(over_num))]
DT[, ball_order_num := suppressWarnings(as.numeric(ball_in_over_num))]
# Delivery order is normally present; fallback values keep odd rows deterministic.
DT[!is.finite(delivery_order_num), delivery_order_num := seq_len(.N), by = .(match_id, innings_no)]
DT[!is.finite(over_order_num), over_order_num := 0]
DT[!is.finite(ball_order_num), ball_order_num := delivery_order_num]
data.table::setorder(DT, match_id, innings_no, over_order_num, ball_order_num, delivery_order_num)

message("[2/8] Building compact bowling-length, direction and coverage summaries...")
valid_len <- DT[!is.na(length_group) & nzchar(as.character(length_group)), .(venue, match_id, format_group, year_num, length_group, wicket_num, total_runs_num, legal_num, bowler_type)]
length_summary <- valid_len[, .(
  labelled_deliveries = .N,
  wicket_deliveries = sum(wicket_num, na.rm = TRUE),
  total_runs = sum(total_runs_num, na.rm = TRUE),
  wicket_rate_pct = 100 * sum(wicket_num, na.rm = TRUE) / .N,
  runs_per_6_balls = 6 * sum(total_runs_num, na.rm = TRUE) / max(1, sum(legal_num, na.rm = TRUE))
), by = .(venue, format_group, length_group)]
length_summary <- length_summary[order(venue, format_group, length_group)]

valid_dir <- DT[!is.na(shot_direction) & nzchar(trimws(as.character(shot_direction))) & is.finite(as.numeric(runs_for_direction)), .(venue, match_id, format_group, shot_direction, runs_for_direction, wagon_angle, wagon_distance)]
direction_summary <- valid_dir[, .(
  batter_runs = sum(as.numeric(runs_for_direction), na.rm = TRUE),
  scoring_deliveries = sum(as.numeric(runs_for_direction) > 0, na.rm = TRUE),
  recorded_deliveries = .N
), by = .(venue, format_group, shot_direction)]
direction_summary <- direction_summary[order(venue, format_group, -batter_runs)]

valid_wagon <- valid_dir[is.finite(as.numeric(wagon_angle)) & is.finite(as.numeric(wagon_distance)),
  .(venue, format_group, shot_direction, runs_for_direction, angle_num = as.numeric(wagon_angle), distance_num = as.numeric(wagon_distance))]
if (nrow(valid_wagon)) {
  set.seed(20261020)
  wagon_sample <- valid_wagon[, if (.N > 60L) .SD[sample.int(.N, 60L)] else .SD, by = .(venue, format_group)]
} else {
  wagon_sample <- data.table::data.table(venue=character(),format_group=character(),shot_direction=character(),
    runs_for_direction=numeric(),angle_num=numeric(),distance_num=numeric())
}

coverage <- DT[, .(
  total_deliveries = .N,
  labelled_deliveries = sum(!is.na(length_group) & nzchar(as.character(length_group))),
  direction_rows = sum(!is.na(shot_direction) & nzchar(as.character(shot_direction)) & is.finite(as.numeric(runs_for_direction))),
  wagon_rows = sum(!is.na(shot_direction) & nzchar(as.character(shot_direction)) & is.finite(as.numeric(runs_for_direction)) & is.finite(as.numeric(wagon_angle)) & is.finite(as.numeric(wagon_distance))),
  rows_with_winner = sum(!is.na(match_winner) & nzchar(trimws(as.character(match_winner))))
), by = .(venue, format_group)]

message("[3/8] Creating delivery-state snapshots and innings totals...")
# Cumulative state is computed once per match/innings in sorted delivery order.
DT[, `:=`(
  run_cum = cumsum(total_runs_num),
  wickets_cum = cumsum(wicket_num),
  legal_cum = cumsum(legal_num)
), by = .(match_id, innings_no)]
first_nonblank <- function(x) { x <- as.character(x); i <- which(!is.na(x) & nzchar(trimws(x))); if (length(i)) x[[i[[1]]]] else NA_character_ }
innings_final <- DT[, .(
  venue = first_nonblank(venue),
  format_group = first_nonblank(format_group),
  year = suppressWarnings(as.integer(first_nonblank(year_num))),
  batting_team = first_nonblank(batting_team),
  match_winner = first_nonblank(match_winner),
  team_1 = if ("team_1" %in% names(DT)) first_nonblank(team_1) else NA_character_,
  team_2 = if ("team_2" %in% names(DT)) first_nonblank(team_2) else NA_character_,
  scheduled_overs = if ("scheduled_overs" %in% names(DT)) suppressWarnings(as.numeric(first_nonblank(scheduled_overs))) else NA_real_,
  final_runs = sum(total_runs_num, na.rm = TRUE),
  final_wickets = sum(wicket_num, na.rm = TRUE),
  final_legal_balls = sum(legal_num, na.rm = TRUE),
  runs_total_max = max(run_cum, na.rm = TRUE)
), by = .(match_id, innings_no)]
innings_final[!is.finite(scheduled_overs), scheduled_overs := NA_real_]
innings_final[, final_runs := as.numeric(final_runs)]

# Historical baseline is computed on the older 80% of years to reduce look-ahead leakage.
valid_years <- sort(unique(innings_final$year[is.finite(innings_final$year)]))
if (length(valid_years) >= 5) cutoff_year <- as.integer(stats::quantile(valid_years, 0.80, names = FALSE, type = 1)) else cutoff_year <- if (length(valid_years)) max(valid_years) else NA_integer_
base_final <- innings_final[!is.na(year) & (is.na(cutoff_year) | year <= cutoff_year)]
venue_baselines <- base_final[, .(venue_avg_score = mean(final_runs, na.rm = TRUE), baseline_innings = .N), by = .(venue, format_group, innings_no)]
global_baselines <- base_final[, .(global_avg_score = mean(final_runs, na.rm = TRUE)), by = .(format_group, innings_no)]

# Use snapshots at regular legal-ball counts: T20 every 2 overs, ODI every 5, Test at selected intervals.
thresholds <- list(T20 = c(12,24,36,48,60,72,84,96,108), ODI = c(30,60,90,120,150,180,210,240,270),
                   Test = c(30,60,120,180,240,300,360,420,480,540,600))
keep_snapshot <- (DT$format_group == "T20" & DT$legal_cum %in% thresholds$T20) |
                 (DT$format_group == "ODI" & DT$legal_cum %in% thresholds$ODI) |
                 (DT$format_group == "Test" & DT$legal_cum %in% thresholds$Test)
snap <- DT[keep_snapshot, .(
  match_id, innings_no, venue, format_group, year = year_num, match_winner, batting_team,
  legal_cum, current_runs = run_cum, wickets_lost = wickets_cum,
  scheduled_overs, delivery_order_num
)]
if (nrow(snap)) {
  # Select the last delivery at each exact legal-ball count, so wides/no-balls at that count are included.
  data.table::setorder(snap, match_id, innings_no, legal_cum, delivery_order_num)
  snap <- snap[, .SD[.N], by = .(match_id, innings_no, legal_cum)]
  finals_slim <- innings_final[, .(match_id, innings_no, final_runs, final_legal_balls, final_wickets,
                                    final_batting_team = batting_team, final_winner = match_winner,
                                    scheduled_overs = scheduled_overs)]
  snap <- merge(snap, finals_slim, by = c("match_id", "innings_no"), all.x = TRUE, suffixes = c("", "_final"), sort = FALSE)
  snap[, overs_elapsed := legal_cum / 6]
  snap[, current_run_rate := current_runs / pmax(overs_elapsed, 0.2)]
  snap[, recent_runs_30 := NA_real_]
  # Recent 30 legal-ball total only when an exact earlier state exists.
  prev <- DT[DT$legal_cum %in% unique(pmax(0, snap$legal_cum - 30)), .(match_id, innings_no, legal_cum, prior_runs = run_cum, delivery_order_num)]
  if (nrow(prev)) {
    data.table::setorder(prev, match_id, innings_no, legal_cum, delivery_order_num)
    prev <- prev[, .SD[.N], by = .(match_id, innings_no, legal_cum)]
    snap[, prior_key := pmax(0, legal_cum - 30)]
    prev_key <- prev[, .(match_id, innings_no, prior_key = legal_cum, prior_runs)]
    snap <- merge(snap, prev_key, by = c("match_id", "innings_no", "prior_key"), all.x = TRUE, sort = FALSE)
    snap[!is.na(prior_runs), recent_runs_30 := pmax(0, current_runs - prior_runs)]
    snap[is.na(recent_runs_30), recent_runs_30 := current_runs]
    snap[, c("prior_key", "prior_runs") := NULL]
  } else snap[, recent_runs_30 := current_runs]
  snap <- merge(snap, venue_baselines, by = c("venue", "format_group", "innings_no"), all.x = TRUE, sort = FALSE)
  snap <- merge(snap, global_baselines, by = c("format_group", "innings_no"), all.x = TRUE, sort = FALSE)
  snap[, venue_avg_score := data.table::fifelse(is.finite(venue_avg_score), venue_avg_score, global_avg_score)]
  snap[!is.finite(venue_avg_score), venue_avg_score := 0]
  snap <- snap[!is.na(final_runs) & legal_cum < final_legal_balls]
  score_snapshots <- snap[, .(match_id, venue, format_group, year, innings_no, overs_elapsed,
    current_runs, wickets_lost, current_run_rate, recent_runs_30, venue_avg_score, final_runs)]
} else {
  score_snapshots <- data.table::data.table()
}

message("[4/8] Creating historical chase states and recent match outcomes...")
if (nrow(score_snapshots)) {
  first_inn <- innings_final[innings_no == 1, .(match_id, first_score = final_runs, first_team = batting_team,
                                                venue, format_group, year, match_winner)]
  second_inn <- innings_final[innings_no == 2, .(match_id, second_team = batting_team, second_score = final_runs,
                                                  second_balls = final_legal_balls, venue2 = venue, format2 = format_group, year2 = year)]
  match_results <- merge(first_inn, second_inn, by = "match_id", all = FALSE, sort = FALSE)
  match_results[, venue := data.table::fifelse(!is.na(venue) & nzchar(venue), venue, venue2)]
  match_results[, format_group := data.table::fifelse(!is.na(format_group) & nzchar(format_group), format_group, format2)]
  match_results[, year := data.table::fifelse(!is.na(year), year, year2)]
  match_results <- match_results[format_group %in% c("T20", "ODI") & !is.na(match_winner) & nzchar(trimws(match_winner)) &
                                  !is.na(first_team) & !is.na(second_team) & (match_winner == first_team | match_winner == second_team)]
  match_results[, batting_first_win := as.integer(match_winner == first_team)]
  match_results[, chasing_win := as.integer(match_winner == second_team)]
  match_results[, recent_window_date := as.integer(year)]

  first_targets <- innings_final[innings_no == 1, .(match_id, target_runs = final_runs + 1)]
  chase_snapshots <- merge(snap[innings_no == 2], first_targets, by = "match_id", all = FALSE, sort = FALSE)
  chase_snapshots[, scheduled := suppressWarnings(as.numeric(scheduled_overs))]
  chase_snapshots[!is.finite(scheduled) | scheduled <= 0, scheduled := data.table::fifelse(format_group == "T20", 20, 50)]
  chase_snapshots[, balls_remaining := pmax(0, scheduled * 6 - legal_cum)]
  chase_snapshots[, runs_required := pmax(0, target_runs - current_runs)]
  chase_snapshots[, required_run_rate := runs_required / pmax(balls_remaining / 6, 1/6)]
  chase_snapshots[, chase_success := as.integer(!is.na(match_winner) & match_winner == batting_team)]
  chase_snapshots <- chase_snapshots[format_group %in% c("T20", "ODI") & !is.na(match_winner) & nzchar(trimws(match_winner)) &
    legal_cum < final_legal_balls & is.finite(balls_remaining) & match_id %in% match_results$match_id]
  chase_snapshots <- chase_snapshots[, .(match_id, venue, format_group, year = as.integer(year),
    current_runs, wickets_lost, current_run_rate, balls_remaining, runs_required, required_run_rate,
    venue_avg_score, target_runs, chase_success)]
} else {
  match_results <- data.table::data.table()
  chase_snapshots <- data.table::data.table()
}

message("[5/8] Creating bowling-style and recent length recommendation summaries...")
style_summary <- DT[bowler_type %in% c("Pace", "Spin"), .(
  legal_balls = sum(legal_num, na.rm = TRUE),
  deliveries = .N,
  runs_conceded = sum(pmax(0, total_runs_num - byes_num - legbyes_num), na.rm = TRUE),
  wickets_recorded = sum(bowler_credit_num, na.rm = TRUE),
  matches = data.table::uniqueN(match_id)
), by = .(venue, format_group, year = year_num, bowler_type)]
style_summary[, economy_per_over := ifelse(legal_balls > 0, 6 * runs_conceded / legal_balls, NA_real_)]
style_summary[, wickets_per_100_balls := ifelse(legal_balls > 0, 100 * wickets_recorded / legal_balls, NA_real_)]

pace_length_summary <- valid_len[bowler_type == "Pace", .(
  labelled_deliveries = .N,
  wicket_deliveries = sum(wicket_num, na.rm = TRUE),
  total_runs = sum(total_runs_num, na.rm = TRUE),
  legal_balls = sum(legal_num, na.rm = TRUE),
  wicket_rate_pct = 100 * sum(wicket_num, na.rm = TRUE) / .N,
  runs_per_6_balls = 6 * sum(total_runs_num, na.rm = TRUE) / max(1, sum(legal_num, na.rm = TRUE)),
  matches = data.table::uniqueN(match_id)
), by = .(venue, format_group, year = year_num, length_group)]

message("[6/8] Training simple, explainable baseline prediction models...")
score_bundle <- NULL
score_metrics <- data.frame()
if (nrow(score_snapshots) >= 100) {
  sdat <- as.data.frame(score_snapshots)
  sdat$format_group <- factor(sdat$format_group, levels = c("T20", "ODI", "Test"))
  sdat$innings_no <- as.numeric(sdat$innings_no)
  years <- sdat$year[is.finite(sdat$year)]
  test_cut <- if (length(unique(years)) >= 5) as.integer(stats::quantile(unique(years), .80, names = FALSE, type = 1)) else max(years, na.rm = TRUE)
  train <- sdat[is.na(sdat$year) | sdat$year < test_cut, , drop = FALSE]
  test <- sdat[!is.na(sdat$year) & sdat$year >= test_cut, , drop = FALSE]
  if (nrow(train) < 50 || length(unique(stats::na.omit(train$format_group))) < 1) train <- sdat
  score_formula <- final_runs ~ current_runs + wickets_lost + overs_elapsed + current_run_rate + recent_runs_30 + format_group + innings_no + venue_avg_score
  score_model <- stats::lm(score_formula, data = train)
  residuals_test <- if (nrow(test) >= 20) test$final_runs - as.numeric(stats::predict(score_model, newdata = test)) else stats::residuals(score_model)
  residuals_test <- residuals_test[is.finite(residuals_test)]
  err80 <- if (length(residuals_test)) as.numeric(stats::quantile(abs(residuals_test), .80, na.rm = TRUE, names = FALSE)) else 35
  score_bundle <- list(model = score_model, uncertainty80 = max(10, err80), train_rows = nrow(train), test_rows = nrow(test),
                       training_through_year = if (nrow(train)) max(train$year, na.rm = TRUE) else NA_integer_,
                       formula = paste(deparse(score_formula), collapse = ""))
  if (length(residuals_test)) score_metrics <- data.frame(metric = c("MAE", "RMSE", "80% absolute error band"),
    value = c(mean(abs(residuals_test)), sqrt(mean(residuals_test^2)), err80),
    test_rows = length(residuals_test), cutoff_year = test_cut)
}

chase_bundle <- NULL
chase_metrics <- data.frame()
if (nrow(chase_snapshots) >= 100 && length(unique(chase_snapshots$chase_success)) == 2) {
  cdat <- as.data.frame(chase_snapshots)
  cdat$format_group <- factor(cdat$format_group, levels = c("T20", "ODI"))
  yrs <- cdat$year[is.finite(cdat$year)]
  test_cut <- if (length(unique(yrs)) >= 5) as.integer(stats::quantile(unique(yrs), .80, names = FALSE, type = 1)) else max(yrs, na.rm = TRUE)
  train <- cdat[is.na(cdat$year) | cdat$year < test_cut, , drop = FALSE]
  test <- cdat[!is.na(cdat$year) & cdat$year >= test_cut, , drop = FALSE]
  if (nrow(train) < 50 || length(unique(stats::na.omit(train$chase_success))) < 2) train <- cdat
  chase_formula <- chase_success ~ current_runs + wickets_lost + current_run_rate + balls_remaining + runs_required + required_run_rate + format_group
  chase_model <- tryCatch(stats::glm(chase_formula, data = train, family = stats::binomial()), error = function(e) NULL)
  if (!is.null(chase_model)) {
    probs <- if (nrow(test) >= 20) tryCatch(as.numeric(stats::predict(chase_model, newdata = test, type = "response")), error = function(e) numeric()) else numeric()
    if (length(probs) == nrow(test) && nrow(test)) {
      brier <- mean((probs - test$chase_success)^2, na.rm = TRUE)
      chase_metrics <- data.frame(metric = "Brier score (lower is better)", value = brier, test_rows = nrow(test), cutoff_year = test_cut)
    }
    chase_bundle <- list(model = chase_model, train_rows = nrow(train), test_rows = nrow(test),
                         training_through_year = if (nrow(train)) max(train$year, na.rm = TRUE) else NA_integer_,
                         formula = paste(deparse(chase_formula), collapse = ""))
  }
}

message("[7/8] Writing compact caches and model metrics...")
out <- "data/processed"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
saveRDS(length_summary, file.path(out, "enriched_length_summary.rds"), compress = "gzip")
saveRDS(direction_summary, file.path(out, "enriched_direction_summary.rds"), compress = "gzip")
saveRDS(wagon_sample, file.path(out, "enriched_wagon_sample.rds"), compress = "gzip")
saveRDS(coverage, file.path(out, "enriched_coverage_summary.rds"), compress = "gzip")
saveRDS(innings_final, file.path(out, "enriched_innings_final.rds"), compress = "gzip")
saveRDS(match_results, file.path(out, "enriched_match_results.rds"), compress = "gzip")
saveRDS(score_snapshots, file.path(out, "prediction_score_snapshots.rds"), compress = "gzip")
saveRDS(chase_snapshots, file.path(out, "prediction_chase_snapshots.rds"), compress = "gzip")
saveRDS(venue_baselines, file.path(out, "prediction_venue_baselines.rds"), compress = "gzip")
saveRDS(style_summary, file.path(out, "bowling_style_summary.rds"), compress = "gzip")
saveRDS(pace_length_summary, file.path(out, "pace_length_summary.rds"), compress = "gzip")
if (!is.null(score_bundle)) saveRDS(score_bundle, file.path(out, "score_prediction_model.rds"), compress = "gzip")
if (!is.null(chase_bundle)) saveRDS(chase_bundle, file.path(out, "chase_probability_model.rds"), compress = "gzip")
if (nrow(score_metrics)) readr::write_csv(score_metrics, file.path(out, "score_model_evaluation.csv"))
if (nrow(chase_metrics)) readr::write_csv(chase_metrics, file.path(out, "chase_model_evaluation.csv"))

message("[8/8] COMPLETE")
message("  Source delivery rows: ", format(nrow(DT), big.mark = ","))
message("  Summary venues: ", format(data.table::uniqueN(DT$venue), big.mark = ","))
message("  Length summary rows: ", format(nrow(length_summary), big.mark = ","))
message("  Shot-direction summary rows: ", format(nrow(direction_summary), big.mark = ","))
message("  Score snapshots: ", format(nrow(score_snapshots), big.mark = ","))
message("  Chase snapshots: ", format(nrow(chase_snapshots), big.mark = ","))
message("  Completed T20/ODI match outcomes: ", format(nrow(match_results), big.mark = ","))
message("  Bowling-style summary rows: ", format(nrow(style_summary), big.mark = ","))
message("  Cache folder: data/processed")
message("The Shiny app now uses these compact summaries and models, not the multi-million-row source cache.")
