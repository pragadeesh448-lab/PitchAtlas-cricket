# Download and parse the official Cricsheet JSON archives.
# Run from the project root: source("scripts/01_ingest_cricsheet.R")

if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
  stop("Please install jsonlite and dplyr first. See README.md.")
}
source(file.path("R", "02_analysis_helpers.R"))

cricsheet_urls <- c(
  tests_male = "https://cricsheet.org/downloads/tests_male_json.zip",
  odis_male = "https://cricsheet.org/downloads/odis_male_json.zip",
  t20s_male = "https://cricsheet.org/downloads/t20s_male_json.zip"
)

ensure_dirs <- function() {
  dirs <- c("data/raw/cricsheet", "data/processed")
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

download_cricsheet_archives <- function(formats = names(cricsheet_urls), force = FALSE) {
  ensure_dirs()
  invalid <- setdiff(formats, names(cricsheet_urls))
  if (length(invalid)) stop("Unknown archive key(s): ", paste(invalid, collapse = ", "))
  for (key in formats) {
    dest <- file.path("data", "raw", "cricsheet", paste0(key, "_json.zip"))
    if (!file.exists(dest) || force) {
      message("Downloading Cricsheet ", key, " archive...")
      utils::download.file(cricsheet_urls[[key]], destfile = dest, mode = "wb", quiet = FALSE)
    } else {
      message("Using cached archive: ", dest)
    }
    extract_dir <- file.path("data", "raw", "cricsheet", key)
    if (!dir.exists(extract_dir) || force) {
      dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(dest, exdir = extract_dir)
    }
  }
  invisible(TRUE)
}

safe_chr <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  as.character(x[[1]])
}

parse_cricsheet_match <- function(path) {
  empty_deliveries <- data.frame()
  empty_innings <- data.frame()
  tryCatch({
    x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    info <- x$info %||% list()
    date_raw <- unlist(info$dates %||% list(NA_character_))
    match_date <- suppressWarnings(as.Date(as.character(date_raw[[1]])))
    match_id <- sub("\\.json$", "", basename(path), ignore.case = TRUE)
    match_type <- safe_chr(info$match_type, "Unknown")
    venue <- safe_chr(info$venue, "Unknown venue")
    city <- safe_chr(info$city, NA_character_)
    gender <- safe_chr(info$gender, NA_character_)
    teams <- unlist(info$teams %||% list())
    team_a <- if (length(teams) >= 1) as.character(teams[[1]]) else NA_character_
    team_b <- if (length(teams) >= 2) as.character(teams[[2]]) else NA_character_
    event_obj <- info$event %||% list()
    event_name <- safe_chr(event_obj$name, NA_character_)
    inn_list <- x$innings %||% list()
    del_rows <- list()
    inn_rows <- list()

    for (i in seq_along(inn_list)) {
      inn <- inn_list[[i]]
      batting_team <- safe_chr(inn$team, NA_character_)
      overs <- inn$overs %||% list()
      total_runs <- 0
      wicket_count <- 0L
      delivery_count <- 0L

      for (ov in overs) {
        over_no <- suppressWarnings(as.integer(ov$over %||% NA_integer_))
        deliveries <- ov$deliveries %||% list()
        if (!length(deliveries)) next
        # Parse one over at a time to avoid creating a full data.frame per ball.
        over_df <- dplyr::bind_rows(lapply(deliveries, function(d) {
          runs <- d$runs %||% list()
          extras_obj <- d$extras %||% list()
          wickets <- d$wickets %||% list()
          wicket_types <- if (length(wickets)) paste(vapply(wickets, function(w) safe_chr(w$kind, "unknown"), character(1)), collapse = ";") else ""
          dismissed <- if (length(wickets)) paste(vapply(wickets, function(w) safe_chr(w$player_out, ""), character(1)), collapse = ";") else ""
          list(
            batter = safe_chr(d$batter, NA_character_),
            bowler = safe_chr(d$bowler, NA_character_),
            runs_batter = suppressWarnings(as.numeric(runs$batter %||% 0)),
            runs_extras = suppressWarnings(as.numeric(runs$extras %||% 0)),
            runs_total = suppressWarnings(as.numeric(runs$total %||% 0)),
            is_wicket = length(wickets) > 0,
            wicket_type = wicket_types,
            player_dismissed = dismissed,
            legal_ball = !any(c("wides", "noballs") %in% names(extras_obj))
          )
        }))
        if (!nrow(over_df)) next
        n_over <- nrow(over_df)
        over_df$match_id <- match_id
        over_df$match_date <- match_date
        over_df$match_type <- match_type
        over_df$gender <- gender
        over_df$venue <- venue
        over_df$city <- city
        over_df$event <- event_name
        over_df$team_a <- team_a
        over_df$team_b <- team_b
        over_df$innings_number <- i
        over_df$batting_team <- batting_team
        over_df$over_number <- over_no
        over_df$delivery_in_over <- seq_len(n_over)
        over_df <- over_df[, c("match_id", "match_date", "match_type", "gender", "venue", "city", "event",
                               "team_a", "team_b", "innings_number", "batting_team", "over_number", "delivery_in_over",
                               "batter", "bowler", "runs_batter", "runs_extras", "runs_total", "is_wicket",
                               "wicket_type", "player_dismissed", "legal_ball")]
        total_runs <- total_runs + sum(over_df$runs_total, na.rm = TRUE)
        wicket_count <- wicket_count + sum(over_df$is_wicket, na.rm = TRUE)
        delivery_count <- delivery_count + n_over
        del_rows[[length(del_rows) + 1L]] <- over_df
      }
      inn_rows[[length(inn_rows) + 1L]] <- data.frame(
        match_id = match_id, match_date = match_date, match_type = match_type,
        gender = gender, venue = venue, city = city, event = event_name,
        team_a = team_a, team_b = team_b, innings_number = i,
        batting_team = batting_team, innings_runs = total_runs,
        wickets_recorded = wicket_count, delivery_records = delivery_count,
        stringsAsFactors = FALSE
      )
    }
    list(
      deliveries = if (length(del_rows)) dplyr::bind_rows(del_rows) else empty_deliveries,
      innings = if (length(inn_rows)) dplyr::bind_rows(inn_rows) else empty_innings,
      error = NULL
    )
  }, error = function(e) {
    message("Skipped ", basename(path), ": ", conditionMessage(e))
    list(deliveries = empty_deliveries, innings = empty_innings, error = conditionMessage(e))
  })
}

process_cricsheet_json <- function(raw_root = "data/raw/cricsheet", output_root = "data/processed") {
  ensure_dirs()
  json_files <- list.files(raw_root, pattern = "\\.json$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(json_files)) stop("No JSON files found. Run download_cricsheet_archives() first.")
  message("Parsing ", length(json_files), " match files. This may take several minutes...")
  parsed <- vector("list", length(json_files))
  for (i in seq_along(json_files)) {
    if (i %% 250 == 0 || i == length(json_files)) message("Processed ", i, " / ", length(json_files))
    parsed[[i]] <- parse_cricsheet_match(json_files[[i]])
  }
  deliveries <- dplyr::bind_rows(lapply(parsed, `[[`, "deliveries"))
  innings <- dplyr::bind_rows(lapply(parsed, `[[`, "innings"))
  if (!nrow(innings)) stop("No innings rows could be parsed. Check the downloaded archives and R package versions.")
  matches <- innings |>
    dplyr::distinct(match_id, match_date, match_type, gender, venue, city, event, team_a, team_b)
  saveRDS(deliveries, file.path(output_root, "deliveries.rds"))
  saveRDS(innings, file.path(output_root, "innings.rds"))
  saveRDS(matches, file.path(output_root, "matches.rds"))
  readr::write_csv(innings, file.path(output_root, "innings_summary.csv"))
  message("Finished. Matches: ", dplyr::n_distinct(matches$match_id),
          " | innings: ", nrow(innings), " | deliveries: ", nrow(deliveries))
  invisible(list(deliveries = deliveries, innings = innings, matches = matches))
}

# Initial coverage: men's Test, ODI and T20 International data.
download_cricsheet_archives()
process_cricsheet_json()
