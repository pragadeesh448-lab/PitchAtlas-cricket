# Run from the PitchAtlas project root:
# source("scripts/04_validate_project.R")
# Checks reference files and the bowling-length label handling before analysis.

required_files <- c(
  "app.R",
  "R/02_analysis_helpers.R",
  "data/reference/venue_coordinates.csv",
  "data/reference/pitch_surface_reference.csv",
  "data/reference/ground_dimensions_template.csv"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("PitchAtlas is missing required file(s): ", paste(missing_files, collapse = ", "))
}

source("R/02_analysis_helpers.R")
coordinates <- readr::read_csv("data/reference/venue_coordinates.csv", show_col_types = FALSE)
pitches <- readr::read_csv("data/reference/pitch_surface_reference.csv", show_col_types = FALSE)
dimensions <- readr::read_csv("data/reference/ground_dimensions_template.csv", show_col_types = FALSE)

if (nrow(coordinates) != 49L || dplyr::n_distinct(coordinates$display_name) != 49L) {
  stop("Expected 49 unique venue coordinate records; found ", nrow(coordinates), ".")
}
if (nrow(pitches) != 49L || dplyr::n_distinct(pitches$venue) != 49L) {
  stop("Expected 49 unique pitch reference records; found ", nrow(pitches), ".")
}
if (nrow(dimensions) != 49L || dplyr::n_distinct(dimensions$venue) != 49L) {
  stop("Expected 49 unique ground dimension records; found ", nrow(dimensions), ".")
}
if (any(coordinates$latitude < -90 | coordinates$latitude > 90, na.rm = TRUE) ||
    any(coordinates$longitude < -180 | coordinates$longitude > 180, na.rm = TRUE)) {
  stop("A venue coordinate is outside the valid latitude/longitude range.")
}
if (any(abs(dimensions$pitch_length_m - 20.12) > 0.001, na.rm = TRUE) ||
    any(abs(dimensions$pitch_width_m - 3.05) > 0.001, na.rm = TRUE)) {
  stop("The standard adult pitch dimensions are not consistent across reference rows.")
}

label_test <- normalise_ball_length(c(
  "full toss", "yorker", "full", "good length", "short of a good length", "hard length", "short"
))
expected <- c("Full toss", "Yorker", "Fuller", "Good length", "Short of good length", "Hard length", "Short")
if (!identical(label_test, expected)) {
  stop("Ball-length category validation failed. Got: ", paste(label_test, collapse = " | "))
}

report_count <- sum(!is.na(pitches$source_url) & nzchar(pitches$source_url))
cat("PitchAtlas reference checks passed.\n")
cat("Mapped venue reference: ", nrow(coordinates), " records. Coordinates are approximate starter points.\n", sep = "")
cat("Pitch surface records with linked source: ", report_count, " / ", nrow(pitches), "\n", sep = "")
cat("Ground dimensions reference: ", nrow(dimensions), " records; boundary distances remain unverified where blank.\n", sep = "")
cat("Bowling-length category checks passed.\n")

innings_path <- "data/processed/innings.rds"
if (file.exists(innings_path)) {
  innings <- readRDS(innings_path)
  needed <- c("match_id", "match_date", "match_type", "venue", "innings_number", "innings_runs", "wickets_recorded")
  absent <- setdiff(needed, names(innings))
  if (length(absent)) stop("Processed innings data is missing columns: ", paste(absent, collapse = ", "))
  cat("Local Cricsheet innings summary: ", nrow(innings), " innings from ", dplyr::n_distinct(innings$match_id), " matches.\n", sep = "")
} else {
  cat("No processed innings RDS found. Run source(\"scripts/01_ingest_cricsheet.R\") before launching the Ground Explorer.\n")
}
