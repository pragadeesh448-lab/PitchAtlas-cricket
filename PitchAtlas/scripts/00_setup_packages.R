# Run once in the RStudio Console: source("scripts/00_setup_packages.R")
# Installs the libraries required for PitchAtlas if missing.

required_packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "purrr", "stringr",
  "readr", "jsonlite", "lubridate", "ggplot2", "plotly",
  "leaflet", "DT", "scales"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("All required PitchAtlas packages are already installed.")
}
unavailable <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(unavailable)) {
  stop("Some packages are still unavailable: ", paste(unavailable, collapse = ", "))
}
message("Setup complete. Next run source(\"scripts/01_ingest_cricsheet.R\") to download and process data.")
