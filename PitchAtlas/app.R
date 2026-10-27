library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(readr)
library(stringr)
library(scales)
options(shiny.maxRequestSize = 2 * 1024^3, shiny.autoreload = FALSE)
source(file.path("R", "02_analysis_helpers.R"))

THEME <- bs_theme(
  version = 5, bg = "#0c1118", fg = "#e8edf3", primary = "#e7ad50",
  secondary = "#8da1b5", base_font = font_google("DM Sans"),
  heading_font = font_google("Manrope"), code_font = font_google("IBM Plex Mono")
)

load_rds_if_present <- function(filename) {
  if (file.exists(filename)) tryCatch(readRDS(filename), error = function(e) NULL) else NULL
}


# Ground Data must be evidence-only. The reference CSV also contains a 49-ground
# starter list; most rows explicitly say they are not verified. Those rows must
# never appear in this page, its selector, charts, or map.
read_sourced_pitch_profiles <- function() {
  path <- "data/reference/pitch_surface_reference.csv"
  if (!file.exists(path)) return(data.frame())
  x <- tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) data.frame())
  required <- c("venue", "soil_type_reported", "historical_bounce_or_behaviour", "evidence_scope", "source_url", "source_date", "verification_status")
  if (!nrow(x) || length(setdiff(required, names(x)))) return(data.frame())
  
  x <- x |>
    dplyr::mutate(dplyr::across(where(is.character), ~ trimws(.x))) |>
    dplyr::mutate(
      .venue_valid = !is.na(venue) & nzchar(venue),
      .url_valid = !is.na(source_url) & stringr::str_detect(source_url, "^https?://"),
      .date_valid = !is.na(source_date) & nzchar(as.character(source_date)) &
        !stringr::str_detect(stringr::str_to_lower(as.character(source_date)), "not supplied|unknown|pending"),
      .soil_valid = !is.na(soil_type_reported) & nzchar(soil_type_reported) &
        !stringr::str_detect(stringr::str_to_lower(soil_type_reported), "not verified|not supplied|unknown|needs source review|no evidence|not available|n/?a$|^none$"),
      .behaviour_valid = !is.na(historical_bounce_or_behaviour) & nzchar(historical_bounce_or_behaviour) &
        !stringr::str_detect(stringr::str_to_lower(historical_bounce_or_behaviour), "no source-verified|needs a dated|not verified|do not infer|not supplied|no behaviour|not available|unknown|n/?a$|^none$"),
      .scope_valid = !is.na(evidence_scope) & nzchar(evidence_scope) &
        !stringr::str_detect(stringr::str_to_lower(evidence_scope), "not supplied|needs source review|unverified|unknown|not available|n/?a$|^none$"),
      .status_valid = !is.na(verification_status) &
        stringr::str_detect(stringr::str_to_lower(verification_status), "^reported")
    ) |>
    dplyr::filter(.venue_valid, .url_valid, .date_valid, .soil_valid,
                  .behaviour_valid, .scope_valid, .status_valid) |>
    dplyr::select(-dplyr::starts_with(".")) |>
    dplyr::distinct(venue, .keep_all = TRUE) |>
    dplyr::arrange(venue)
  
  x
}

classify_reported_surface <- function(x) {
  z <- stringr::str_to_lower(as.character(x %||% ""))
  dplyr::case_when(
    stringr::str_detect(z, "mixed") & stringr::str_detect(z, "red") & stringr::str_detect(z, "black") ~ "Mixed red/black",
    stringr::str_detect(z, "mixed") ~ "Mixed / layered soil",
    stringr::str_detect(z, "black|clay") ~ "Black / clay-rich",
    stringr::str_detect(z, "red") ~ "Red soil",
    TRUE ~ "Other reported surface"
  )
}

classify_reported_behaviour <- function(x) {
  z <- stringr::str_to_lower(as.character(x %||% ""))
  dplyr::case_when(
    stringr::str_detect(z, "low bounce|comparatively low bounce") ~ "Low bounce mentioned",
    stringr::str_detect(z, "true bounce|truer bounce|good bounce|carry") ~ "Good / true bounce mentioned",
    stringr::str_detect(z, "spin|turn") ~ "Spin / turn mentioned",
    stringr::str_detect(z, "wear|varies|vary|preparation|selected pitch|selected strip|individual strips|surface can change") ~ "Varies with strip / preparation",
    TRUE ~ "General behaviour note"
  )
}

# Return a named palette sized to the actual innings categories in the data.
# Cricsheet may contain super-over innings beyond innings 1-4.
make_innings_palette <- function(values) {
  keys <- unique(as.character(values[!is.na(values)]))
  if (!length(keys)) return(stats::setNames(character(0), character(0)))
  base_cols <- c("#e7ad50", "#66c4b0", "#7798e6", "#e27f6f",
                 "#b49ad8", "#91b85b", "#d88aa4", "#62a8c7",
                 "#d69a72", "#9bb9a8", "#c4b458", "#8c96d8")
  if (length(keys) > length(base_cols)) {
    base_cols <- grDevices::hcl.colors(length(keys), palette = "Dark 3")
  }
  stats::setNames(base_cols[seq_along(keys)], keys)
}

# Read only the columns PitchAtlas needs. The enriched archive has millions of
# rows, so importing all 50 columns at once wastes memory.
normalise_headers <- function(x) {
  x <- gsub("[^a-z0-9]+", "_", tolower(trimws(x)))
  gsub("^_|_$", "", x)
}

read_selected_csv <- function(path, wanted_columns) {
  header <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE,
                                  progress = FALSE, name_repair = "minimal"))
  normalised <- normalise_headers(header)
  
  # This enriched dataset names the delivery key `ball`; older exports may call
  # it `delivery_number`. Select either alias when the app asks for the key.
  effective_wanted <- wanted_columns
  if ("delivery_number" %in% wanted_columns) {
    effective_wanted <- unique(c(effective_wanted, "ball"))
  }
  keep <- which(normalised %in% effective_wanted)
  if (!length(keep)) stop("None of the requested fields were found. Checked: ", paste(effective_wanted, collapse = ", "))
  
  # Read join keys as character, especially `ball` (e.g. 6.10), so decimal
  # parsing cannot silently turn distinct ball labels into the same number.
  key_names <- header[keep[normalised[keep] %in% c("match_id", "innings", "ball", "delivery_number")]]
  type_args <- setNames(rep(list(readr::col_character()), length(key_names)), key_names)
  type_args$.default <- readr::col_guess()
  column_spec <- do.call(readr::cols, type_args)
  
  dat <- readr::read_csv(path, col_select = dplyr::all_of(header[keep]),
                         col_types = column_spec, show_col_types = FALSE,
                         progress = TRUE, name_repair = "minimal", guess_max = 10000)
  names(dat) <- normalised[keep]
  if (!"delivery_number" %in% names(dat) && "ball" %in% names(dat)) {
    names(dat)[names(dat) == "ball"] <- "delivery_number"
  }
  dat
}

normalise_delivery_keys <- function(x) {
  key_cols <- c("match_id", "innings", "delivery_number")
  for (nm in key_cols) {
    if (!nm %in% names(x)) stop("Required join key missing: ", nm)
    val <- trimws(as.character(x[[nm]]))
    # Match IDs and innings may be parsed as integer-like strings. Keep the
    # delivery_number exactly as text: stripping trailing zeroes could turn
    # a delivery key such as 6.10 into 6.1 and cause a wrong join.
    if (nm %in% c("match_id", "innings")) val <- sub("\\.0+$", "", val)
    x[[nm]] <- val
  }
  x
}


# Search the standard project location first, then the user's existing
# Downloads/archive folder. This makes the enriched data local and persistent:
# users do not need to select the CSV files through the browser on each run.
find_local_enriched_file <- function(filename) {
  profile <- Sys.getenv("USERPROFILE", unset = "")
  roots <- c(
    "data/raw/enriched", "data/raw", "data/enriched",
    "../../archive", "../archive", "archive"
  )
  if (nzchar(profile)) roots <- c(roots, file.path(profile, "Downloads", "archive"))
  roots <- unique(roots)
  candidate_names <- unique(c(filename, tools::file_path_sans_ext(filename)))
  candidates <- unique(unlist(lapply(roots, function(root) file.path(root, candidate_names)), use.names = FALSE))
  found <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (!length(found)) return(NA_character_)
  normalizePath(found[[1]], winslash = "/", mustWork = TRUE)
}

load_local_enriched_data <- function(force = FALSE, progress = function(value, detail) {}) {
  # Shiny must never parse the 1.68 GB CSV during app startup. The source files
  # are prepared once by scripts/05_prepare_enriched_data.R and cached as a compact RDS.
  cache_path <- "data/processed/enriched_ball_data.rds"
  progress(0.10, "Checking the prepared enriched-data cache...")
  if (!file.exists(cache_path)) {
    stop(paste0(
      "Prepared enriched-data cache not found. First stop the Shiny app, then run ",
      "source(\"scripts/05_prepare_enriched_data.R\") once in the RStudio Console. ",
      "This keeps the large source CSV out of app startup."
    ))
  }
  progress(0.35, "Reading the prepared local cache...")
  cached <- tryCatch(readRDS(cache_path), error = function(e) {
    stop("Could not read ", cache_path, ": ", conditionMessage(e))
  })
  required <- c("venue", "format_group", "ball_length", "length_group", "is_wicket",
                "shot_direction", "runs_for_direction")
  missing <- setdiff(required, names(cached))
  cache_version <- attr(cached, "pitchatlas_cache_version") %||% NA_integer_
  if (!is.data.frame(cached) || !nrow(cached) || length(missing) || !identical(cache_version, 6L)) {
    stop("Prepared cache is missing required fields or is from an older version. Missing fields: ",
         if (length(missing)) paste(missing, collapse = ", ") else "none",
         ". Stop the app and run source(\"scripts/05_prepare_enriched_data.R\") again.")
  }
  progress(0.80, "Refreshing filters from the prepared cache...")
  list(
    data = cached,
    source = "data/processed/enriched_ball_data.rds",
    cached = TRUE,
    missing_venue_rows = attr(cached, "pitchatlas_missing_venue_rows") %||% 0L,
    tracking_joined = isTRUE(attr(cached, "pitchatlas_tracking_joined"))
  )
}

sourced_pitch_profiles <- read_sourced_pitch_profiles()
ground_profile_choices <- if (nrow(sourced_pitch_profiles)) stats::setNames(sourced_pitch_profiles$venue, sourced_pitch_profiles$venue) else character(0)

# Prime the Ground Explorer selector before the browser session starts. This avoids
# an initially blank Selectize value being interpreted as a real venue filter.
.initial_innings <- load_rds_if_present("data/processed/innings.rds")
.initial_venues <- if (is.data.frame(.initial_innings) && "venue" %in% names(.initial_innings)) {
  z <- trimws(as.character(.initial_innings$venue))
  sort(unique(z[!is.na(z) & nzchar(z)]))
} else character(0)
ground_filter_choices <- c("All grounds" = "All grounds", stats::setNames(.initial_venues, .initial_venues))
rm(.initial_innings, .initial_venues)

ui <- page_navbar(
  title = div(span("PITCH", class = "brand-pitch"), span("ATLAS", class = "brand-atlas")),
  id = "main_nav", theme = THEME, fillable = FALSE,
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "pitchatlas.css"),
      tags$style(HTML("html, body { height: auto !important; min-height: 100%; overflow-y: auto !important; overflow-x: hidden; } .bslib-page-navbar { height: auto !important; min-height: 100vh; overflow: visible !important; } .bslib-page-navbar .tab-content, .bslib-page-navbar .tab-pane { height: auto !important; min-height: 0 !important; overflow: visible !important; } .bslib-sidebar-layout { height: auto !important; min-height: 0 !important; overflow: visible !important; } .app-footer { position: static !important; bottom: auto !important; } .ground-data-metrics { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:12px; margin: 0 0 18px; } .ground-data-metric { background:#111923; border:1px solid #263342; border-radius:14px; padding:18px; min-width:0; border-top:3px solid #e7ad50; } .ground-data-metric.metric-teal { border-top-color:#66c4b0; } .ground-data-metric.metric-blue { border-top-color:#7798e6; } .ground-data-metric.metric-purple { border-top-color:#b49ad8; } .ground-data-metric span { display:block; color:#8da1b5; font-size:10px; letter-spacing:.09em; font-weight:700; } .ground-data-metric h3 { font-size:30px; color:#e8edf3; font-weight:800; margin:8px 0 4px; } .ground-data-metric small { color:#8da1b5; font-size:11px; } .ground-profile-visual { height:100%; background:linear-gradient(150deg,#111923,#101b17); border:1px solid #263342; border-radius:14px; padding:22px; } .ground-profile-kicker { color:#e7ad50; font-size:10px; font-weight:800; letter-spacing:.13em; } .ground-profile-top h3 { font-size:26px; font-weight:800; margin:10px 0 12px; } .ground-profile-badge { display:inline-block; border:1px solid #466754; color:#a8d7bd; background:#15271d; border-radius:999px; padding:7px 10px; font-size:11px; font-weight:700; } .ground-profile-fact { margin-top:18px; padding:15px; border:1px solid #2a3a32; border-radius:10px; background:#0c1411; } .ground-profile-fact span { display:block; font-size:9px; letter-spacing:.12em; color:#8da1b5; font-weight:700; margin-bottom:8px; } .ground-profile-fact strong { display:block; color:#e7ad50; font-size:17px; } .ground-profile-fact p { color:#c2ced8; font-size:12px; line-height:1.75; margin:7px 0 0; } .ground-profile-meta { display:grid; grid-template-columns:1fr; gap:9px; margin-top:16px; } .ground-profile-meta div { display:flex; flex-direction:column; gap:4px; border-bottom:1px solid #263342; padding:8px 0; } .ground-profile-meta span { color:#8195a9; font-size:9px; letter-spacing:.08em; text-transform:uppercase; } .ground-profile-meta strong { color:#d6e0e8; font-size:11px; font-weight:600; line-height:1.5; } .ground-source-link { display:inline-block; margin-top:17px; color:#e7ad50; font-weight:800; font-size:12px; } .ground-empty-state { border:1px solid #263342; border-radius:12px; padding:22px; color:#9aabba; } .ground-empty-state h3 { color:#e7ad50; font-size:17px; } @media(max-width:900px){.ground-data-metrics{grid-template-columns:repeat(2,minmax(0,1fr));}} @media(max-width:560px){.ground-data-metrics{grid-template-columns:1fr 1fr; gap:8px;} .ground-data-metric{padding:12px;} .ground-data-metric h3{font-size:24px;}}")),
      tags$script(HTML("function togglePitchAtlasFullscreen(){if(!document.fullscreenElement){var p=document.documentElement.requestFullscreen();if(p&&p.catch){p.catch(function(){alert('Full-screen is unavailable in this browser window. Click Open in Browser and try again.');});}}else{document.exitFullscreen();}}document.addEventListener('fullscreenchange',function(){document.querySelectorAll('.module-expand-button').forEach(function(b){b.textContent=document.fullscreenElement?'⤢ Exit full screen':'⛶ Expand page';});});"))
    ),
    div(class = "hero-strip",
        div(class = "eyebrow", "GLOBAL CRICKET · HISTORICAL DATA · R SHINY"),
        h1("Understand the ground behind the game."),
        p("Explore innings scoring, venue history and delivery-length outcomes. Every metric is tied to its data coverage."))
  ),
  nav_panel("Ground Explorer", value = "grounds",
            layout_sidebar(
              fillable = FALSE,
              sidebar = sidebar(
                selectInput("format", "Match format", choices = c("All formats", "Test", "ODI", "T20"), selected = "All formats"),
                selectizeInput("venue", "Ground", choices = ground_filter_choices, selected = "All grounds", options = list(placeholder = "Choose a venue", allowEmptyOption = FALSE, dropdownParent = "body")),
                sliderInput("years", "Year range", min = 2000, max = 2026, value = c(2000, 2026), sep = ""),
                actionButton("reload", "Reload processed data", class = "btn-warning w-100"),
                hr(),
                div(class = "sidebar-note", strong("Data setup"),
                    p("Cricsheet match data has been processed locally. If you need to rebuild it, run source(\"scripts/01_ingest_cricsheet.R\") in the RStudio Console.")),
                width = 300
              ),
              div(class = "metric-grid",
                  div(class = "metric-card", span("MATCHES IN FILTER", class = "metric-label"), textOutput("match_count", inline = TRUE)),
                  div(class = "metric-card", span("GROUND COVERAGE", class = "metric-label"), textOutput("ground_count", inline = TRUE)),
                  div(class = "metric-card", span("INNINGS RECORDS", class = "metric-label"), textOutput("innings_count", inline = TRUE))),
              layout_columns(
                card(class = "atlas-card", full_screen = TRUE, card_header("Average score by innings"), plotlyOutput("innings_chart", height = "500px")),
                card(class = "atlas-card", full_screen = TRUE, card_header("World venue map · 49 venue markers (approximate coordinates)"), leafletOutput("venue_map", height = "460px")),
                col_widths = c(7, 5)
              ),
              card(class = "atlas-card", full_screen = TRUE,
                   card_header("Year-by-year ground behaviour"),
                   plotlyOutput("year_chart", height = "460px")),
              card(class = "atlas-card", full_screen = TRUE,
                   card_header("Animated world venue map · historical innings scoring"),
                   div(class = "sidebar-note",
                       p("Choose a format and year range, then press Play. Marker size scales with the selected year's average innings score. All 49 reference venues remain on the map; venues without a local match in that year appear in grey.")),
                   selectInput("map_format", "Animation format", choices = c("T20", "ODI", "Test"), selected = "T20"),
                   sliderInput("animated_year_range", "Years to animate", min = 2000, max = 2026, value = c(2000, 2026), sep = ""),
                   sliderInput("animated_year", "Current year", min = 2000, max = 2026, value = 2026, sep = ""),
                   actionButton("toggle_map_animation", "Play animation", class = "btn-warning"),
                   leafletOutput("animated_map", height = "620px"))
            )
  ),
  nav_panel("Compare Grounds", value = "compare",
            layout_sidebar(
              fillable = FALSE,
              sidebar = sidebar(
                selectInput("compare_format", "Format", choices = c("Test", "ODI", "T20"), selected = "T20"),
                selectizeInput("compare_venues", "Grounds to compare", choices = character(0), multiple = TRUE, options = list(placeholder = "Choose up to 5 grounds", maxItems = 5, dropdownParent = "body")),
                sliderInput("compare_years", "Year range", min = 2000, max = 2026, value = c(2000, 2026), sep = ""),
                width = 300
              ),
              card(class = "atlas-card", full_screen = TRUE,
                   card_header("Venue average by innings position"), plotlyOutput("compare_chart", height = "440px")),
              card(class = "atlas-card", full_screen = TRUE,
                   card_header("Underlying summary table"), DTOutput("summary_table"))
            )
  ),
  nav_panel("Bowling Length", value = "length",
            div(class = "module-intro module-intro-expanded",
                style = "display:flex;align-items:flex-start;justify-content:space-between;gap:1rem;flex-wrap:wrap;",
                div(style = "flex:1;min-width:300px;",
                    h2("What happens by delivery length?"),
                    p("Prepare the enriched archive once by running source(\"scripts/05_prepare_enriched_data.R\") in the RStudio Console. PitchAtlas then reads the prepared cache, builds the actual ground list and calculates ground-specific results without re-reading the 1.68 GB CSV at each launch."),
                    p("The source distinguishes full toss, yorker, full/full-pitched, good length, short of a good length/bouncer and short. PitchAtlas keeps source labels separate; it does not fabricate a hard-length category when the source does not record one."),
                    p(tags$a(href = "https://www.kaggle.com/datasets/ariadaikalam/the-ultimate-ball-by-ball-cricket-dataset", target = "_blank", rel = "noreferrer", "Download enriched ball-by-ball dataset on Kaggle ↗"))),
                tags$button(type = "button", class = "btn btn-outline-warning module-expand-button",
                            onclick = "togglePitchAtlasFullscreen()", style = "white-space:nowrap;margin-top:.5rem;", "⛶ Expand page")),
            layout_sidebar(
              fillable = FALSE,
              sidebar = sidebar(
                p(class = "sidebar-note",
                  "The app reads compact local summaries. Run the two one-time setup scripts in Sources & Methods only when the source dataset changes. No browser uploads are required."),
                actionButton("reload_enriched_data", "Reload prepared local data", class = "btn-warning w-100"),
                uiOutput("length_load_status"),
                selectizeInput("length_venue", "Bowling-length ground filter", choices = "All grounds", selected = "All grounds", options = list(placeholder = "Choose a ground", dropdownParent = "body")),
                selectInput("length_format", "Format filter", choices = c("All formats"), selected = "All formats"),
                selectizeInput("run_venue", "Run-direction ground", choices = "All stadiums", selected = "All stadiums", options = list(placeholder = "All stadiums", dropdownParent = "body")),
                width = 300
              ),
              layout_columns(
                card(class = "atlas-card", full_screen = TRUE,
                     card_header("Wicket-delivery rate by length"), plotlyOutput("length_chart", height = "390px")),
                card(class = "atlas-card", full_screen = TRUE,
                     card_header("Coverage & interpretation"), uiOutput("length_coverage")),
                col_widths = c(8, 4)
              ),
              layout_columns(
                card(class = "atlas-card", full_screen = TRUE,
                     card_header("Pitch length-zone schematic · not to scale"),
                     plotlyOutput("pitch_zone_map", height = "430px")),
                col_widths = 12
              ),
              layout_columns(
                card(class = "atlas-card", full_screen = TRUE,
                     card_header("Runs by recorded shot direction · wagon-wheel style"),
                     plotlyOutput("wagon_wheel", height = "480px")),
                card(class = "atlas-card", full_screen = TRUE,
                     card_header("Share of batter runs by recorded shot direction"),
                     plotlyOutput("run_direction_pie", height = "480px")),
                col_widths = c(6, 6)
              ),
              card(class = "atlas-card", full_screen = TRUE, card_header("Length summary"), DTOutput("length_table"))
            )
  ),
  nav_panel("Match Predictions", value = "predictions",
            div(class = "module-intro",
                h2("Predict an innings outcome"),
                p("Use an innings snapshot to estimate the final score and, for limited-overs chases, the probability of reaching the target. Forecasts come from a time-split historical model and show uncertainty; they are not guarantees.")),
            layout_sidebar(
              fillable = FALSE,
              sidebar = sidebar(
                selectInput("pred_format", "Match format", choices = c("T20", "ODI", "Test"), selected = "T20"),
                selectizeInput("pred_venue", "Ground baseline", choices = c("All grounds"), selected = "All grounds", options = list(placeholder = "Choose a ground", dropdownParent = "body")),
                selectInput("pred_innings", "Innings position", choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4), selected = "1"),
                numericInput("pred_overs", "Overs completed", value = 10, min = 0, max = 200, step = 1),
                numericInput("pred_runs", "Current runs", value = 85, min = 0, max = 1000, step = 1),
                numericInput("pred_wickets", "Wickets lost", value = 2, min = 0, max = 10, step = 1),
                numericInput("pred_recent_runs", "Runs in most recent 5 overs (or last 30 legal balls)", value = 42, min = 0, max = 300, step = 1),
                hr(),
                h4("Chase scenario (ODI/T20 only)"),
                numericInput("pred_target", "Target runs", value = 180, min = 1, max = 1000, step = 1),
                numericInput("pred_balls_remaining", "Legal balls remaining", value = 60, min = 0, max = 300, step = 1),
                actionButton("calculate_forecast", "Calculate forecast", class = "btn-warning w-100"),
                width = 330
              ),
              uiOutput("prediction_summary"),
              layout_columns(
                card(class = "atlas-card", full_screen = TRUE, card_header("Historical score states and model forecast"), plotlyOutput("prediction_score_chart", height = "420px")),
                card(class = "atlas-card", full_screen = TRUE, card_header("Chase probability vs recent baseline"), plotlyOutput("prediction_chase_chart", height = "420px")),
                col_widths = c(7, 5)
              ),
              card(class = "atlas-card", full_screen = TRUE, card_header("Model evaluation · held-out later matches"), uiOutput("prediction_evaluation"))
            )
  ),
  nav_panel("Strategy Recommendations", value = "strategy",
            div(class = "module-intro",
                h2("Evidence-based match strategy"),
                p("Compare recorded spin and pace performance at a ground over a recent window. Length advice uses only source-labelled fast-bowling deliveries and displays sample sizes. Recommendations are conditional on coverage and are not guaranteed tactics.")),
            layout_sidebar(
              fillable = FALSE,
              sidebar = sidebar(
                selectizeInput("strategy_venue", "Ground", choices = c("All grounds"), selected = "All grounds", options = list(placeholder = "Choose a ground", dropdownParent = "body")),
                selectInput("strategy_format", "Format", choices = c("T20", "ODI", "Test"), selected = "T20"),
                selectInput("strategy_window", "Recent period", choices = c("Last 3 years" = 3, "Last 5 years" = 5, "Last 10 years" = 10, "All available years" = 999), selected = 5),
                numericInput("strategy_min_balls", "Minimum legal balls per style", value = 300, min = 30, max = 100000, step = 30),
                width = 300
              ),
              uiOutput("strategy_recommendation"),
              layout_columns(
                card(class = "atlas-card", full_screen = TRUE, card_header("Economy rate by bowling type"), plotlyOutput("strategy_economy_chart", height = "330px")),
                card(class = "atlas-card", full_screen = TRUE, card_header("Recorded wicket rate by bowling type"), plotlyOutput("strategy_wicket_chart", height = "330px")),
                col_widths = c(6, 6)
              ),
              card(class = "atlas-card", full_screen = TRUE, card_header("Fast-bowling length advice · recent recorded deliveries"), plotlyOutput("strategy_length_chart", height = "380px")),
              card(class = "atlas-card", full_screen = TRUE, card_header("Evidence behind the recommendation"), DTOutput("strategy_evidence_table"))
            )
  ),
  nav_panel("Recent Match Trends", value = "trends",
            div(class = "module-intro",
                h2("Recent batting-first vs chasing outcomes"),
                p("This view uses completed ODI/T20 matches from the selected recent window, not the entire historical archive by default. Ties, no-results and matches without a recorded winner are excluded from the win-rate denominator.")),
            layout_sidebar(
              fillable = FALSE,
              sidebar = sidebar(
                selectInput("trend_format", "Format", choices = c("T20", "ODI"), selected = "T20"),
                selectizeInput("trend_venue", "Ground", choices = c("All grounds"), selected = "All grounds", options = list(placeholder = "Choose a ground", dropdownParent = "body")),
                selectInput("trend_window", "Recent window", choices = c("Last 1 year" = 1, "Last 3 years" = 3, "Last 5 years" = 5, "Last 10 years" = 10), selected = 3),
                width = 300
              ),
              uiOutput("recent_trend_summary"),
              layout_columns(
                card(class = "atlas-card", full_screen = TRUE, card_header("Batting-first vs chasing win percentage"), plotlyOutput("recent_trend_chart", height = "390px")),
                card(class = "atlas-card", full_screen = TRUE, card_header("Year-by-year recent pattern"), plotlyOutput("recent_trend_year_chart", height = "390px")),
                col_widths = c(6, 6)
              ),
              card(class = "atlas-card", full_screen = TRUE, card_header("Matches supporting the percentages"), DTOutput("recent_trend_table"))
            )
  ),
  nav_panel("Ground Data", value = "ground_data",
            div(class = "module-intro module-intro-expanded",
                style = "display:flex;align-items:flex-start;justify-content:space-between;gap:1rem;flex-wrap:wrap;",
                div(style = "flex:1;min-width:300px;",
                    h2("Ground intelligence · sourced venues only"),
                    p("Only grounds with a valid source URL, source date, reported surface, behaviour note and evidence scope appear here. Rows with missing or placeholder evidence are excluded from this selector, map and charts. Notes may be historical or match-specific, not live pitch measurements."),
                    p("No venue-specific straight or square boundary measurements are source-verified in the current reference, so blank boundary tables are intentionally omitted.")),
                tags$button(type = "button", class = "btn btn-outline-warning module-expand-button",
                            onclick = "togglePitchAtlasFullscreen()", style = "white-space:nowrap;margin-top:.5rem;", "⛶ Expand page")),
            uiOutput("ground_data_kpis"),
            layout_columns(
              card(class = "atlas-card", full_screen = TRUE,
                   card_header("Reported surface / soil mix · sourced profiles"),
                   plotlyOutput("ground_surface_chart", height = "340px")),
              card(class = "atlas-card", full_screen = TRUE,
                   card_header("Bounce and behaviour themes in sourced reports"),
                   plotlyOutput("ground_bounce_chart", height = "340px")),
              col_widths = c(5, 7)
            ),
            layout_sidebar(
              fillable = FALSE,
              sidebar = sidebar(
                selectInput("profile_venue", "Choose a sourced ground", choices = ground_profile_choices,
                            selected = if (length(ground_profile_choices)) unname(ground_profile_choices[[1]]) else NULL),
                div(class = "sidebar-note", strong("Evidence rule"),
                    p("Only entries with a linked report appear in this selector. The charts summarize the wording in those reports; they do not estimate current bounce from scores.")),
                width = 300
              ),
              layout_columns(
                uiOutput("pitch_profile_detail"),
                card(class = "atlas-card", full_screen = TRUE,
                     card_header("Historical innings scoring · selected sourced ground"),
                     plotlyOutput("venue_behaviour_chart", height = "420px")),
                col_widths = c(5, 7)
              ),
              card(class = "atlas-card", full_screen = TRUE,
                   card_header("Map · only grounds with linked pitch reports"),
                   leafletOutput("sourced_ground_map", height = "450px"))
            )
  ),
  nav_panel("Sources & Methods", value = "sources",
            div(class = "method-wrap",
                h2("Data before conclusions."),
                p("PitchAtlas separates source-observed fields from indicators derived in R."),
                layout_columns(
                  card(class = "atlas-card", card_header("Match and delivery records"),
                       p("Cricsheet official JSON archives: Tests, ODIs and men's T20 internationals."),
                       tags$a(href = "https://cricsheet.org/downloads/", target = "_blank", "Open Cricsheet downloads ↗")),
                  card(class = "atlas-card", card_header("Delivery lengths and wagon-wheel records"),
                       p("The optional enriched dataset separates Cricsheet facts from Cricinfo tracking. Actual length labels, shot direction and wagon-wheel coordinates are present only where recorded."),
                       tags$a(href = "https://www.kaggle.com/datasets/ariadaikalam/the-ultimate-ball-by-ball-cricket-dataset", target = "_blank", rel = "noreferrer", "Download enriched data ↗")),
                  card(class = "atlas-card", card_header("Prediction and recommendation methods"),
                       tags$ul(tags$li("Final-score model is trained on over-state snapshots and evaluated on later-dated held-out matches."),
                               tags$li("Chase probability uses limited-overs second-innings states where a match winner is recorded; it is unavailable for Tests."),
                               tags$li("Recent batting-first/chasing percentages exclude ties, no-results and unknown winners."),
                               tags$li("Spin/pace recommendations require minimum sample sizes and compare observed economy and wicket rates; they are not causal guarantees."),
                               tags$li("Fast-bowling length tips use recorded length-labelled deliveries only."),
                               tags$li("Soil/bounce notes are historical or match-specific reports, not a live strip inspection."),
                               tags$li("Missing venue coordinates and unverified boundary dimensions remain missing.")))
                )
            )
  ),
  footer = div(class = "app-footer", "PITCHATLAS · DATA VISUALIZATION PROJECT · R / SHINY · NO UNSOURCED GROUND MEASUREMENTS")
)

server <- function(input, output, session) {
  # Only the compact innings summary is needed by the Ground Explorer. Do not
  # load the multi-million-row deliveries RDS into memory unless a module uses it.
  data_state <- reactiveValues(
    innings = load_rds_if_present("data/processed/innings.rds"),
    status = "Processed Cricsheet data not loaded yet. Run scripts/01_ingest_cricsheet.R in RStudio."
  )
  # Compact summaries are loaded by Shiny; millions of raw delivery rows stay out of the reactive path.
  length_data <- reactiveVal(NULL) # retained only for backwards-compatible status handling
  length_summary_state <- reactiveVal(data.frame())
  direction_summary_state <- reactiveVal(data.frame())
  wagon_sample_state <- reactiveVal(data.frame())
  length_coverage_state <- reactiveVal(data.frame())
  length_status <- reactiveVal("Searching for prepared cricket summaries...")
  prediction_state <- reactiveValues(
    score_model = load_rds_if_present("data/processed/score_prediction_model.rds"),
    chase_model = load_rds_if_present("data/processed/chase_probability_model.rds"),
    score_snapshots = load_rds_if_present("data/processed/prediction_score_snapshots.rds"),
    chase_snapshots = load_rds_if_present("data/processed/prediction_chase_snapshots.rds"),
    venue_baselines = load_rds_if_present("data/processed/prediction_venue_baselines.rds"),
    match_results = load_rds_if_present("data/processed/enriched_match_results.rds"),
    bowling_style = load_rds_if_present("data/processed/bowling_style_summary.rds"),
    pace_length = load_rds_if_present("data/processed/pace_length_summary.rds"),
    score_metrics = if (file.exists("data/processed/score_model_evaluation.csv")) tryCatch(readr::read_csv("data/processed/score_model_evaluation.csv", show_col_types = FALSE), error=function(e) data.frame()) else data.frame(),
    chase_metrics = if (file.exists("data/processed/chase_model_evaluation.csv")) tryCatch(readr::read_csv("data/processed/chase_model_evaluation.csv", show_col_types = FALSE), error=function(e) data.frame()) else data.frame()
  )
  
  
  observeEvent(input$reload, {
    data_state$innings <- load_rds_if_present("data/processed/innings.rds")
    if (!is.null(data_state$innings) && nrow(data_state$innings) > 0) {
      data_state$status <- paste("Loaded", format(nrow(data_state$innings), big.mark = ","), "innings records.")
    } else {
      data_state$status <- "No processed records found. Run scripts/01_ingest_cricsheet.R in RStudio first."
    }
    showNotification(data_state$status, type = if (!is.null(data_state$innings)) "message" else "warning")
  })
  
  # Reactive flag so Compare Grounds only auto-selects default 3 grounds on the very first initialization
  compare_venues_initial_set <- reactiveVal(FALSE)

  observeEvent(data_state$innings, {
    inn <- data_state$innings
    if (is.null(inn) || !nrow(inn)) return()
    venues <- trimws(as.character(inn$venue))
    venues <- sort(unique(venues[!is.na(venues) & nzchar(venues)]))
    venue_choices <- c("All grounds" = "All grounds", stats::setNames(venues, venues))
    
    cur_venue <- isolate(input$venue)
    sel_venue <- if (!is.null(cur_venue) && cur_venue %in% venues) cur_venue else "All grounds"
    updateSelectizeInput(session, "venue", choices = venue_choices, selected = sel_venue, server = FALSE)
    
    pred_venues <- if (is.data.frame(prediction_state$venue_baselines) && nrow(prediction_state$venue_baselines)) sort(unique(as.character(prediction_state$venue_baselines$venue))) else venues
    cur_pred <- isolate(input$pred_venue)
    sel_pred <- if (!is.null(cur_pred) && cur_pred %in% pred_venues) cur_pred else "All grounds"
    updateSelectizeInput(session, "pred_venue", choices = c("All grounds", pred_venues), selected = sel_pred, server = FALSE)
    
    trend_venues <- if (is.data.frame(prediction_state$match_results) && nrow(prediction_state$match_results)) sort(unique(as.character(prediction_state$match_results$venue))) else venues
    cur_trend <- isolate(input$trend_venue)
    sel_trend <- if (!is.null(cur_trend) && cur_trend %in% trend_venues) cur_trend else "All grounds"
    updateSelectizeInput(session, "trend_venue", choices = c("All grounds", trend_venues), selected = sel_trend, server = FALSE)
    
    strategy_venues <- if (is.data.frame(prediction_state$bowling_style) && nrow(prediction_state$bowling_style)) sort(unique(as.character(prediction_state$bowling_style$venue))) else venues
    cur_strat <- isolate(input$strategy_venue)
    sel_strat <- if (!is.null(cur_strat) && cur_strat %in% strategy_venues) cur_strat else "All grounds"
    updateSelectizeInput(session, "strategy_venue", choices = c("All grounds", strategy_venues), selected = sel_strat, server = FALSE)
    
    years <- lubridate::year(inn$match_date)
    years <- years[is.finite(years)]
    if (length(years)) {
      min_y <- min(years)
      max_y <- max(years)
      cur_y <- isolate(input$years)
      val_y <- if (!is.null(cur_y) && length(cur_y) == 2L && all(is.finite(cur_y))) {
        c(max(min_y, cur_y[1]), min(max_y, cur_y[2]))
      } else c(min_y, max_y)
      updateSliderInput(session, "years", min = min_y, max = max_y, value = val_y)
      
      cur_cy <- isolate(input$compare_years)
      val_cy <- if (!is.null(cur_cy) && length(cur_cy) == 2L && all(is.finite(cur_cy))) {
        c(max(min_y, cur_cy[1]), min(max_y, cur_cy[2]))
      } else c(min_y, max_y)
      updateSliderInput(session, "compare_years", min = min_y, max = max_y, value = val_cy)
    }
  }, ignoreNULL = TRUE)

  # Ground Explorer: dynamically filter ground choices by selected format and year range
  observeEvent(
    list(input$format, input$years),
    {
      inn <- data_state$innings
      if (is.null(inn) || !nrow(inn)) return()
      fmt <- input$format
      x <- inn
      if (length(fmt) == 1L && !is.na(fmt) && nzchar(trimws(as.character(fmt))) &&
          fmt %in% c("Test", "ODI", "T20")) {
        x <- dplyr::filter(x, format_label(match_type) == fmt)
      }
      yr <- input$years
      if (!is.null(yr) && length(yr) == 2L && all(is.finite(yr))) {
        x <- dplyr::filter(x, lubridate::year(match_date) >= yr[1], lubridate::year(match_date) <= yr[2])
      }
      venues <- sort(unique(trimws(as.character(x$venue))))
      venues <- venues[!is.na(venues) & nzchar(venues)]
      venue_choices <- c("All grounds" = "All grounds", stats::setNames(venues, venues))
      
      cur_venue <- isolate(input$venue)
      sel_venue <- if (!is.null(cur_venue) && cur_venue %in% venues) cur_venue else "All grounds"
      updateSelectizeInput(session, "venue", choices = venue_choices, selected = sel_venue, server = FALSE)
    },
    ignoreInit = TRUE
  )
  
  # Compare Grounds: show only venues with matching records.
  observeEvent(
    list(input$compare_format, input$compare_years, data_state$innings),
    {
      inn <- data_state$innings
      if (is.null(inn) || !nrow(inn)) return()

      fmt <- input$compare_format
      if (is.null(fmt) || length(fmt) != 1L || is.na(fmt) || !nzchar(fmt)) fmt <- "T20"

      x <- dplyr::mutate(
        inn,
        format_group = format_label(match_type),
        year = lubridate::year(match_date)
      )
      x <- dplyr::filter(x, format_group == fmt)
      if (fmt != "Test") x <- dplyr::filter(x, innings_number <= 2)

      yrs <- input$compare_years
      if (!is.null(yrs) && length(yrs) == 2L && all(is.finite(yrs))) {
        x <- dplyr::filter(x, year >= yrs[1], year <= yrs[2])
      }

      venues <- sort(unique(trimws(as.character(x$venue))))
      venues <- venues[!is.na(venues) & nzchar(venues)]

      selected <- isolate(input$compare_venues)
      if (is.null(selected)) selected <- character(0)
      selected <- intersect(as.character(selected), venues)

      if (!compare_venues_initial_set() && !length(selected) && length(venues)) {
        selected <- head(venues, min(3L, length(venues)))
        compare_venues_initial_set(TRUE)
      }

      updateSelectizeInput(session, "compare_venues",
        choices = venues, selected = selected, server = FALSE)
    },
    ignoreInit = FALSE
  )

  filtered_innings <- reactive({
    x <- data_state$innings
    if (is.null(x) || !nrow(x)) return(data.frame())
    x <- x |> mutate(format_group = format_label(match_type), year = lubridate::year(match_date))
    
    # Ignore transient empty values from Selectize while its choices initialise.
    fmt <- input$format
    if (length(fmt) == 1L && !is.na(fmt) && nzchar(trimws(as.character(fmt))) &&
        fmt %in% c("Test", "ODI", "T20")) {
      x <- x |> filter(format_group == fmt)
    }
    
    # In limited-overs records, innings positions after 2 are generally Super Overs.
    # Keep regular innings in the venue averages; Test cricket retains innings 1–4.
    x <- x |> filter(format_group == "Test" | innings_number <= 2)
    
    venue_choice <- input$venue
    if (length(venue_choice) == 1L && !is.na(venue_choice) &&
        nzchar(trimws(as.character(venue_choice))) &&
        venue_choice != "All grounds") {
      x <- x |> filter(venue == venue_choice)
    }
    
    yr <- input$years
    if (length(yr) == 2L && all(is.finite(yr))) {
      x <- x |> filter(year >= yr[1], year <= yr[2])
    }
    x
  })
  
  filtered_matches <- reactive({
    x <- filtered_innings()
    if (!nrow(x)) return(data.frame())
    distinct(x, match_id, match_date, match_type, venue)
  })
  
  output$match_count <- renderText({ x <- filtered_matches(); if (!nrow(x)) "—" else format(nrow(x), big.mark = ",") })
  output$ground_count <- renderText({ x <- filtered_innings(); if (!nrow(x)) "—" else format(dplyr::n_distinct(x$venue), big.mark = ",") })
  output$innings_count <- renderText({ x <- filtered_innings(); if (!nrow(x)) "—" else format(nrow(x), big.mark = ",") })
  
  output$innings_chart <- renderPlotly({
    x <- filtered_innings()
    validate(need(nrow(x) > 0, "No processed data for this selection. First run scripts/01_ingest_cricsheet.R."))
    
    s <- x |>
      group_by(format_group, innings_number) |>
      summarise(
        avg_score = mean(innings_runs, na.rm = TRUE),
        innings_count = dplyr::n(),
        matches_count = dplyr::n_distinct(match_id),
        .groups = "drop"
      ) |>
      mutate(innings_label = paste0("Innings ", innings_number))
    
    innings_cols <- make_innings_palette(s$innings_label)
    p <- ggplot(s, aes(
      x = reorder(innings_label, innings_number), y = avg_score, fill = innings_label,
      text = paste0(
        "Format: ", format_group,
        "<br>", innings_label,
        "<br>Average score: ", round(avg_score, 1),
        "<br>Innings records: ", innings_count,
        "<br>Matches represented: ", matches_count
      )
    )) +
      geom_col(width = 0.66, show.legend = FALSE) +
      geom_text(aes(label = format(round(avg_score, 1), nsmall = 1)),
                hjust = -0.15, color = "#f2f5f8", size = 3.5) +
      coord_flip(clip = "off") +
      facet_wrap(~format_group, ncol = 1) +
      scale_fill_manual(values = innings_cols, drop = FALSE) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(
        x = NULL, y = "Average innings runs",
        caption = "Formats are shown separately. Limited-overs Super Overs are excluded from regular innings averages."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "#111923", color = NA),
        panel.background = element_rect(fill = "#111923", color = NA),
        strip.background = element_rect(fill = "#172330", color = "#263342"),
        strip.text = element_text(color = "#e7ad50", face = "bold"),
        text = element_text(color = "#e8edf3"),
        axis.text = element_text(color = "#bbc6d3"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_text(color = "#bbc6d3"),
        plot.caption = element_text(color = "#8494a5", size = 8),
        plot.margin = margin(8, 24, 8, 8)
      )
    
    suppressWarnings(ggplotly(p, tooltip = "text")) |>
      layout(paper_bgcolor = "#111923", plot_bgcolor = "#111923", font = list(color = "#e8edf3"))
  })
  
  output$year_chart <- renderPlotly({
    x <- filtered_innings()
    validate(need(nrow(x) > 0, "No data for this selection."))
    
    s <- x |>
      mutate(year = lubridate::year(match_date), innings_label = paste0("Innings ", innings_number)) |>
      filter(!is.na(year)) |>
      group_by(format_group, year, innings_number, innings_label) |>
      summarise(avg_score = mean(innings_runs, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
    
    innings_keys <- paste0("Innings ", sort(unique(s$innings_number)))
    year_innings_cols <- make_innings_palette(innings_keys)
    p <- ggplot(s, aes(
      x = year, y = avg_score, color = innings_label, group = innings_label,
      text = paste0(
        "Format: ", format_group,
        "<br>Year: ", year,
        "<br>", innings_label,
        "<br>Average score: ", round(avg_score, 1),
        "<br>Innings records: ", n
      )
    )) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      geom_point(size = 1.8, na.rm = TRUE) +
      facet_wrap(~format_group, ncol = 1) +
      scale_color_manual(values = year_innings_cols, name = "Innings", drop = FALSE) +
      labs(x = NULL, y = "Average innings runs") +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "#111923", color = NA),
        panel.background = element_rect(fill = "#111923", color = NA),
        strip.background = element_rect(fill = "#172330", color = "#263342"),
        strip.text = element_text(color = "#e7ad50", face = "bold"),
        text = element_text(color = "#e8edf3"),
        axis.text = element_text(color = "#bbc6d3"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(color = "#bbc6d3"),
        legend.position = "bottom"
      )
    
    suppressWarnings(ggplotly(p, tooltip = "text")) |>
      layout(paper_bgcolor = "#111923", plot_bgcolor = "#111923", font = list(color = "#e8edf3"))
  })
  
  output$venue_map <- renderLeaflet({
    coords_path <- "data/reference/venue_coordinates.csv"
    coords <- if (file.exists(coords_path)) readr::read_csv(coords_path, show_col_types = FALSE) else data.frame()
    validate(need(nrow(coords) > 0, "Add venue coordinates to data/reference/venue_coordinates.csv."))
    x <- filtered_innings()
    if (nrow(coords) && !"venue_pattern" %in% names(coords)) coords$venue_pattern <- coords$venue
    if (nrow(x)) {
      coords$innings_records <- vapply(coords$venue_pattern, function(pt) {
        if (is.na(pt) || !nzchar(pt)) return(0L)
        as.integer(sum(stringr::str_detect(stringr::str_to_lower(x$venue), stringr::str_to_lower(pt)), na.rm = TRUE))
      }, integer(1))
    } else coords$innings_records <- NA_integer_
    coords <- coords |> filter(!is.na(latitude), !is.na(longitude))
    leaflet(coords) |> addTiles(group = "OpenStreetMap") |>
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = ~pmax(5, pmin(14, sqrt(pmax(innings_records, 1, na.rm = TRUE)))),
                       color = "#e7ad50", fillColor = "#e7ad50", fillOpacity = .78, stroke = TRUE, weight = 1,
                       popup = ~paste0("<strong>", display_name, "</strong><br>Country: ", country,
                                       "<br>Matched innings records in current filter: ", ifelse(is.na(innings_records), "Data not loaded", innings_records),
                                       "<br><small>", coordinate_note, "</small>")) |>
      setView(lng = 25, lat = 18, zoom = 2)
  })
  
  map_is_playing <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_map_animation, {
    playing_now <- !isolate(map_is_playing())
    map_is_playing(playing_now)
    updateActionButton(session, "toggle_map_animation",
                       label = if (playing_now) "Pause animation" else "Play animation")
  }, ignoreInit = TRUE)
  
  # Set animation-year limits to the actual years available for the chosen format.
  observe({
    inn <- data_state$innings
    req(!is.null(inn), nrow(inn) > 0, input$map_format)
    yrs <- lubridate::year(inn$match_date[format_label(inn$match_type) == input$map_format])
    yrs <- yrs[is.finite(yrs)]
    req(length(yrs) > 1)
    updateSliderInput(session, "animated_year_range", min = min(yrs), max = max(yrs),
                      value = c(min(yrs), max(yrs)) )
    updateSliderInput(session, "animated_year", min = min(yrs), max = max(yrs), value = max(yrs))
  })
  
  observeEvent(input$animated_year_range, {
    req(length(input$animated_year_range) == 2)
    current <- input$animated_year
    if (current < input$animated_year_range[1]) {
      updateSliderInput(session, "animated_year", value = input$animated_year_range[1])
    } else if (current > input$animated_year_range[2]) {
      updateSliderInput(session, "animated_year", value = input$animated_year_range[2])
    }
  }, ignoreInit = TRUE)
  
  observe({
    if (!isTRUE(map_is_playing())) return()
    invalidateLater(900, session)
    # Isolate slider inputs so each frame advances on the timer, not on both
    # the timer and the slider's own reactive update.
    year_range <- isolate(input$animated_year_range)
    current <- isolate(input$animated_year)
    if (length(year_range) != 2 || is.null(current)) return()
    lo <- year_range[1]
    hi <- year_range[2]
    next_year <- if (current >= hi) lo else current + 1
    updateSliderInput(session, "animated_year", value = next_year)
  })
  
  output$animated_map <- renderLeaflet({
    leaflet() |>
      addTiles(group = "OpenStreetMap") |>
      addLegend(position = "bottomright", colors = c("#e7ad50", "#7b8794"),
                labels = c("Match data available", "No match in selected year"),
                title = "Venue status", opacity = 1) |>
      setView(lng = 25, lat = 18, zoom = 2)
  })
  
  observe({
    inn <- data_state$innings
    req(!is.null(inn), nrow(inn) > 0, input$map_format, input$animated_year)
    coords_path <- "data/reference/venue_coordinates.csv"
    coords <- if (file.exists(coords_path)) readr::read_csv(coords_path, show_col_types = FALSE) else data.frame()
    req(nrow(coords) > 0)
    selected_year <- as.integer(input$animated_year)
    x <- inn |>
      mutate(format_group = format_label(match_type), year = lubridate::year(match_date)) |>
      filter(format_group == input$map_format, year == selected_year)
    if (input$map_format != "Test") x <- x |> filter(innings_number <= 2)
    
    coords$average_innings_score <- NA_real_
    coords$match_count <- 0L
    coords$innings_count <- 0L
    coords$average_wickets <- NA_real_
    for (i in seq_len(nrow(coords))) {
      pattern <- coords$venue_pattern[[i]]
      hit <- x[stringr::str_detect(stringr::str_to_lower(x$venue), stringr::str_to_lower(pattern)), , drop = FALSE]
      if (nrow(hit)) {
        coords$average_innings_score[[i]] <- mean(hit$innings_runs, na.rm = TRUE)
        coords$match_count[[i]] <- dplyr::n_distinct(hit$match_id)
        coords$innings_count[[i]] <- nrow(hit)
        coords$average_wickets[[i]] <- mean(hit$wickets_recorded, na.rm = TRUE)
      }
    }
    coords$marker_radius <- ifelse(coords$match_count > 0,
                                   pmin(17, pmax(7, sqrt(pmax(coords$average_innings_score, 1, na.rm = TRUE)))), 4)
    coords$year_selected <- selected_year
    coords$format_selected <- input$map_format
    leafletProxy("animated_map", session) |>
      clearGroup("venue-year") |>
      addCircleMarkers(
        data = coords, lng = ~longitude, lat = ~latitude, radius = ~marker_radius,
        color = ~ifelse(match_count > 0, "#e7ad50", "#7b8794"),
        fillColor = ~ifelse(match_count > 0, "#e7ad50", "#7b8794"),
        fillOpacity = 0.82, stroke = TRUE, weight = 1,
        group = "venue-year",
        popup = ~paste0(
          "<strong>", display_name, "</strong><br>Country: ", country,
          "<br>Year: ", year_selected, " · Format: ", format_selected,
          ifelse(match_count > 0,
                 paste0("<br>Average innings score: ", round(average_innings_score, 1),
                        "<br>Matches: ", match_count, "<br>Innings records: ", innings_count,
                        "<br>Average wickets per innings: ", round(average_wickets, 1)),
                 "<br>No matching local match record in this year."),
          "<br><small>Approximate venue coordinates; see Ground Data for note.</small>"
        )
      )
  })
  
  compare_summary <- reactive({
    x <- data_state$innings
    if (is.null(x) || !nrow(x)) return(data.frame())
    x <- x |> mutate(format_group = format_label(match_type), year = lubridate::year(match_date)) |>
      filter(format_group == input$compare_format)
    if (input$compare_format != "Test") x <- x |> filter(innings_number <= 2)
    if (!is.null(input$compare_years) && length(input$compare_years) == 2) x <- x |> filter(year >= input$compare_years[1], year <= input$compare_years[2])
    if (!is.null(input$compare_venues) && length(input$compare_venues)) x <- x |> filter(venue %in% input$compare_venues)
    x |> group_by(venue, innings_number) |> summarise(avg_score = mean(innings_runs, na.rm = TRUE), innings_count = n(), .groups = "drop")
  })
  
  output$compare_chart <- renderPlotly({
    s <- compare_summary()
    validate(need(nrow(s) > 0, "Load processed data and choose one or more grounds."))
    compare_innings_cols <- make_innings_palette(as.character(sort(unique(s$innings_number))))
    p <- ggplot(s, aes(venue, avg_score, fill = factor(innings_number),
                       text = paste0("Ground: ", venue, "<br>Innings: ", innings_number,
                                     "<br>Average: ", round(avg_score, 1), "<br>Innings records: ", innings_count))) +
      geom_col(position = position_dodge(width = .78), width = .7) +
      coord_flip() + scale_fill_manual(values = compare_innings_cols, name = "Innings", drop = FALSE) +
      labs(x = NULL, y = "Average innings runs") + theme_minimal(base_size = 12) +
      theme(plot.background = element_rect(fill = "#111923", color = NA), panel.background = element_rect(fill = "#111923", color = NA),
            text = element_text(color = "#e8edf3"), axis.text = element_text(color = "#bbc6d3"), panel.grid.minor = element_blank())
    suppressWarnings(ggplotly(p, tooltip = "text")) |> layout(paper_bgcolor = "#111923", plot_bgcolor = "#111923", font = list(color = "#e8edf3"))
  })
  
  output$summary_table <- renderDT({
    s <- compare_summary()
    datatable(s, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) |>
      formatRound("avg_score", 1)
  })
  
  output$length_load_status <- renderUI({
    msg <- length_status()
    div(class = "sidebar-note", strong("Enriched-data status"), p(msg))
  })
  
  refresh_local_enriched_data <- function(force = FALSE) {
    length_status("Loading compact prepared cricket summaries...")
    tryCatch({
      cache_files <- c(
        length = "data/processed/enriched_length_summary.rds",
        direction = "data/processed/enriched_direction_summary.rds",
        wagon = "data/processed/enriched_wagon_sample.rds",
        coverage = "data/processed/enriched_coverage_summary.rds"
      )
      missing_files <- cache_files[!file.exists(cache_files)]
      if (length(missing_files)) {
        stop(paste0(
          "Fast dashboard summaries are not prepared yet. Stop the app and run ",
          "source(\"scripts/06_build_dashboard_cache.R\") once. This reads the existing 50 MB prepared RDS, ",
          "not the original 1.68 GB CSVs. Missing: ", paste(basename(missing_files), collapse = ", ")
        ))
      }
      withProgress(message = "Loading compact PitchAtlas summaries", value = 0, {
        setProgress(0.25, detail = "Loading delivery-length rates...")
        len <- readRDS(cache_files[["length"]])
        setProgress(0.50, detail = "Loading run-direction totals...")
        dir_sum <- readRDS(cache_files[["direction"]])
        setProgress(0.70, detail = "Loading sampled wagon-wheel coordinates...")
        wagon <- readRDS(cache_files[["wagon"]])
        setProgress(0.90, detail = "Loading venue coverage...")
        cov <- readRDS(cache_files[["coverage"]])
      })
      must_have <- list(
        length = c("venue", "format_group", "length_group", "labelled_deliveries", "wicket_deliveries", "wicket_rate_pct"),
        direction = c("venue", "format_group", "shot_direction", "batter_runs", "scoring_deliveries"),
        wagon = c("venue", "format_group", "shot_direction", "runs_for_direction", "angle_num", "distance_num"),
        coverage = c("venue", "format_group", "total_deliveries", "labelled_deliveries", "direction_rows", "wagon_rows")
      )
      objects <- list(length = len, direction = dir_sum, wagon = wagon, coverage = cov)
      for (nm in names(objects)) {
        if (!is.data.frame(objects[[nm]]) || length(setdiff(must_have[[nm]], names(objects[[nm]])))) {
          stop("The ", nm, " dashboard cache has an unexpected schema. Rebuild it with scripts/06_build_dashboard_cache.R.")
        }
      }
      length_summary_state(len)
      direction_summary_state(dir_sum)
      wagon_sample_state(wagon)
      length_coverage_state(cov)
      length_data(NULL)
      
      venues <- sort(unique(as.character(cov$venue[!is.na(cov$venue) & nzchar(as.character(cov$venue))])))
      formats <- sort(unique(as.character(cov$format_group[!is.na(cov$format_group) & nzchar(as.character(cov$format_group))])))
      if (!length(venues)) stop("No valid venue names were found in the prepared summaries.")
      cur_length_venue <- isolate(input$length_venue)
      sel_length <- if (!is.null(cur_length_venue) && cur_length_venue %in% venues) cur_length_venue else "All grounds"
      updateSelectizeInput(session, "length_venue", choices = c("All grounds", venues), selected = sel_length, server = FALSE)
      
      cur_format <- isolate(input$length_format)
      sel_format <- if (!is.null(cur_format) && cur_format %in% formats) cur_format else "All formats"
      updateSelectInput(session, "length_format", choices = c("All formats", formats), selected = sel_format)
      
      cur_run_venue <- isolate(input$run_venue)
      sel_run <- if (!is.null(cur_run_venue) && cur_run_venue %in% venues) cur_run_venue else "All stadiums"
      updateSelectizeInput(session, "run_venue", choices = c("All stadiums", venues), selected = sel_run, server = FALSE)
      
      total <- sum(cov$total_deliveries, na.rm = TRUE)
      labelled <- sum(cov$labelled_deliveries, na.rm = TRUE)
      dir_n <- sum(cov$direction_rows, na.rm = TRUE)
      wagon_n <- sum(cov$wagon_rows, na.rm = TRUE)
      status <- paste0(
        "Loaded fast summaries for ", format(length(venues), big.mark = ","), " venues and ",
        format(total, big.mark = ","), " deliveries. Length-labelled: ", format(labelled, big.mark = ","),
        ". Rows with recorded shot direction and batter runs: ", format(dir_n, big.mark = ","),
        ". Wagon-wheel chart uses a stratified display sample (max 400 shots per ground/format); run-direction totals use all eligible records."
      )
      if (!wagon_n) status <- paste(status, "No valid source-recorded shot coordinates were available.")
      length_status(status)
      showNotification(status, type = "message", duration = 10)
    }, error = function(e) {
      length_data(NULL)
      length_summary_state(data.frame())
      direction_summary_state(data.frame())
      wagon_sample_state(data.frame())
      length_coverage_state(data.frame())
      msg <- paste0("Could not load fast dashboard summaries: ", conditionMessage(e))
      length_status(msg)
      showNotification(msg, type = "error", duration = 18)
    })
  }
  
  # Automatic first load: no file picker or browser upload is required.
  observeEvent(TRUE, {
    refresh_local_enriched_data(force = FALSE)
  }, once = TRUE, ignoreInit = FALSE)
  
  observeEvent(input$reload_enriched_data, {
    refresh_local_enriched_data(force = TRUE)
  }, ignoreInit = TRUE)
  
  filtered_length <- reactive({
    d <- length_summary_state()
    if (is.null(d) || !nrow(d)) return(data.frame())
    if (!is.null(input$length_venue) && input$length_venue != "All grounds") d <- d |> dplyr::filter(venue == input$length_venue)
    if (!is.null(input$length_format) && input$length_format != "All formats") d <- d |> dplyr::filter(format_group == input$length_format)
    d
  })
  
  filtered_coverage <- reactive({
    d <- length_coverage_state()
    if (is.null(d) || !nrow(d)) return(data.frame())
    if (!is.null(input$length_venue) && input$length_venue != "All grounds") d <- d |> dplyr::filter(venue == input$length_venue)
    if (!is.null(input$length_format) && input$length_format != "All formats") d <- d |> dplyr::filter(format_group == input$length_format)
    d
  })
  
  direction_data <- reactive({
    d <- wagon_sample_state()
    if (is.null(d) || !nrow(d)) return(data.frame())
    if (!is.null(input$run_venue) && input$run_venue != "All stadiums") d <- d |> dplyr::filter(venue == input$run_venue)
    if (!is.null(input$length_format) && input$length_format != "All formats") d <- d |> dplyr::filter(format_group == input$length_format)
    d
  })
  
  filtered_direction_summary <- reactive({
    d <- direction_summary_state()
    if (is.null(d) || !nrow(d)) return(data.frame())
    if (!is.null(input$run_venue) && input$run_venue != "All stadiums") d <- d |> dplyr::filter(venue == input$run_venue)
    if (!is.null(input$length_format) && input$length_format != "All formats") d <- d |> dplyr::filter(format_group == input$length_format)
    d
  })
  
  output$pitch_zone_map <- renderPlotly({
    d <- filtered_length()
    validate(need(nrow(d) > 0, "No labelled delivery-length records match this filter yet."))
    zone_order <- c("Fuller", "Good length", "Short of good length", "Hard length", "Short")
    zone_stats <- d |>
      dplyr::filter(!is.na(length_group)) |>
      dplyr::group_by(length_group) |>
      dplyr::summarise(
        labelled_deliveries = sum(labelled_deliveries, na.rm = TRUE),
        wicket_deliveries = sum(wicket_deliveries, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(wicket_rate_pct = dplyr::if_else(labelled_deliveries > 0,
                                                     100 * wicket_deliveries / labelled_deliveries, NA_real_))
    zones <- data.frame(
      length_group = zone_order,
      ymin = c(0.80, 0.60, 0.40, 0.20, 0.00),
      ymax = c(1.00, 0.80, 0.60, 0.40, 0.20),
      fill = c("#d4b57b", "#c5b88a", "#b7ad82", "#9ca777", "#75966e"),
      stringsAsFactors = FALSE
    ) |>
      dplyr::left_join(zone_stats, by = "length_group") |>
      dplyr::mutate(
        labelled_deliveries = tidyr::replace_na(labelled_deliveries, 0L),
        wicket_deliveries = tidyr::replace_na(wicket_deliveries, 0L),
        zone_text = ifelse(labelled_deliveries > 0,
                           paste0(length_group, "\nn = ", format(labelled_deliveries, big.mark = ","),
                                  "\nWicket-delivery rate = ", round(wicket_rate_pct, 1), "%"),
                           paste0(length_group, "\nNo labelled data")),
        tooltip = ifelse(labelled_deliveries > 0,
                         paste0(length_group, "<br>Labelled deliveries: ", format(labelled_deliveries, big.mark = ","),
                                "<br>Wicket deliveries: ", format(wicket_deliveries, big.mark = ","),
                                "<br>Wicket-delivery rate: ", round(wicket_rate_pct, 2), "%"),
                         paste0(length_group, "<br>No labelled delivery data for this filter."))
      )
    p <- ggplot2::ggplot(zones, ggplot2::aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax,
                                             fill = length_group, text = tooltip)) +
      ggplot2::geom_rect(color = "#eee0bf", linewidth = 1.2) +
      ggplot2::geom_text(ggplot2::aes(x = 0.5, y = (ymin + ymax) / 2, label = zone_text),
                         inherit.aes = FALSE, color = "#17251b", fontface = "bold", size = 4, lineheight = 1.05) +
      ggplot2::scale_fill_manual(values = stats::setNames(zones$fill, zones$length_group)) +
      ggplot2::coord_cartesian(xlim = c(-0.05, 1.05), ylim = c(-0.02, 1.02), expand = FALSE) +
      ggplot2::labs(caption = "SCHEMATIC ONLY — zone bands are illustrative, not measured landing coordinates. Source labels remain separate.") +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(legend.position = "none", plot.background = ggplot2::element_rect(fill = "#111923", color = NA),
                     panel.background = ggplot2::element_rect(fill = "#111923", color = NA),
                     plot.caption = ggplot2::element_text(color = "#e7ad50", size = 9), plot.margin = ggplot2::margin(12, 26, 12, 26))
    suppressWarnings(ggplotly(p, tooltip = "text"))
  })
  
  output$wagon_wheel <- renderPlotly({
    s <- direction_data()
    validate(need(is.data.frame(s) && nrow(s) > 0, "No sampled tracked shots match this ground/format. Check the coverage panel."))
    required <- c("angle_num", "distance_num", "shot_direction", "runs_for_direction")
    validate(need(all(required %in% names(s)), "Wagon-wheel cache needs source-recorded angle and distance values. Rebuild scripts/06_build_dashboard_cache.R."))
    s <- s |>
      dplyr::mutate(angle_num = suppressWarnings(as.numeric(angle_num)),
                    distance_num = suppressWarnings(as.numeric(distance_num)),
                    runs_num = suppressWarnings(as.numeric(runs_for_direction)),
                    direction_text = trimws(as.character(shot_direction))) |>
      dplyr::filter(is.finite(angle_num), is.finite(distance_num), is.finite(runs_num),
                    !is.na(direction_text), nzchar(direction_text))
    validate(need(nrow(s) > 0, "No valid source-recorded wagon angles and distances exist for this selection."))
    if (nrow(s) > 500L) { set.seed(20261020); s <- s[sample.int(nrow(s), 500L), , drop = FALSE] }
    max_dist <- max(s$distance_num, na.rm = TRUE)
    validate(need(is.finite(max_dist) && max_dist > 0, "Wagon distances are not usable for this selection."))
    s$radius_plot <- pmin(1, pmax(0, s$distance_num / max_dist))
    theta <- (s$angle_num %% 360) * pi / 180
    s$x_plot <- s$radius_plot * sin(theta)
    s$y_plot <- s$radius_plot * cos(theta)
    s$marker_size <- pmin(8, 2 + sqrt(pmax(s$runs_num, 0)))
    s$hover_text <- paste0("Venue: ", s$venue, "<br>Format: ", s$format_group,
                           "<br>Recorded direction: ", s$direction_text, "<br>Recorded angle: ", round(s$angle_num, 1), "°",
                           "<br>Recorded distance: ", round(s$distance_num, 3), "<br>Batter runs: ", s$runs_num,
                           "<br>Radius normalized to furthest displayed shot; not surveyed boundary distance.")
    shape_list <- list()
    for (rr in c(0.25, 0.5, 0.75, 1)) {
      shape_list[[length(shape_list) + 1L]] <- list(type="circle", xref="x", yref="y", x0=-rr, y0=-rr, x1=rr, y1=rr,
                                                    line=list(color="#34483f", width=1), fillcolor="rgba(0,0,0,0)")
    }
    for (deg in seq(0, 315, by=45)) {
      r <- deg * pi/180
      shape_list[[length(shape_list) + 1L]] <- list(type="line", xref="x", yref="y", x0=0, y0=0,
                                                    x1=sin(r), y1=cos(r), line=list(color="#34483f", width=1))
    }
    p <- plotly::plot_ly(data=s, x=~x_plot, y=~y_plot, type="scatter", mode="markers",
                         text=~hover_text, hovertemplate="%{text}<extra></extra>",
                         marker=list(size=~marker_size, color="#e7ad50", opacity=0.78, line=list(color="#111923", width=0.5)),
                         showlegend=FALSE)
    selected_ground <- input$run_venue %||% "All stadiums"
    selected_format <- input$length_format %||% "All formats"
    plotly::layout(p, title=list(text=paste0("Recorded shot locations · ", selected_ground,
                                             if (selected_format != "All formats") paste0(" · ", selected_format) else ""), font=list(color="#e7ad50",size=13)),
                   paper_bgcolor="#111923", plot_bgcolor="#111923", font=list(color="#e8edf3"), shapes=shape_list,
                   xaxis=list(visible=FALSE, range=c(-1.15,1.15), scaleanchor="y", scaleratio=1, zeroline=FALSE),
                   yaxis=list(visible=FALSE, range=c(-1.15,1.15), zeroline=FALSE), margin=list(l=10,r=10,t=50,b=32),
                   annotations=list(list(x=0,y=-1.1,xref="x",yref="y",text=paste0("Showing ",format(nrow(s),big.mark=","),
                                                                                  " tracked shots; full eligible run totals are used in the pie chart."),showarrow=FALSE,font=list(size=9,color="#8494a5")))) |>
      plotly::config(displayModeBar=FALSE, responsive=TRUE)
  })
  
  output$run_direction_pie <- renderPlotly({
    d <- filtered_direction_summary()
    validate(need(nrow(d) > 0, "No runs with a recorded shot direction match this ground/format."))
    s_all <- d |>
      dplyr::filter(!is.na(shot_direction), nzchar(shot_direction), is.finite(batter_runs), batter_runs > 0) |>
      dplyr::mutate(direction_label = stringr::str_to_title(stringr::str_squish(shot_direction))) |>
      dplyr::group_by(direction_label) |>
      dplyr::summarise(batter_runs = sum(batter_runs, na.rm = TRUE),
                       scoring_deliveries = sum(scoring_deliveries, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(batter_runs))
    validate(need(nrow(s_all) > 0, "No batter runs with a recorded shot direction in this selection."))
    if (nrow(s_all) > 11) {
      top <- s_all |> dplyr::slice_head(n = 10)
      rest <- s_all |> dplyr::slice(-(seq_len(10))) |>
        dplyr::summarise(direction_label = "Other recorded directions",
                         batter_runs = sum(batter_runs, na.rm = TRUE),
                         scoring_deliveries = sum(scoring_deliveries, na.rm = TRUE))
      s <- dplyr::bind_rows(top, rest)
    } else s <- s_all
    selected_ground <- input$run_venue %||% "All stadiums"
    selected_format <- input$length_format %||% "All formats"
    chart_title <- paste0("Batter runs by recorded shot direction · ", selected_ground,
                          if (selected_format != "All formats") paste0(" · ", selected_format) else "")
    plotly::plot_ly(
      data = s, labels = ~direction_label, values = ~batter_runs, type = "pie", hole = 0.35,
      textinfo = "label+percent", sort = FALSE,
      hovertemplate = "%{label}<br>Batter runs: %{value:,}<br>Share of recorded-direction batter runs: %{percent}<extra></extra>",
      marker = list(line = list(color = "#111923", width = 2))
    ) |>
      plotly::layout(paper_bgcolor = "#111923", plot_bgcolor = "#111923",
                     title = list(text = chart_title, font = list(size = 13, color = "#e7ad50")),
                     font = list(color = "#e8edf3"), showlegend = TRUE,
                     annotations = list(list(text = "BATTER<br>RUNS", x = 0.5, y = 0.5, showarrow = FALSE,
                                             font = list(size = 12, color = "#e7ad50"))))
  })
  
  output$length_chart <- renderPlotly({
    s <- filtered_length()
    validate(need(nrow(s) > 0, "No labelled delivery-length records match this ground/format."))
    s <- s |>
      dplyr::mutate(length_group = factor(length_group, levels = c("Full toss", "Yorker", "Fuller", "Good length", "Short of good length", "Hard length", "Short"))) |>
      dplyr::arrange(length_group)
    length_cols <- c("Full toss" = "#c8a2c8", "Yorker" = "#d88aa4", "Fuller" = "#e7ad50",
                     "Good length" = "#66c4b0", "Short of good length" = "#b7ad82",
                     "Hard length" = "#7798e6", "Short" = "#e27f6f")
    p <- ggplot2::ggplot(s, ggplot2::aes(
      x = length_group, y = wicket_rate_pct, fill = length_group,
      text = paste0("Ground: ", venue, "<br>Format: ", format_group,
                    "<br>Length: ", length_group,
                    "<br>Wicket-delivery rate: ", round(wicket_rate_pct, 2), "%",
                    "<br>Labelled deliveries: ", format(labelled_deliveries, big.mark = ","),
                    "<br>Wicket deliveries: ", format(wicket_deliveries, big.mark = ","))
    )) +
      ggplot2::geom_col(show.legend = FALSE, width = 0.66) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(round(wicket_rate_pct, 1), "%")),
                         hjust = -0.15, color = "#f2f5f8", size = 3.5) +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_fill_manual(values = length_cols, drop = FALSE) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
      ggplot2::labs(x = NULL, y = "Deliveries with a recorded wicket (%)",
                    caption = "Rates are computed over source-labelled deliveries. Full toss, yorker and short-of-good-length remain separate.") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#111923", color = NA),
                     panel.background = ggplot2::element_rect(fill = "#111923", color = NA),
                     text = ggplot2::element_text(color = "#e8edf3"), axis.text = ggplot2::element_text(color = "#bbc6d3"),
                     panel.grid.minor = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
                     plot.caption = ggplot2::element_text(color = "#8494a5", size = 8),
                     plot.margin = ggplot2::margin(8, 22, 8, 8))
    suppressWarnings(ggplotly(p, tooltip = "text"))
  })
  
  output$length_coverage <- renderUI({
    cov <- filtered_coverage()
    if (!nrow(cov)) return(tagList(p("Prepared summary data is not loaded yet."), p("Run scripts/06_build_dashboard_cache.R once, then restart the app.")))
    total <- sum(cov$total_deliveries, na.rm = TRUE)
    labelled <- sum(cov$labelled_deliveries, na.rm = TRUE)
    pct <- if (total > 0) 100 * labelled / total else 0
    direction_n <- sum(cov$direction_rows, na.rm = TRUE)
    wagon_n <- sum(cov$wagon_rows, na.rm = TRUE)
    tagList(
      div(class = "coverage-big", paste0(format(round(pct, 1), nsmall = 1), "%")),
      p("of deliveries in this filter have a supported recorded bowling-length label."),
      tags$ul(
        tags$li(paste("Deliveries in filter:", format(total, big.mark = ","))),
        tags$li(paste("Length-labelled deliveries:", format(labelled, big.mark = ","))),
        tags$li(paste("Rows with shot direction and batter runs:", format(direction_n, big.mark = ","))),
        tags$li(paste("Tracked shots eligible for the sampled wagon wheel:", format(wagon_n, big.mark = ","))),
        tags$li("Wicket-delivery rate includes recorded wicket deliveries; not every dismissal is credited to the bowler.")
      ),
      p("The wagon-wheel panel samples tracked shots for display speed; the run-direction chart uses aggregated totals from all eligible records.")
    )
  })
  
  output$length_table <- renderDT({
    s <- filtered_length()
    if (!nrow(s)) return(datatable(data.frame(Message = "No labelled delivery-length records for this selection."), options = list(dom = "t"), rownames = FALSE))
    datatable(s, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) |>
      formatRound("wicket_rate_pct", 2)
  })
  
  prediction_message <- reactiveVal("Prediction caches are loading. Run scripts/06_build_dashboard_cache.R after preparing the enriched data if caches are missing.")
  
  current_ground_baseline <- function(venue, format_group, innings_no) {
    b <- prediction_state$venue_baselines
    if (!is.data.frame(b) || !nrow(b)) return(NA_real_)
    z <- b[b$format_group == format_group & b$innings_no == as.numeric(innings_no), , drop=FALSE]
    if (!identical(venue, "All grounds")) z <- z[z$venue == venue, , drop=FALSE]
    if (!nrow(z)) return(NA_real_)
    mean(z$venue_avg_score, na.rm=TRUE)
  }
  
  forecast_result <- eventReactive(input$calculate_forecast, {
    bundle <- prediction_state$score_model
    if (is.null(bundle) || is.null(bundle$model)) return(NULL)
    fmt <- as.character(input$pred_format %||% "T20")
    inn <- as.numeric(input$pred_innings %||% 1)
    overs <- max(0, as.numeric(input$pred_overs %||% 0))
    runs <- max(0, as.numeric(input$pred_runs %||% 0))
    wkts <- min(10, max(0, as.numeric(input$pred_wickets %||% 0)))
    recent <- max(0, as.numeric(input$pred_recent_runs %||% 0))
    venue_avg <- current_ground_baseline(input$pred_venue %||% "All grounds", fmt, inn)
    if (!is.finite(venue_avg)) {
      z <- prediction_state$score_snapshots
      if (is.data.frame(z) && nrow(z)) venue_avg <- mean(z$venue_avg_score[z$format_group == fmt & z$innings_no == inn], na.rm=TRUE)
    }
    if (!is.finite(venue_avg)) venue_avg <- 0
    form_levels <- bundle$model$xlevels$format_group %||% c("T20", "ODI", "Test")
    nd <- data.frame(current_runs=runs, wickets_lost=wkts, overs_elapsed=overs,
                     current_run_rate=if (overs > 0) runs/overs else 0, recent_runs_30=recent,
                     format_group=factor(fmt, levels=form_levels), innings_no=inn, venue_avg_score=venue_avg)
    predicted <- tryCatch(as.numeric(stats::predict(bundle$model, newdata=nd)), error=function(e) NA_real_)
    if (!is.finite(predicted)) stop("Model could not produce a score for this combination. Choose a format/innings represented in the training data.")
    max_overs <- switch(fmt, T20=20, ODI=50, Test=200, 50)
    predicted <- max(runs, predicted)
    width <- as.numeric(bundle$uncertainty80 %||% 35)
    list(format=fmt, venue=input$pred_venue %||% "All grounds", innings=inn, current_runs=runs,
         predicted=round(predicted), lower=round(max(runs,predicted-width)), upper=round(predicted+width),
         overs=overs, max_overs=max_overs, wickets=wkts, venue_avg=round(venue_avg,1), recent=recent,
         trained_through=bundle$training_through_year %||% NA_integer_, test_rows=bundle$test_rows %||% NA_integer_)
  }, ignoreInit=TRUE)
  
  chase_result <- eventReactive(input$calculate_forecast, {
    fmt <- as.character(input$pred_format %||% "T20")
    if (!fmt %in% c("T20", "ODI")) return(list(prob=NA_real_,message="Chase probability is only offered for limited-overs matches."))
    bundle <- prediction_state$chase_model
    if (is.null(bundle) || is.null(bundle$model)) return(list(prob=NA_real_,message="Chase model cache is unavailable. Build caches with scripts/06_build_dashboard_cache.R."))
    target <- as.numeric(input$pred_target %||% NA_real_)
    balls <- as.numeric(input$pred_balls_remaining %||% NA_real_)
    runs <- as.numeric(input$pred_runs %||% NA_real_)
    wkts <- as.numeric(input$pred_wickets %||% NA_real_)
    overs <- max(0, as.numeric(input$pred_overs %||% 0))
    if (!all(is.finite(c(target,balls,runs,wkts))) || target < 1 || balls < 0) return(list(prob=NA_real_,message="Enter a valid target, current score and balls remaining."))
    req_runs <- max(0,target-runs)
    rr <- if (overs > 0) runs/overs else 0
    rrr <- req_runs/max(balls/6,1/6)
    form_levels <- bundle$model$xlevels$format_group %||% c("T20","ODI")
    nd <- data.frame(current_runs=runs,wickets_lost=min(10,max(0,wkts)),current_run_rate=rr,
                     balls_remaining=balls,runs_required=req_runs,required_run_rate=rrr,format_group=factor(fmt,levels=form_levels))
    prob <- tryCatch(as.numeric(stats::predict(bundle$model,newdata=nd,type="response")),error=function(e) NA_real_)
    if (!is.finite(prob)) return(list(prob=NA_real_,message="This model cannot estimate the chosen format/state. Check the model evaluation table."))
    list(prob=min(1,max(0,prob)),message=paste0("Estimated from historical second-innings snapshots; current target ",target,", runs required ",req_runs," from ",balls," balls."))
  }, ignoreInit=TRUE)
  
  output$prediction_summary <- renderUI({
    f <- forecast_result()
    c <- chase_result()
    if (is.null(f)) return(div(class="ground-empty-state",h3("Enter an innings state and click Calculate forecast"),p("Forecasts are calculated only from the locally trained model cache.")))
    tagList(
      div(class="metric-grid",
          div(class="metric-card",span("PREDICTED FINAL SCORE",class="metric-label"),h3(format(f$predicted,big.mark=",")),p(paste0("Approx. 80% historical error band: ",f$lower,"–",f$upper))),
          div(class="metric-card metric-teal",span("GROUND BASELINE",class="metric-label"),h3(if (is.finite(f$venue_avg)) as.character(f$venue_avg) else "—"),p(paste(f$venue,"·",f$format))),
          div(class="metric-card metric-blue",span("OVERS / WICKETS",class="metric-label"),h3(paste(f$overs,"overs ·",f$wickets,"wkts")),p(paste("Innings",f$innings))),
          div(class="metric-card metric-purple",span("CHASE PROBABILITY",class="metric-label"),h3(if (is.finite(c$prob)) paste0(round(100*c$prob),"%") else "Not available"),p(c$message))
      ),
      div(class="sidebar-note",strong("Interpretation"),p("This is a historical model estimate, not a certainty. Accuracy depends on training coverage, recent seasons, venue sample size and the difference between international and domestic competitions."))
    )
  })
  
  output$prediction_score_chart <- renderPlotly({
    snaps <- prediction_state$score_snapshots
    validate(need(is.data.frame(snaps) && nrow(snaps)>0,"Score-prediction snapshots are missing. Stop Shiny and run scripts/06_build_dashboard_cache.R."))
    fmt <- input$pred_format %||% "T20"
    x <- snaps[snaps$format_group == fmt, , drop=FALSE]
    if (!identical(input$pred_venue %||% "All grounds","All grounds")) x <- x[x$venue == input$pred_venue, , drop=FALSE]
    if (nrow(x)>1500) { set.seed(20261020); x <- x[sample.int(nrow(x),1500),,drop=FALSE] }
    validate(need(nrow(x)>0,"No training snapshots for this ground/format."))
    p <- ggplot2::ggplot(x,ggplot2::aes(x=overs_elapsed,y=final_runs,text=paste0("Ground: ",venue,"<br>Current state: ",current_runs," runs, ",wickets_lost," wickets<br>Overs: ",overs_elapsed,"<br>Actual final score: ",final_runs))) +
      ggplot2::geom_point(alpha=.24,color="#66c4b0",size=1.2) + ggplot2::stat_summary(fun=mean,geom="line",color="#e7ad50",linewidth=1.1) +
      ggplot2::labs(x="Overs completed at snapshot",y="Actual final score",caption="Dots are historical snapshot/finish pairs; orange line is the average final score at each stage.") + ggplot2::theme_minimal(base_size=11) +
      ggplot2::theme(plot.background=ggplot2::element_rect(fill="#111923",color=NA),panel.background=ggplot2::element_rect(fill="#111923",color=NA),text=ggplot2::element_text(color="#e8edf3"),axis.text=ggplot2::element_text(color="#bbc6d3"),panel.grid.minor=ggplot2::element_blank())
    f <- tryCatch(forecast_result(),error=function(e) NULL)
    if (!is.null(f)) p <- p + ggplot2::geom_hline(yintercept=f$predicted,color="#e7ad50",linetype="dashed",linewidth=.8)
    suppressWarnings(ggplotly(p,tooltip="text")) |> plotly::layout(paper_bgcolor="#111923",plot_bgcolor="#111923",font=list(color="#e8edf3"))
  })
  
  output$prediction_chase_chart <- renderPlotly({
    c <- tryCatch(chase_result(),error=function(e) NULL)
    validate(need(!is.null(c) && is.finite(c$prob), if (!is.null(c)) c$message else "Click Calculate forecast to estimate a chase."))
    m <- prediction_state$match_results
    base <- NA_real_
    if (is.data.frame(m) && nrow(m)) {
      fmt <- input$pred_format %||% "T20"; mm <- m[m$format_group==fmt,,drop=FALSE]
      if (nrow(mm)) base <- mean(mm$chasing_win,na.rm=TRUE)
    }
    if (!is.finite(base)) base <- c$prob
    z <- data.frame(outcome=c("Model chase probability","Historical chase win rate"),value=c(100*c$prob,100*base))
    plotly::plot_ly(z,x=~value,y=~outcome,type="bar",orientation="h",text=~paste0(round(value,1),"%"),textposition="outside",
                    marker=list(color=c("#66c4b0","#e7ad50")),hovertemplate="%{y}: %{x:.1f}%<extra></extra>") |>
      plotly::layout(paper_bgcolor="#111923",plot_bgcolor="#111923",font=list(color="#e8edf3"),xaxis=list(title="Win probability / historical rate (%)",range=c(0,105),gridcolor="#263342"),yaxis=list(title="",automargin=TRUE),margin=list(l=160,r=20,t=20,b=45))
  })
  
  output$prediction_evaluation <- renderUI({
    sm <- prediction_state$score_metrics; cm <- prediction_state$chase_metrics
    if ((!is.data.frame(sm)||!nrow(sm)) && (!is.data.frame(cm)||!nrow(cm))) return(div(class="ground-empty-state",h3("Model evaluation not available"),p("After the data cache is built, the app records score-model MAE/RMSE and chase-model Brier score on a later-date holdout.")))
    tagList(
      if (is.data.frame(sm)&&nrow(sm)) div(h4("Final-score model"),DT::dataTableOutput("score_eval_table")),
      if (is.data.frame(cm)&&nrow(cm)) div(h4("Chase-probability model"),DT::dataTableOutput("chase_eval_table"))
    )
  })
  output$score_eval_table <- DT::renderDataTable({ DT::datatable(prediction_state$score_metrics,options=list(dom="t",paging=FALSE),rownames=FALSE) })
  output$chase_eval_table <- DT::renderDataTable({ DT::datatable(prediction_state$chase_metrics,options=list(dom="t",paging=FALSE),rownames=FALSE) })
  
  strategy_filtered <- reactive({
    z <- prediction_state$bowling_style
    if (!is.data.frame(z) || !nrow(z)) return(data.frame())
    if (!is.null(input$strategy_venue) && input$strategy_venue!="All grounds") z <- z[z$venue==input$strategy_venue,,drop=FALSE]
    if (!is.null(input$strategy_format)) z <- z[z$format_group==input$strategy_format,,drop=FALSE]
    max_year <- suppressWarnings(max(z$year,na.rm=TRUE)); if (!is.finite(max_year)) max_year <- as.integer(format(Sys.Date(),"%Y"))
    win <- as.numeric(input$strategy_window %||% 5)
    if (win < 999) z <- z[!is.na(z$year) & z$year >= max_year-win+1,,drop=FALSE]
    if (!nrow(z)) return(z)
    dplyr::as_tibble(z) |> dplyr::group_by(bowler_type) |> dplyr::summarise(
      legal_balls=sum(legal_balls,na.rm=TRUE),deliveries=sum(deliveries,na.rm=TRUE),runs_conceded=sum(runs_conceded,na.rm=TRUE),
      wickets_recorded=sum(wickets_recorded,na.rm=TRUE),matches=sum(matches,na.rm=TRUE),
      economy_per_over=ifelse(sum(legal_balls,na.rm=TRUE)>0,6*sum(runs_conceded,na.rm=TRUE)/sum(legal_balls,na.rm=TRUE),NA_real_),
      wickets_per_100_balls=ifelse(sum(legal_balls,na.rm=TRUE)>0,100*sum(wickets_recorded,na.rm=TRUE)/sum(legal_balls,na.rm=TRUE),NA_real_),.groups="drop") |> as.data.frame()
  })
  
  output$strategy_recommendation <- renderUI({
    z <- strategy_filtered()
    if (!nrow(z)) return(div(class="ground-empty-state",h3("No bowling-style evidence for this filter"),p("Choose another format/ground or rebuild the cache. Unknown player styles are not guessed.")))
    pace <- z[z$bowler_type=="Pace",,drop=FALSE]; spin <- z[z$bowler_type=="Spin",,drop=FALSE]
    min_balls <- as.numeric(input$strategy_min_balls %||% 300)
    if (!nrow(pace)||!nrow(spin)||pace$legal_balls[1]<min_balls||spin$legal_balls[1]<min_balls) {
      return(div(class="ground-empty-state",h3("Insufficient local sample for a specific mix"),
                 p(paste0("Minimum required: ",min_balls," legal balls per bowling type. Current evidence is shown below; the app will not guess a spinner/pacer count."))))
    }
    zscore <- function(x) { if (length(x)<2||!is.finite(stats::sd(x))||stats::sd(x)==0) rep(0,length(x)) else as.numeric(scale(x)) }
    economy_z <- zscore(-z$economy_per_over); wicket_z <- zscore(z$wickets_per_100_balls)
    z$performance_index <- economy_z + wicket_z
    pace_score <- z$performance_index[z$bowler_type=="Pace"][1]; spin_score <- z$performance_index[z$bowler_type=="Spin"][1]
    mix <- if (spin_score > pace_score + .45) "3 spinners + 2 pacers" else if (pace_score > spin_score + .45) "1 spinner + 4 pacers" else "2 spinners + 3 pacers"
    stronger <- if (spin_score > pace_score+.45) "Spin" else if (pace_score > spin_score+.45) "Pace" else "Mixed / no strong edge"
    tagList(div(class="metric-grid",
                div(class="metric-card",span("SUGGESTED FIVE-BOWLER MIX",class="metric-label"),h3(mix),p("Heuristic from recent recorded economy and wicket rates; not a guaranteed team selection.")),
                div(class="metric-card metric-teal",span("STRONGER HISTORICAL SIGNAL",class="metric-label"),h3(stronger),p(paste("Based on",input$strategy_venue,"·",input$strategy_format,"· selected recent window"))),
                div(class="metric-card metric-blue",span("LEGAL BALLS IN SAMPLE",class="metric-label"),h3(format(sum(z$legal_balls,na.rm=TRUE),big.mark=",")),p("Only rows with a classified bowler style are included."))))
  })
  
  output$strategy_economy_chart <- renderPlotly({
    z <- strategy_filtered(); validate(need(nrow(z)>0,"Bowling style summaries are missing. Run scripts/06_build_dashboard_cache.R."))
    plotly::plot_ly(z,x=~economy_per_over,y=~bowler_type,type="bar",orientation="h",text=~paste0("Economy: ",round(economy_per_over,2),"<br>Legal balls: ",legal_balls,"<br>Recorded wickets: ",wickets_recorded),
                    hovertemplate="%{y}<br>%{text}<extra></extra>",marker=list(color=c("#66c4b0","#e7ad50"))) |>
      plotly::layout(paper_bgcolor="#111923",plot_bgcolor="#111923",font=list(color="#e8edf3"),xaxis=list(title="Runs conceded per 6 legal balls",gridcolor="#263342"),yaxis=list(title=""))
  })
  output$strategy_wicket_chart <- renderPlotly({
    z <- strategy_filtered(); validate(need(nrow(z)>0,"Bowling style summaries are missing. Run scripts/06_build_dashboard_cache.R."))
    plotly::plot_ly(z,x=~wickets_per_100_balls,y=~bowler_type,type="bar",orientation="h",text=~paste0("Bowler-credited wickets per 100 legal balls: ",round(wickets_per_100_balls,2),"<br>Legal balls: ",legal_balls),
                    hovertemplate="%{y}<br>%{text}<extra></extra>",marker=list(color=c("#7798e6","#d88aa4"))) |>
      plotly::layout(paper_bgcolor="#111923",plot_bgcolor="#111923",font=list(color="#e8edf3"),xaxis=list(title="Recorded wickets per 100 legal balls",gridcolor="#263342"),yaxis=list(title=""))
  })
  strategy_pace_lengths <- reactive({
    z <- prediction_state$pace_length
    if (!is.data.frame(z)||!nrow(z)) return(data.frame())
    if (!is.null(input$strategy_venue)&&input$strategy_venue!="All grounds") z <- z[z$venue==input$strategy_venue,,drop=FALSE]
    if (!is.null(input$strategy_format)) z <- z[z$format_group==input$strategy_format,,drop=FALSE]
    max_year <- suppressWarnings(max(z$year,na.rm=TRUE)); if(!is.finite(max_year)) max_year<-as.integer(format(Sys.Date(),"%Y"))
    win<-as.numeric(input$strategy_window %||% 5); if(win<999) z<-z[!is.na(z$year)&z$year>=max_year-win+1,,drop=FALSE]
    if (!nrow(z)) return(z)
    dplyr::as_tibble(z) |> dplyr::group_by(length_group) |> dplyr::summarise(labelled_deliveries=sum(labelled_deliveries),wicket_deliveries=sum(wicket_deliveries),
                                                                             total_runs=sum(total_runs),matches=sum(matches),runs_per_6_balls=6*sum(total_runs)/max(1,sum(legal_balls)),
                                                                             wicket_rate_pct=100*sum(wicket_deliveries)/sum(labelled_deliveries),.groups="drop") |> as.data.frame()
  })
  output$strategy_length_chart <- renderPlotly({
    z <- strategy_pace_lengths(); validate(need(nrow(z)>0,"No labelled fast-bowling deliveries for this ground and period."))
    z <- z[z$labelled_deliveries>=max(30,as.numeric(input$strategy_min_balls %||% 300)/5),,drop=FALSE]
    validate(need(nrow(z)>0,"Not enough length-labelled fast-bowling deliveries meet the sample threshold."))
    z <- z[order(z$wicket_rate_pct,decreasing=TRUE),,drop=FALSE]
    p <- ggplot2::ggplot(z,ggplot2::aes(x=reorder(length_group,wicket_rate_pct),y=wicket_rate_pct,fill=length_group,
                                        text=paste0("Length: ",length_group,"<br>Wicket-delivery rate: ",round(wicket_rate_pct,2),"%<br>Labelled deliveries: ",labelled_deliveries,"<br>Runs per 6 balls: ",round(runs_per_6_balls,2)))) +
      ggplot2::geom_col(show.legend=FALSE) + ggplot2::coord_flip() + ggplot2::labs(x=NULL,y="Wicket-delivery rate (%)",caption="Only source-labelled pace deliveries. All dismissal types may be included in delivery-level wicket rates.") +
      ggplot2::theme_minimal(base_size=11) + ggplot2::theme(plot.background=ggplot2::element_rect(fill="#111923",color=NA),panel.background=ggplot2::element_rect(fill="#111923",color=NA),text=ggplot2::element_text(color="#e8edf3"),axis.text=ggplot2::element_text(color="#bbc6d3"),panel.grid.minor=ggplot2::element_blank())
    suppressWarnings(ggplotly(p,tooltip="text")) |> plotly::layout(paper_bgcolor="#111923",plot_bgcolor="#111923",font=list(color="#e8edf3"))
  })
  output$strategy_evidence_table <- DT::renderDataTable({
    z <- strategy_filtered(); if(!nrow(z)) return(DT::datatable(data.frame(Message="No eligible bowling-style sample."),options=list(dom="t"),rownames=FALSE))
    DT::datatable(z,options=list(pageLength=5,scrollX=TRUE),rownames=FALSE) |> DT::formatRound(c("economy_per_over","wickets_per_100_balls"),2)
  })
  
  trend_filtered <- reactive({
    z <- prediction_state$match_results
    if (!is.data.frame(z)||!nrow(z)) return(data.frame())
    if (!is.null(input$trend_format)) z <- z[z$format_group==input$trend_format,,drop=FALSE]
    if (!is.null(input$trend_venue)&&input$trend_venue!="All grounds") z <- z[z$venue==input$trend_venue,,drop=FALSE]
    max_year<-suppressWarnings(max(z$year,na.rm=TRUE)); if(!is.finite(max_year)) return(data.frame())
    win<-as.numeric(input$trend_window %||% 3); z<-z[!is.na(z$year)&z$year>=max_year-win+1,,drop=FALSE]
    z
  })
  output$recent_trend_summary <- renderUI({
    z<-trend_filtered(); if(!nrow(z)) return(div(class="ground-empty-state",h3("Recent match outcome cache is unavailable"),p("Run the updated scripts/05_prepare_enriched_data.R and scripts/06_build_dashboard_cache.R. The source must contain match winners and innings batting teams.")))
    first<-mean(z$batting_first_win,na.rm=TRUE); chase<-mean(z$chasing_win,na.rm=TRUE)
    tagList(div(class="metric-grid",
                div(class="metric-card",span("COMPLETED MATCHES IN WINDOW",class="metric-label"),h3(format(nrow(z),big.mark=",")),p(paste(input$trend_format,input$trend_venue,"· recent window"))),
                div(class="metric-card metric-teal",span("BATTING-FIRST WINS",class="metric-label"),h3(paste0(round(100*first,1),"%")),p(paste(sum(z$batting_first_win==1),"wins"))),
                div(class="metric-card metric-blue",span("CHASING WINS",class="metric-label"),h3(paste0(round(100*chase,1),"%")),p(paste(sum(z$chasing_win==1),"wins")))))
  })
  output$recent_trend_chart <- renderPlotly({
    z<-trend_filtered(); validate(need(nrow(z)>0,"No completed matches for this recent filter."))
    s<-data.frame(innings_choice=c("Batting first","Chasing"),win_pct=c(100*mean(z$batting_first_win),100*mean(z$chasing_win)))
    plotly::plot_ly(s,x=~innings_choice,y=~win_pct,type="bar",text=~paste0(round(win_pct,1),"%"),textposition="outside",marker=list(color=c("#e7ad50","#66c4b0")),hovertemplate="%{x}: %{y:.1f}%<extra></extra>") |>
      plotly::layout(paper_bgcolor="#111923",plot_bgcolor="#111923",font=list(color="#e8edf3"),yaxis=list(title="Win percentage (%)",range=c(0,100),gridcolor="#263342"),xaxis=list(title=""))
  })
  output$recent_trend_year_chart <- renderPlotly({
    z<-trend_filtered(); validate(need(nrow(z)>0,"No completed matches for this recent filter."))
    s<-dplyr::as_tibble(z) |> dplyr::group_by(year) |> dplyr::summarise(matches=dplyr::n(),batting_first_pct=100*mean(batting_first_win),chasing_pct=100*mean(chasing_win),.groups="drop") |> tidyr::pivot_longer(cols=c(batting_first_pct,chasing_pct),names_to="innings_choice",values_to="win_pct")
    s$innings_choice<-ifelse(s$innings_choice=="batting_first_pct","Batting first","Chasing")
    p<-ggplot2::ggplot(s,ggplot2::aes(x=year,y=win_pct,color=innings_choice,group=innings_choice,text=paste0("Year: ",year,"<br>",innings_choice," wins: ",round(win_pct,1),"%<br>Matches: ",matches))) + ggplot2::geom_line(linewidth=1) + ggplot2::geom_point(size=2) +
      ggplot2::scale_color_manual(values=c("Batting first"="#e7ad50","Chasing"="#66c4b0")) + ggplot2::labs(x=NULL,y="Win percentage (%)",color=NULL) + ggplot2::theme_minimal(base_size=11) + ggplot2::theme(plot.background=ggplot2::element_rect(fill="#111923",color=NA),panel.background=ggplot2::element_rect(fill="#111923",color=NA),text=ggplot2::element_text(color="#e8edf3"),axis.text=ggplot2::element_text(color="#bbc6d3"),panel.grid.minor=ggplot2::element_blank(),legend.position="bottom")
    suppressWarnings(ggplotly(p,tooltip="text")) |> plotly::layout(paper_bgcolor="#111923",plot_bgcolor="#111923",font=list(color="#e8edf3"))
  })
  output$recent_trend_table <- DT::renderDataTable({
    z<-trend_filtered(); if(!nrow(z)) return(DT::datatable(data.frame(Message="No eligible matches."),options=list(dom="t"),rownames=FALSE))
    DT::datatable(as.data.frame(z)[,intersect(c("year","venue","format_group","first_team","first_score","second_team","second_score","match_winner","batting_first_win","chasing_win"),names(z)),drop=FALSE],options=list(pageLength=10,scrollX=TRUE),rownames=FALSE)
  })
  
  
  
  
  
  
  
  output$ground_data_kpis <- renderUI({
    x <- sourced_pitch_profiles
    if (!nrow(x)) {
      return(div(class = "ground-empty-state",
                 h3("No linked pitch reports found"),
                 p("Check data/reference/pitch_surface_reference.csv. This page intentionally does not fill missing ground facts with guesses.")))
    }
    status <- stringr::str_to_lower(x$verification_status)
    historical_n <- sum(stringr::str_detect(status, "historical|curator"), na.rm = TRUE)
    match_n <- sum(stringr::str_detect(status, "match-specific"), na.rm = TRUE)
    surface_n <- dplyr::n_distinct(vapply(x$soil_type_reported, classify_reported_surface, character(1)))
    div(class = "ground-data-metrics",
        div(class = "ground-data-metric metric-gold",
            span("SOURCE-BACKED GROUNDS"), h3(format(nrow(x), big.mark = ",")),
            tags$small("Only linked reports are included")),
        div(class = "ground-data-metric metric-teal",
            span("REPORTED SURFACE GROUPS"), h3(as.character(surface_n)),
            tags$small("Grouped from the report wording")),
        div(class = "ground-data-metric metric-blue",
            span("HISTORICAL / CURATOR NOTES"), h3(as.character(historical_n)),
            tags$small("Not necessarily current strips")),
        div(class = "ground-data-metric metric-purple",
            span("MATCH-SPECIFIC NOTES"), h3(as.character(match_n)),
            tags$small("Describe a reported match/pitch")))
  })
  
  output$ground_surface_chart <- renderPlotly({
    x <- sourced_pitch_profiles
    validate(need(nrow(x) > 0, "No linked pitch reports are available in the reference file."))
    s <- x |>
      dplyr::mutate(surface_group = vapply(soil_type_reported, classify_reported_surface, character(1))) |>
      dplyr::group_by(surface_group) |>
      dplyr::summarise(ground_count = dplyr::n(), grounds = paste(venue, collapse = ", "), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(ground_count))
    plotly::plot_ly(data = s, labels = ~surface_group, values = ~ground_count, type = "pie", hole = 0.54,
                    textinfo = "label+percent", sort = FALSE,
                    customdata = ~grounds,
                    hovertemplate = "%{label}<br>Grounds: %{value}<br>%{customdata}<extra></extra>",
                    marker = list(line = list(color = "#111923", width = 2))) |>
      plotly::layout(paper_bgcolor = "#111923", plot_bgcolor = "#111923",
                     font = list(color = "#e8edf3"), showlegend = TRUE,
                     annotations = list(list(text = paste0(nrow(x), "<br>GROUNDS"), x = 0.5, y = 0.5,
                                             showarrow = FALSE, font = list(size = 14, color = "#e7ad50"))))
  })
  
  output$ground_bounce_chart <- renderPlotly({
    x <- sourced_pitch_profiles
    validate(need(nrow(x) > 0, "No linked pitch reports are available in the reference file."))
    s <- x |>
      dplyr::mutate(behaviour_theme = vapply(historical_bounce_or_behaviour, classify_reported_behaviour, character(1))) |>
      dplyr::group_by(behaviour_theme) |>
      dplyr::summarise(report_count = dplyr::n(), grounds = paste(venue, collapse = ", "), .groups = "drop") |>
      dplyr::arrange(report_count)
    plotly::plot_ly(data = s, x = ~report_count, y = ~reorder(behaviour_theme, report_count), type = "bar", orientation = "h",
                    text = ~report_count, textposition = "outside", customdata = ~grounds,
                    marker = list(color = c("#66c4b0", "#e7ad50", "#7798e6", "#d88aa4", "#9bb9a8")[seq_len(nrow(s))]),
                    hovertemplate = "%{y}<br>Linked reports: %{x}<br>Grounds: %{customdata}<extra></extra>") |>
      plotly::layout(paper_bgcolor = "#111923", plot_bgcolor = "#111923",
                     font = list(color = "#e8edf3"), xaxis = list(title = "Number of linked reports", rangemode = "tozero", gridcolor = "#263342"),
                     yaxis = list(title = "", automargin = TRUE),
                     margin = list(l = 180, r = 30, t = 15, b = 45),
                     annotations = list(list(text = "Themes are classified from report wording, not measured bounce tests.",
                                             x = 0, y = -0.18, xref = "paper", yref = "paper", showarrow = FALSE,
                                             xanchor = "left", font = list(size = 10, color = "#8da1b5"))))
  })
  
  output$pitch_profile_detail <- renderUI({
    x <- sourced_pitch_profiles
    if (!nrow(x)) return(div(class = "ground-empty-state", h3("No sourced profiles")))
    selected_name <- input$profile_venue %||% x$venue[[1]]
    r <- x |> dplyr::filter(venue == selected_name) |> dplyr::slice(1)
    if (!nrow(r)) return(div(class = "ground-empty-state", h3("Choose a sourced ground")))
    soil_group <- classify_reported_surface(r$soil_type_reported[[1]])
    status <- r$verification_status[[1]] %||% "Reported — verification status not supplied"
    scope <- r$evidence_scope[[1]] %||% "Evidence scope not supplied"
    behaviour <- r$historical_bounce_or_behaviour[[1]] %||% "No behaviour note supplied."
    report_date <- r$source_date[[1]]
    if (is.na(report_date) || !nzchar(report_date)) report_date <- "Date not supplied"
    div(class = "atlas-card ground-profile-visual",
        div(class = "ground-profile-top",
            div(class = "ground-profile-kicker", "LINKED PITCH REPORT"),
            h3(r$venue[[1]]),
            span(class = "ground-profile-badge", soil_group)),
        div(class = "ground-profile-fact surface-fact",
            span("REPORTED SURFACE / SOIL"),
            strong(soil_group),
            p(as.character(r$soil_type_reported[[1]]))),
        div(class = "ground-profile-fact behaviour-fact",
            span("BOUNCE / PITCH BEHAVIOUR NOTE"),
            p(as.character(behaviour))),
        div(class = "ground-profile-meta",
            div(span("EVIDENCE TYPE"), strong(as.character(scope))),
            div(span("REPORT STATUS"), strong(as.character(status))),
            div(span("SOURCE DATE"), strong(as.character(report_date)))),
        tags$a(class = "ground-source-link", href = r$source_url[[1]], target = "_blank", rel = "noreferrer", "Open linked report ↗"),
        p(class = "small-note", "These are source-reported historical or match-specific notes, not a live pitch inspection or a standardized bounce measurement."))
  })
  
  output$venue_behaviour_chart <- renderPlotly({
    inn <- data_state$innings
    validate(need(!is.null(inn) && nrow(inn) > 0, "Local Cricsheet innings data has not been loaded."))
    req(input$profile_venue)
    coord_path <- "data/reference/venue_coordinates.csv"
    coords <- if (file.exists(coord_path)) readr::read_csv(coord_path, show_col_types = FALSE) else data.frame()
    selected <- coords |> dplyr::filter(display_name == input$profile_venue) |> dplyr::slice(1)
    validate(need(nrow(selected) > 0, "No venue-name mapping is available for this sourced ground."))
    pattern <- selected$venue_pattern[[1]]
    x <- inn |>
      dplyr::filter(!is.na(venue), stringr::str_detect(stringr::str_to_lower(venue), stringr::str_to_lower(pattern)))
    validate(need(nrow(x) > 0, paste("No local Cricsheet match records match", input$profile_venue, "in the available archive.")))
    s <- x |>
      dplyr::mutate(format_group = format_label(match_type)) |>
      dplyr::filter(format_group == "Test" | innings_number <= 2) |>
      dplyr::group_by(format_group, innings_number) |>
      dplyr::summarise(matches_count = dplyr::n_distinct(match_id), innings_count = dplyr::n(),
                       avg_score = mean(innings_runs, na.rm = TRUE), median_score = stats::median(innings_runs, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate(innings_label = paste(format_group, "· Innings", innings_number),
                    hover = paste0("Ground: ", input$profile_venue, "<br>Format: ", format_group,
                                   "<br>Innings: ", innings_number, "<br>Average score: ", round(avg_score, 1),
                                   "<br>Median score: ", median_score, "<br>Matches: ", matches_count,
                                   "<br>Innings records: ", innings_count))
    validate(need(nrow(s) > 0, "No innings summaries are available for this ground."))
    p <- ggplot2::ggplot(s, ggplot2::aes(x = reorder(innings_label, avg_score), y = avg_score,
                                         fill = factor(innings_number), text = hover)) +
      ggplot2::geom_col(width = 0.72, show.legend = TRUE) +
      ggplot2::coord_flip() +
      ggplot2::geom_text(ggplot2::aes(label = round(avg_score, 1)), hjust = -0.15, color = "#e8edf3", size = 3.5) +
      ggplot2::scale_fill_manual(values = make_innings_palette(s$innings_number), name = "Innings") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
      ggplot2::labs(x = NULL, y = "Average innings runs", caption = "Historical local Cricsheet match data; not a direct soil or bounce measurement.") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#111923", color = NA),
                     panel.background = ggplot2::element_rect(fill = "#111923", color = NA),
                     text = ggplot2::element_text(color = "#e8edf3"),
                     axis.text = ggplot2::element_text(color = "#bbc6d3"),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.position = "bottom", plot.caption = ggplot2::element_text(color = "#8da1b5", size = 8))
    suppressWarnings(ggplotly(p, tooltip = "text")) |>
      plotly::layout(paper_bgcolor = "#111923", plot_bgcolor = "#111923", font = list(color = "#e8edf3"))
  })
  
  output$sourced_ground_map <- renderLeaflet({
    profiles <- read_sourced_pitch_profiles()
    validate(need(nrow(profiles) > 0, "No linked pitch reports are available."))
    coord_path <- "data/reference/venue_coordinates.csv"
    coords <- if (file.exists(coord_path)) readr::read_csv(coord_path, show_col_types = FALSE) else data.frame()
    validate(need(nrow(coords) > 0, "Venue coordinate reference is missing."))
    map_data <- coords |>
      dplyr::inner_join(profiles, by = c("display_name" = "venue")) |>
      dplyr::filter(is.finite(latitude), is.finite(longitude))
    validate(need(nrow(map_data) > 0, "No source-backed grounds could be matched to the coordinate list."))
    map_data$popup_text <- paste0(
      "<strong>", htmltools::htmlEscape(map_data$display_name), "</strong><br>",
      "<b>Surface:</b> ", htmltools::htmlEscape(map_data$soil_type_reported), "<br>",
      "<b>Evidence:</b> ", htmltools::htmlEscape(map_data$verification_status), "<br>",
      "<a href=\"", htmltools::htmlEscape(map_data$source_url), "\" target=\"_blank\" rel=\"noreferrer\">Open report ↗</a>"
    )
    leaflet::leaflet(map_data) |>
      leaflet::addTiles(group = "OpenStreetMap") |>
      leaflet::addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 8,
                                color = "#e7ad50", fillColor = "#e7ad50", fillOpacity = 0.9,
                                weight = 1.5, popup = ~popup_text,
                                label = ~display_name) |>
      leaflet::fitBounds(min(map_data$longitude), min(map_data$latitude),
                         max(map_data$longitude), max(map_data$latitude))
  })
  
}

shinyApp(ui, server)
