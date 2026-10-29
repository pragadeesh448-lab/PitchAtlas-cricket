# PitchAtlas — R / Shiny cricket data visualization project

**PitchAtlas: Global Cricket Ground, Pitch Behaviour and Playing Conditions Explorer**

The primary application is built in **R and Shiny** for the Data Visualization lab. The Hatchable page is only an optional project companion; it is not the R analytics application.

## What works from the local Cricsheet data

- Format, ground and year filters over the locally processed Test, ODI and men's T20I archives.
- Average innings scores and sample counts; Test innings 1–4 are retained, while limited-overs Super Overs are excluded from regular innings averages.
- Year-by-year historical scoring patterns and ground comparisons.
- Interactive Leaflet world map with 49 stadium/venue markers. These are approximate centre coordinates added for visualization and must be checked against authoritative mapping sources before publishing as surveyed locations.
- Ground Data reference: standard adult pitch size, a source-linked starter list of historical soil/bounce reports, and venue-wise historical match behaviour computed from the user's Cricsheet records.
- Weather-context tab using NASA POWER's gridded hourly data for temperature, dew point, humidity, wind and precipitation. This estimates atmospheric conditions that may be associated with dew; it is not a physical dew sensor.

## Features that require the optional enriched delivery dataset

The standard Cricsheet archives do not include physical pitch-impact coordinates, shot direction or wagon-wheel locations. To use the Bowling Length module's pitch-zone map, wicket rates, wagon wheel and runs-by-ground-sector pie chart, download the enriched data and load its three CSVs in the app.

**Dataset:** [The Ultimate Ball-by-Ball Cricket Dataset on Kaggle](https://www.kaggle.com/datasets/ariadaikalam/the-ultimate-ball-by-ball-cricket-dataset)

Download and extract these files:

1. `ball_by_ball_data.csv` — base delivery facts, including batter runs and wickets.
2. `cricinfo_tracking.csv` — recorded ball length, shot direction and wagon-wheel coordinates where tracked.
3. `matches.csv` — match ID and venue metadata.

In the Shiny app, open **Bowling Length**, choose each file in the three upload controls, and click **Join and load the three source files**. The join uses `match_id`, `innings`, and `delivery_number`; duplicate delivery keys stop the join rather than silently multiplying records. Only required columns are imported to reduce memory use.

The source's tracking coverage is incomplete. The app preserves `full toss`, `yorker`, `good length`, `short of a good length` and `short` as distinct recorded categories. It does not relabel `short of a good length` as `good length`, nor invent `hard length` records when the source does not explicitly provide that label. The pitch bands are schematic categories, not measured landing coordinates. The wagon-wheel plot uses source-recorded angle/distance, and the ground-sector pie counts batter runs from deliveries with a recorded shot direction.

## Pitch surface and dimensions

`data/reference/pitch_surface_reference.csv` holds 49 ground profiles. Only records with linked reports are described as reported; notes can be historical or match-specific rather than a permanent stadium-wide classification. The starter list has **8 linked reports** and **41 records needing source review**. Missing soil/bounce claims are left unfilled rather than inferred from scoring outcomes.

`data/reference/ground_dimensions_template.csv` contains the standard adult pitch size **20.12 m × 3.05 m** based on [MCC Law 6 — The Pitch](https://www.lords.org/MCC/The-Laws-of-Cricket/The-pitch). This is a law reference, not a stadium-specific pitch survey. Straight and square boundary measurements remain blank until a published measurement and date are supplied.

## Install and run in RStudio

1. Extract the project ZIP over your existing PitchAtlas project folder. **Do not delete your existing `data/processed/` folder**; it contains the Cricsheet results already downloaded.
2. Open `PitchAtlas.Rproj` in RStudio.
3. Run once:

   ```r
   source("R/00_setup_packages.R")
   ```

4. Since you have already processed the Cricsheet files, do not rerun ingestion unless you intentionally want to rebuild them. If needed:

   ```r
   source("R/01_ingest_cricsheet.R")
   ```

5. Run the reference/data checks:

   ```r
   source("R/04_validate_project.R")
   ```

6. Start the app:

   ```r
   shiny::runApp()
   ```

The Ground Explorer uses `data/processed/innings.rds`; the updated app avoids loading the multi-million-row `deliveries.rds` into memory for the main dashboard. If an error appears, copy the complete RStudio Console error, including the first `Error:` line.

## Main files

- `app.R` — R Shiny application.
- `R/00_setup_packages.R` — installs required R packages.
- `R/01_ingest_cricsheet.R` — downloads and parses official Cricsheet JSON archives.
- `R/02_analysis_helpers.R` — shared format, length and shot-direction normalization.
- `R/03_weather_context.R` — NASA POWER hourly weather requests and daily summaries.
- `data/reference/venue_coordinates.csv` — 49 approximate map points and venue-name patterns.
- `data/reference/pitch_surface_reference.csv` — source-tracked soil/bounce starter records.
- `data/reference/ground_dimensions_template.csv` — standard pitch size and blank ground-specific boundary fields.

## Interpretation and caveats

- A historical scoring/wicket profile is a statistical behaviour indicator, not a physical pitch-condition measurement.
- The wicket-delivery rate counts any recorded wicket on a delivery; not every dismissal is credited to the bowler. Run-outs and retirements require a separate dismissal-type analysis for a bowler-credit rate.
- Shot-sector pie slices sum **batter runs only**, not wides, byes or leg-byes, because those extras cannot reliably be attributed to the struck-shot direction.
- The map shows only venues in this 49-ground reference table. Coordinates are approximate starter positions and should be verified before final academic publication.
- This project was statically reviewed in the build environment, but the full R runtime was not available there for a complete live execution test. Run it in your installed RStudio and use any console message for the next fix.
