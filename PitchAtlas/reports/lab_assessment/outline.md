# Lab Assessment Outline — PitchAtlas

Use this scaffold with the CSI3005 lab format. Fill in real findings after running R code. Do not claim unobserved patterns.

## 1. Title and problem/domain
PitchAtlas: Global Cricket Ground, Pitch Behaviour and Playing Conditions Explorer.

## 2. Objective
Explore venue-level innings scores and historical trends in Test, ODI and T20 International matches; evaluate wicket-delivery rates by commentary-derived bowling length where labels exist; add source-verified ground reference data and weather context in later iterations.

## 3. Dataset and sources
- Cricsheet official JSON archives: https://cricsheet.org/downloads/
- Optional commentary-enriched ball-by-ball dataset: https://github.com/Ari-Adaikalam/The-Ultimate-Ball-by-Ball-Cricket-Dataset
- NASA POWER hourly weather API (future module): https://power.larc.nasa.gov/docs/services/api/temporal/hourly/
- Manually sourced dimensions and coordinate references recorded in CSVs under `data/reference/`.

## 4. Important attributes
Match ID/date, format, venue, innings number, batting team, total innings runs, ball length, recorded wicket indicator, year, latitude/longitude and source metadata.

## 5. Preprocessing
Download and extract archives; parse JSON; standardize match format; derive innings number and year; aggregate deliveries to innings; normalize optional length labels; preserve unknown/missing categories; record data coverage.

## 6. Visualizations
- Average score by innings and format.
- Year-wise line chart for venue scoring trends.
- Interactive map for grounds with coordinates.
- Cross-ground comparison.
- Optional wicket-delivery rate by delivery length.

## 7. Interpretation / findings
Write only after executing the app and checking the calculated tables. Mention sample sizes and missing coverage.

## 8. Limitations
Commentary-derived length labels are incomplete and may vary in wording. Weather is contextual, not direct pitch-dew measurement. Dimensions and coordinate coverage are partial.

## 9. R code
Attach the actual R scripts and describe how to reproduce the results.
