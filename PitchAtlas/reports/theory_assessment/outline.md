# Theory Assessment Outline — PitchAtlas

This file is a planning scaffold for the CSI3005 theory case study, not the final submitted assessment.

## Case study title
PitchAtlas: Visual Analysis of Global Cricket Ground Performance and Historical Playing Conditions

## Intended audience and decision objective
Cricket fans, analysts and students exploring how venue performance varies by format, innings position and year. The visualization supports venue comparison and interpretation of historical patterns; it is not a guaranteed match predictor.

## Candidate analytical questions
1. How does the average innings score at a venue vary by innings position and format, and how has it changed across years?
2. Among deliveries with available commentary-derived length labels, how does the recorded wicket-delivery rate vary by length category at selected venues?

## Two visualization candidates to sketch
- Grouped bar chart: innings position on x-axis, average score on y-axis, color for format; annotate innings/match counts. Compare with a table or stacked bar and justify why a grouped bar supports direct comparison.
- Time-series line chart: year on x-axis, average score on y-axis, separate lines for innings position; show confidence/sample context or counts. Compare with an animated bar chart and justify when trend continuity matters.

## Flawed vs corrected sketch
Potential flawed chart: truncated y-axis and color-only categories with a legend too far away. Corrected chart: zero baseline for bars, labelled categories, accessible palette, direct sample counts and consistent axis scales. Use hypothetical values only when clearly marked illustrative.

## Streaming change requirement
Treat newly updated scorecards as incoming records. Suggest batch refresh after each match finishes or at a daily interval, show recent matches in a short window and long-range aggregates separately. Explain the trade-off between rapid change detection and stable historical trends.

## Replace this scaffold with actual sketches and course-specific wording before submission.
