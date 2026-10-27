# Explore the project

From the project root, run:

```r
shiny::runApp("app")
```

The app reads `data/processed/panel.csv`; run the data pipeline first. It never substitutes generated observations when data is missing. Restart the app after rebuilding the panel.

Choose an SIC industry group, optionally compare one company with that group, and switch between next fiscal-year revenue growth and the complete 12-month return above SPY. The data tab includes the selected rows and a CSV download. Charts are descriptive; the report contains the controlled regressions and data exclusions.
