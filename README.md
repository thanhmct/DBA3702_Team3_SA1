# Hawker Site Finder — Shiny app (DBA3702, SA1 Team 03)

**Live app:** https://thanhmct.shinyapps.io/hawker-site-finder/ (designed for a 1920×1080 screen)

One scrolling page in the style of McKinsey Global Institute's "2025 in charts": themed sections with a
serif heading, and exhibits that open with a one-sentence takeaway, a "**Metric**, unit" subtitle, the chart
and a source line. Jump links in the header move between sections; one sidebar holds every filter.
1. **Where hawker centres are** — the one map (V1+V2) with a live takeaway headline, a callout on the top
   planning area, McKinsey-style tooltips, the legend below the map, the centre card beside it and the
   centre table (CSV download) below.
2. **Compare your shortlist** (V11) — add centres with **Compare** on the centre card or by double-clicking a
   centre on the map; drag ⋮⋮ to reorder, click a header to sort, ✕ to remove, CSV download. An empty
   shortlist offers quick starts (lowest rent, most vacant stalls, nearest to MRT).
3. **Cuisine supply and demand** (V3–V7), **Access and residents** (V8), **Tenancy risk** (V9–V10) —
   exhibit frames for the team's charts. Put a chart inside `exhibit(...)` in `ui.R` and finish the plotly
   code with `|> mck_plotly()` (in `global.R`) so it matches the house style.

## Files

| File | What it does |
|---|---|
| `global.R` | Loads packages and data, groups cuisines and land use, builds the tables every tab uses |
| `ui.R` | Layout (bslib `page_navbar`, sidebar, cards) |
| `server.R` | Charts, maps and tables |
| `data/*.csv` | Copy of the SQL database tables, used when MySQL is not running |
| `data/planning_areas.geojson` | Planning-area boundaries for the maps |
| `data/cuisine_survey.csv` | *(not included yet)* the synthetic survey — add it to turn on the demand charts |

## Run it

1. Install the packages once:

   ```r
   install.packages(c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly",
                      "leaflet", "DT", "sf", "scales", "DBI", "RMariaDB"))
   ```

2. Choose the data source at the top of `global.R`:
   - `USE_DATABASE <- TRUE` reads MySQL (`hawker_project`). Set your password first:
     `Sys.setenv(HAWKER_DB_PASSWORD = "your password")`
   - If the connection fails, the app uses the CSV files in `data/` automatically.

3. Open `global.R` in RStudio and click **Run App**, or run `shiny::runApp("path/to/Group Project")`.

The maps load their background tiles from the internet, so you need a connection.

## Redeploy to shinyapps.io

1. Once per computer: `install.packages("rsconnect")`, then in shinyapps.io go to Account → Tokens → Show,
   copy the `rsconnect::setAccountInfo(...)` line and run it in the RStudio console. Never commit or share it.
2. Keep `USE_DATABASE <- FALSE` in `global.R` (shinyapps.io cannot reach a local MySQL).
3. Run from the project folder (the file list keeps `data/v1_backup/` off the server):

   ```r
   rsconnect::deployApp(
     appName  = "hawker-site-finder",
     appFiles = c("global.R", "ui.R", "server.R",
                  list.files("data", pattern = "\\.(csv|geojson)$", full.names = TRUE)))
   ```

   The link stays the same. If the live app errors: `rsconnect::showLogs(appName = "hawker-site-finder")`.
