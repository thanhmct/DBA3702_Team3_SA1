# Hawker Site Finder — Shiny app (DBA3702, SA1 Team 03)

**Live app:** https://thanhmct.shinyapps.io/hawker-site-finder/ (designed for a 1920×1080 screen)

R Shiny version of the Figma wireframe, 2 tabs, each with its own filters:
1. **Explore** — the one map (V1+V2): planning-area shading, hawker centres, MRT exits, legend below the map,
   bordered centre card beside it, centre table below. Sidebar filters grouped by type: dropdowns (region,
   planning area, centres that sell, shade by), sliders (budget = max rent, MRT distance, min. vacant stalls),
   checkboxes (tender listings only, MRT exits).
2. **Cuisine Gap** — empty named spaces V3–V10 for the team's charts.

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

## Adding the synthetic survey

Save it as `data/cuisine_survey.csv` with these columns:

| planning_area | age_group | cuisine_group | n_people |
|---|---|---|---|
| WOODLANDS | 20-29 | Western | 34 |

- `planning_area` uses the same names as the database (`WOODLANDS`, `ANG MO KIO`, …).
- `cuisine_group` must be one of the 11 groups in `group_cuisine()` in `global.R`:
  Chinese, Malay & Indonesian, Indian, Western, Japanese & Korean, Thai & Vietnamese,
  Middle Eastern, Local & Other, Desserts & Snacks, Beverages, Vegetarian.

Without the file, the Cuisine Gap tab shows supply only.

## Notes on the data

- The SQL dump uses the MySQL 8 collation `utf8mb4_0900_ai_ci`. On MariaDB/XAMPP replace it
  with `utf8mb4_unicode_ci` before importing.
- One stall can be listed in several months. "Listings" counts rows; "distinct stalls" counts `stall_id`.
- MRT distances are straight-line, not walking routes.
- Compare Centres: pick planning areas, MRT station(s) and max distance, a residents range and a competitor range for your cuisine. Centres with a tender listing that pass every filter appear on the map; click them to add to the comparison table (no limit). There is no score or ranking.
  Missing values count as neutral (50).
