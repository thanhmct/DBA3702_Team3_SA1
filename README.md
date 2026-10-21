# Hawker Site Finder — Shiny app (DBA3702, SA1 Team 03)

**Live app:** https://thanhmct.shinyapps.io/hawker-site-finder/ (designed for a 1920×1080 screen)

Helps a prospective hawker decide **which hawker centre to rent a stall at** and **what cuisine to sell**.

## What is on the page

Themed sections with a
serif heading, and exhibits that open with a one-sentence takeaway, a "**Metric**, unit" subtitle, the chart
and a source line.

- **Header** — jump links to each section (the section in view is highlighted). The line under the header
  fills as you scroll and shows how far down the page you are.
- **Filters** (left sidebar; the ☰ button hides or shows it) — grouped by type:
  dropdowns (region, planning area, centres that sell, shade planning areas by, cuisine to analyse, hawker
  centre for the charts), sliders (max median rent = budget, max distance to MRT, min. vacant stalls) and
  checkboxes (only centres with tender listings, show MRT/LRT exits). A region limits the planning areas and a
  planning area sets its region; the cuisine list and slider ranges follow the chosen area. **Reset filters**
  restores everything.

1. **Where hawker centres are** — a sentence summarising what matches your filters, four headline
   numbers, and the full-width map: planning areas shaded by the chosen measure, hawker centres as circles,
   a callout on the top planning area, tooltips on hover, and the legend below the map. Clicking a circle
   (or a row of the centre table) opens the **centre card** floating at the top right of the map: status,
   key numbers (stalls, median rent, tender listings, vacant stalls, residents, nearest MRT), cuisine mix and
   a **Compare** button. Expanding the map card (⤢) shows the map full screen. The **centre table** below
   lists every matching centre, 10 per page, with a CSV download.
2. **Compare your shortlist** — add centres with **Compare** on the centre card or by
   **double-clicking** a centre on the map (a message confirms it, with a link down to the table). One row
   per centre: median rent, MRT distance (station), residents in the subzone, stalls of the chosen cuisine,
   latest listing. Drag ⋮⋮ to reorder, click a column header to sort, ✕ to remove, **Clear all**, CSV
   download. An empty shortlist explains how to add centres and offers quick starts (lowest rent, most
   vacant stalls, nearest to MRT).
3. **Cuisine supply and demand**, **Access and residents**, **Tenancy risk** —
   exhibit frames for the team's charts (see below).

The round ↑ button at the bottom right takes you back to the top.

## Adding a chart (V3–V10)

Each chart has a frame in `ui.R`, e.g.
`exhibit("V9", "Relisting frequency", "<b>Listings per distinct stall, by release category</b>, ratio", TENDER_SOURCE)`.
Replace the headline with the chart's one-sentence takeaway and put the output inside the frame:

```r
exhibit("V9", "Five centres relist more than half their stalls", "<b>Listings per distinct stall</b>, ratio",
        TENDER_SOURCE, plotlyOutput("v9_plot", height = 300))
```

In `server.R`, end the plotly code with `|> mck_plotly()` (defined in `global.R`) so the chart matches the
house style: no gridlines or axis lines, values labelled on the marks, one accent colour for the key point.
Earlier chart code for V3–V7 is kept in `server.R` (marked "NOT DISPLAYED") for reuse.

## Files

| File | What it does |
|---|---|
| `global.R` | Loads packages and data, groups cuisines, builds the tables, shared helpers (`base_map()`, `mck_plotly()`, …) |
| `ui.R` | The page: header, sidebar, sections, exhibits, styles (`app_css`) and small scripts (`app_js`) |
| `server.R` | Filters, map, centre card, centre table, comparison table |
| `data/*.csv` | Copy of the v2 database tables, used when MySQL is not running |
| `data/planning_areas.geojson` | Planning-area boundaries for the map |
| `data/cuisine_survey.csv` | *(not included yet)* the synthetic survey — add it to turn on the demand charts |
| `database/export_v2_to_csv.py` | Rebuilds the CSVs from the v2 SQL dump without MySQL |
| `docs/research_plan.md` | Research questions, visualisation plan and data requirements |

## Run it

1. Install the packages once:

   ```r
   install.packages(c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly",
                      "leaflet", "DT", "sf", "scales", "sortable", "DBI", "RMariaDB"))
   ```

2. Choose the data source at the top of `global.R`:
   - `USE_DATABASE <- TRUE` reads MySQL (`hawker_project`). Set your password first:
     `Sys.setenv(HAWKER_DB_PASSWORD = "your password")`
   - `USE_DATABASE <- FALSE` (or a failed connection) uses the CSV files in `data/`.

3. Open `global.R` in RStudio and click **Run App**, or run `shiny::runApp("path/to/Group Project")`.

The map tiles and fonts load from the internet, so you need a connection.

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
