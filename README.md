# Hawker Site Finder — Shiny app (DBA3702, SA1 Team 03)

**Live app:** https://thanhmct.shinyapps.io/hawker-site-finder/ (designed for a 1920×1080 screen)

Helps a prospective hawker decide **which hawker centre to rent a stall at** and **what cuisine to sell**.

## What is on the page

One page in the style of McKinsey Global Institute's "in charts" pages, in five parts, all in a
sans-serif font (IBM Plex Sans). Each part opens with a centred heading under a thin rule.

- **Header** — the ☰ button (top left) opens the menu: **Home**, **Data visualization**, **Map**,
  **Table comparison**, **About us** (the part in view is highlighted). The header is transparent over the opening photos
  and turns white below them; the orange line under it fills as you scroll. (No logo: removed on the
  team's request.)

1. **Home** — full-screen opening: photos of Lau Pa Sat slide horizontally behind the title every
   5 seconds (`www/slides/`, faces hidden with a motion-streak effect for privacy); the arrow scrolls down.
2. **Data visualization** — one row per chart, V3–V10, each filling one screen. V3 and V4 and V7 (Ridhwan) and V8
   (Lucien) and V9 (Casper) are drawn live from the data (V3 has a search box to pick any hawker centre; each has a labelled toolbar above the chart (a search box for V3, a cuisine dropdown for V4 and V7, a two-way switch for V9), ; hover for details), with findings on the right. The remaining rows await their charts/data.
3. **Map** ("Where hawker centres are") — the filters sit in a sidebar beside the map (its own arrow hides
   or shows them): dropdowns (region, planning area, centres that sell, shade planning areas by), sliders
   (max median rent = budget, distance to MRT, min. vacant stalls) and checkboxes (only centres with tender
   listings, show MRT/LRT exits — drawn as a red MRT train icon). A region limits the planning areas and a
   planning area sets its region; the cuisine list and slider ranges follow the chosen area, and the map
   zooms to it. **Reset filters** restores everything. Then four headline numbers and the full-width map:
   planning areas shaded by the chosen measure, hawker centres as circles, tooltips on hover, a search box,
   and the legend below. Clicking a circle (or a row of the centre table) opens the **centre card** at the
   top right of the map: status, key numbers, cuisine mix and a **Compare** button. Expanding the map card
   (⤢) shows the map full screen. The **centre table** below lists every matching centre with a CSV download.
4. **Table comparison** ("Compare your shortlist") — add centres with **Compare** on the centre card or by
   **double-clicking** a centre on the map. One row per centre: median rent, MRT distance (station),
   residents in the subzone, stalls of the chosen cuisine, latest listing. Drag ⋮⋮ to reorder, click a
   column header to sort, ✕ to remove, **Clear all**, CSV download. An empty shortlist offers quick starts
   (lowest rent, most vacant stalls, nearest to MRT).
5. **About us** — the project, the team (SA1 Team 03) and the data sources, with a small panel to rate
   the dashboard (1–5 stars) and subscribe with an email address. Ratings and emails are **not stored**
   yet: the visitor only gets a thank-you message.

The round ↑ button at the bottom right takes you back to the top.

## Adding a chart (V3–V10)

Each chart has a row in the Data visualization section of `ui.R`:
`viz_row(code, label, headline, subtitle, source, lead, body, ...)` — the chart frame on the left
(`headline`, `subtitle` "**Metric**, unit", `source`) and the story on the right (`lead` in bold, then
`body`). Put the chart output as the last argument, and update the headline and story once the chart
shows its finding:

```r
viz_row("V9", "RQ5 · V9 · Relisting",
        "Five centres relist more than half their stalls", "<b>Listings per distinct stall</b>, ratio",
        TENDER_SOURCE,
        "Stalls that keep coming back are mostly reserved for Indian or halal food.",
        "The bars rank centres by tender listings per distinct stall ...",
        plotlyOutput("v9_plot", height = 300))
```

In `server.R`, end the plotly code with `|> mck_plotly()` (defined in `global.R`) so the chart matches the
house style: no gridlines or axis lines, values labelled on the marks, one accent colour for the key point.
Earlier chart code for V3–V7 is in `docs/old_chart_code.R` (not loaded by the app) for reuse.

## Files

| File | What it does |
|---|---|
| `global.R` | Loads packages and data, groups cuisines, builds the tables, shared helpers (`base_map()`, `mck_plotly()`, …) |
| `ui.R` | The page: header, sidebar, sections, exhibits, styles (`app_css`) and small scripts (`app_js`) |
| `server.R` | Filters, map, centre card, centre table, comparison table |
| `data/*.csv` | Copy of the v2 database tables, used when MySQL is not running |
| `database/SA1_Team03_Project_hawker_data_v2-1.sql` | Full supplied MySQL database dump for local reproduction |
| `charts/V3_Ridhwan/V3.R` | Ridhwan's original V3 script (reads MySQL); the app version is `v3_supply` in `global.R` and `output$v3_plot` in `server.R` |
| `charts/V8_Lucien/V9_2026_graph.R` | Lucien's original V8 script; reads `data/hawker_data_2026.csv`. The app version is `v8` in `global.R` and `output$v8_plot` in `server.R` |
| `charts/V9_Casper/CasperCode.R` | Casper's V9 script (reads MySQL); the app version is `v9_all` in `global.R` and `output$v9_plot` in `server.R` |
| `charts/V4_V7_Ridhwan/` | Ridhwan's V4–V7 script (heatmap, map, age profile, gap bar, gap vs rent) with its output images; the app versions of V4 and V7 are in `global.R` (`engagement`, `centre_gap`) and `server.R` (`v4_map`, `v7_plot`) |
| `data/google_maps_cuisine_engagement_by_planning_area_v3.csv`, `data/pa_age_population.csv` | Google Maps review share by planning area and cuisine (as supplied); residents by age group per planning area (from the v2 database) |
| `data/hawker_data_2026.csv`, `data/V9_2026_RANKING.csv` | Lucien's V8 SQL export (127 centres used) and his ranking output, as supplied |
| `database/V8_Lucien/` | Lucien's extraction query and LTA station files behind `hawker_data_2026.csv` |
| `data/planning_areas.geojson` | Planning-area boundaries for the map |
| `www/slides/` | Opening photos of Lau Pa Sat (faces already hidden) |
| `docs/old_chart_code.R` | Earlier chart code for V3–V7, not loaded by the app (copy back into `server.R` to reuse) |
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
   - `USE_DATABASE <- TRUE` reads MySQL (`hawker_project`). Import the supplied v2-1 SQL dump
     into that schema, then set your password first:
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
                  list.files("data", pattern = "\\.(csv|geojson)$", full.names = TRUE),
                  list.files("www", recursive = TRUE, full.names = TRUE)))
   ```

   The link stays the same. If the live app errors: `rsconnect::showLogs(appName = "hawker-site-finder")`.
