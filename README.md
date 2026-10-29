# Hawker Site Finder — Shiny app (DBA3702, SA1 Team 03)

**Live app:** https://thanhmct.shinyapps.io/hawker-site-finder/ (designed for a 1920×1080 screen)

Helps a prospective hawker decide **which hawker centre to rent a stall at** and **what cuisine to sell**.

## What is on the page

One page in the style of McKinsey Global Institute's "in charts" pages, in five parts, all in a
sans-serif font (IBM Plex Sans). Each part opens with a centred heading under a thin rule.
Every size is relative: the design size is a 16:9 screen of 1920 × 1080, and the whole page (type, map, buttons,
spacing) scales in proportion with the window width on other screens (`html { font-size }` in `app_css`, with all
lengths in `rem`); below about 1320 px wide it stops shrinking and the layout reflows.

- **Header** — the ☰ button (top left) opens the menu: **Home**, **Map**, **Data visualization**,
  **Table comparison**, **About us** (the part in view is highlighted). The header is transparent over the opening photos
  and turns white below them; the orange line under it fills as you scroll. (No logo: removed on the
  team's request.)

1. **Home** — full-screen opening: photos of Lau Pa Sat slide horizontally behind the title every
   5 seconds (`www/slides/`, faces hidden with a motion-streak effect for privacy); the arrow scrolls down.
2. **Map** ("Where hawker centres are") — the filters sit in a sidebar beside the map (its own arrow hides
   or shows them): dropdowns (region, planning area, centres that sell, shade planning areas by), sliders
   (max median rent = budget, distance to MRT, min. vacant stalls) and checkboxes (only centres with tender
   listings, only centres with vacant stalls, show MRT/LRT exits — drawn as a red MRT train icon). "Shade planning
   areas by" includes the share of Google Maps reviews for the cuisine in "Centres that sell". A region limits the planning areas and a
   planning area sets its region; the cuisine list and slider ranges follow the chosen area, and the map
   zooms to it. **Reset filters** restores everything. Then four headline numbers and the full-width map:
   planning areas shaded by the chosen measure, hawker centres as circles, tooltips on hover, a search box,
   and the legend below. Clicking a circle (or a row of the centre table) opens the **centre card** at the
   top right of the map: status, key numbers, cuisine mix (with a **Full breakdown ↓** link to V3) and an **Add to comparison** button (once added it reads **In comparison**; a second click removes the centre, with a notification either way). Expanding the map card
   (⤢) shows the map full screen. The **centre table** below lists every matching centre with a CSV download.
3. **Data visualization** — one row per chart, each filling one screen, drawn live from the data: V3, V4, V6
   and V7 (Ridhwan), V8 (Lucien) and V9 (Casper). Each chart has a labelled toolbar (a centre search box for V3
   and V6, a cuisine dropdown for V4 and V7, a chip naming the map selection) with an **Add to comparison** button at its
   right edge, and its findings on the right, worked out for what is shown. **The map and the charts stay in
   sync both ways**: a centre picked on the map moves V3, V6, V8 and V9 to it and is highlighted on V4 and V7;
   picking a centre or cuisine in a chart (or clicking a dot on V7/V8, an area on V4) updates the map. A chart
   that has no data for the centre says why instead of showing a zero. V8 adds a fact box for the centre; V9 is a
   gauge against the median centre (grey when there is too little tender activity to judge).
4. **Table comparison** ("Compare your shortlist") — add centres with **Add to comparison** on the centre card or by
   **double-clicking** a centre on the map, or with **Add to comparison** on any chart. One row per centre: median rent,
   **Open to you?** (stalls advertised that your cuisine may bid on), vacant stalls, MRT distance (station),
   residents in the subzone and the latest listing; with a cuisine chosen, also that cuisine's stalls and its
   engagement – supply gap. Drag ⋮⋮ to reorder, click a
   column header to sort, ✕ to remove, **Clear all**, and a CSV export that holds the table plus every other column
   we have about each centre (address, coordinates, status, tender and vacancy figures, land use, station activity,
   stalls per cuisine and review shares). An empty shortlist offers quick starts that
   add the best 5 centres passing the map filters by one criterion: lowest rent, most stalls open to you, most
   vacant stalls, best tender record, nearest to MRT, busiest station, most residents, and (with a cuisine
   chosen) the biggest engagement – supply gap.
5. **About us** — the project, the team (SA1 Team 03) and the data sources, with a small panel to rate
   the dashboard (1–5 stars) and subscribe with an email address. Ratings and emails are **not stored**
   yet: the visitor only gets a thank-you message.

The round button at the bottom right takes you back to the map.

## Adding or changing a chart

Each chart has a row in the Data visualization section of `ui.R`:
`viz_row(code, label, headline, subtitle, source, lead, body, ...)` — the chart frame on the left
(`headline`, `subtitle` "**Metric**, unit", `source`) and the story on the right (`lead` in bold, then
`body`). Put the toolbar and the chart output after them; `detail =` adds an optional box under the story:

```r
viz_row("V8", "RQ4 · V8 · Access and residents",
        "MRT accessibility and local market potential", "<b>Exit distance, station tap-outs and subzone residents</b>",
        "Source: ...",
        uiOutput("v8_lead", inline = TRUE), "The chart compares ...",
        viz_toolbar("Centre", uiOutput("v8_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("v8_add"))),
        plotlyOutput("v8_plot", height = "42vh"),
        detail = uiOutput("v8_detail"))
```

In `server.R`, end the plotly code with `|> mck_plotly()` (defined in `global.R`) so the chart matches the
house style: no gridlines or axis lines, values labelled on the marks, one accent colour for the key point.
For a centre a chart cannot show, return `message_plot(title, text)` instead of an empty or zero chart.

## Files

| File | What it does |
|---|---|
| `global.R` | Loads packages and data (MySQL or the CSVs), builds the tables, shared helpers (`base_map()`, `mck_plotly()`, `message_plot()`, `review_share_by_area()`, …) |
| `ui.R` | The page: header, sidebar, sections, exhibits, styles (`app_css`) and small scripts (`app_js`) |
| `server.R` | Filters, map, centre card, centre table, charts V3–V9, map–chart sync, comparison table |
| `data/*.csv` | Copy of the v3 database tables and queries, used when MySQL is not running (rebuild with `database/export_v2_to_csv.py`) |
| `database/SA1_Team03_Project_hawker_data_v2-1.sql` | Earlier supplied MySQL dump (the app now uses v3: `SA1_Team03_Project_hawker_data_v3.sql`, kept outside the repo for size) |
| `charts/team_charts.R` | The team's original chart scripts in one file, numbered: 1. V3 and 2. V4–V7 (Ridhwan), 3. V8 (Lucien), 4. V9 (Casper). Reference only, not sourced by the app; the live versions are `v3_supply_for()`, `engagement`, `centre_gap`, `v6_gap_for`, `v8`, `v9_all` / `v9_status` in `global.R` and `output$v3_plot` … `output$v9_plot` in `server.R` |
| `data/cuisine_engagement.csv`, `data/mrt_access.csv` | Google Maps review engagement by planning area and cuisine (database table); MRT access, station tap-outs and subzone residents per centre for V8 (database query) |
| `database/V8_Lucien/` | Lucien's extraction query and LTA station files (the same data is now in the v3 database) |
| `data/planning_areas.geojson` | Planning-area boundaries for the map |
| `www/slides/` | Opening photos of Lau Pa Sat (faces already hidden) |
| `database/export_v2_to_csv.py` | Rebuilds the CSVs from the SQL dump (v3) without MySQL |
| `docs/research_plan.md` | Research questions, visualisation plan and data requirements |

## Run it

1. Install the packages once:

   ```r
   install.packages(c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly",
                      "leaflet", "DT", "sf", "scales", "sortable", "DBI", "RMariaDB"))
   ```

2. Choose the data source at the top of `global.R`:
   - `USE_DATABASE <- TRUE` reads MySQL (`hawker_project`). Import the supplied v3 SQL dump
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
