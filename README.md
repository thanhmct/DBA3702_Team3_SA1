# Hawker Site Finder — DBA3702, SA1 Team 03

**Live app:** https://thanhmct.shinyapps.io/hawker-site-finder/ (designed for a 16:9 screen)

An R Shiny dashboard that helps a **prospective hawker** decide **which hawker centre to rent a cooked-food stall at**
and **what cuisine to sell**, within their rental budget.

## About the project

NEA tender notices tell a would-be hawker which stalls are up for bid and at what rent, and the Hawker Guide lists
what each centre already sells. What no single place shows is how those stalls compare with **local interest in each
cuisine**, **transit access**, **residents nearby** and **how often a centre's stalls come back to tender**. This app
puts them side by side, so the user can shortlist centres and compare them.

| RQ | Research question | Chart(s) |
|---|---|---|
| RQ1 | How does existing cuisine supply vary across hawker centres? | V1 map, V3 |
| RQ2 | How does cuisine engagement (share of Google Maps reviews) vary across planning areas? | V4 |
| RQ3 | Which centres are over-saturated or under-represented for a cuisine relative to local engagement? | V6, V7 |
| RQ4 | Which centres combine residential density with public-transit access? | V8 |
| RQ5 | Which centres show high tenancy risk through repeated tender listings? | V9 |
| RQ6 | How does rent compare across centres, and which fit the budget? | V2 filters, comparison table |

The full plan (audience, objective, visualisation plan, data requirements, open decisions) is in
[`docs/research_plan.md`](docs/research_plan.md).

**Data:** NEA hawker centres and tender notices, the Hawker Guide (stalls by cuisine and vacancy), Google Maps
reviews (cuisine engagement by planning area), URA Master Plan 2019 (planning areas, subzones, land use),
SingStat population (June 2026), LTA MRT/LRT exits and station passenger volumes (Sep 2026). All of it comes from the
team's **v3 MySQL database**, with a CSV copy in `data/` so the app also runs without MySQL.

## What is on the page

One page in the style of McKinsey Global Institute's "in charts" pages, in five parts, in IBM Plex Sans with
Libre Baskerville headings. Each part opens with a centred heading under a thin rule. Sizes are relative to a
1920 × 1080 design: layout lengths are multiples of `--u` (1 px at 1920 px wide, following the window width) and type
never goes below 16 px; the filters panel is about 22% of the window on every screen.

- **Header** — the ☰ button opens the menu: **Home**, **Map**, **Data visualization**, **Table comparison**,
  **About us** (the part in view is highlighted). The orange line under the header fills as you scroll.

1. **Home** — full-screen opening: photos of Lau Pa Sat slide behind the title every 5 seconds (`www/slides/`, faces
   hidden for privacy); the arrow scrolls down.
2. **Map** ("Where hawker centres are")
   - **Filters** beside the map: region, planning area, **Centres that sell** (cuisine), **Shade planning areas by**
     (number of centres, centres per 10 km², cooked-food stalls, residents, or share of Google Maps reviews — with
     "Any cuisine", the top cuisine's share, named on hover), median rent range (budget), distance to MRT exit,
     min. vacant stalls, and checkboxes for only centres with tender listings / vacant stalls and MRT/LRT exits.
     A region limits the planning areas and a planning area sets its region; the map zooms to the choice.
     **Reset filters** restores everything. The round ‹ button hides the filters; the green **Filters** tab brings them back.
   - Four headline numbers, then the map (it grows to the height of the filters): shaded planning areas, centres as
     circles (size = cooked-food stalls), tooltips on hover and a search box. Clicking a centre opens the
     **centre card** (status, key numbers, cuisine mix with a **Full breakdown ↓** link to V3, and **Add to comparison**).
     ⤢ shows the map full screen. The **centre table** below lists every matching centre, with a CSV download.
3. **Data visualization** — one row per chart, chart on the left and its findings on the right, worked out for what is
   shown: **V3** cuisine supply at a centre, **V4** cuisine engagement map, **V6** review share minus stall share at a
   centre, **V7** engagement–supply gap vs rent (Ridhwan), **V8** MRT access and residents (Lucien), **V9** stalls not
   returning to tender, as a gauge against the median centre (Casper). Each chart has a toolbar (centre search or cuisine
   dropdown, a chip naming the map selection, **Add to comparison**). **The map and the charts stay in sync both ways.**
   A chart with no data for a centre says why instead of showing a zero.
4. **Table comparison** ("Compare your shortlist") — add centres with **Add to comparison** (centre card or any
   chart; click **In comparison** again to remove, with a notification either way) or by double-clicking a centre on
   the map. Columns: median rent, **Open to you?** (advertised stalls your cuisine may bid on), vacant stalls, MRT
   distance (station), residents, latest listing; with a cuisine chosen, that cuisine's stalls and its engagement–supply
   gap. Drag ⋮⋮ to reorder, click a header to sort, ✕ to remove, **Clear all**, and a CSV export with every column we hold
   per centre. An empty shortlist offers **quick starts** (the best 5 centres passing the filters by one criterion:
   lowest rent, most stalls open to you, most vacant stalls, best tender record, nearest to MRT, busiest station, most
   residents, biggest engagement–supply gap), all on one row.
5. **About us** — the project, the team and the data sources, with a panel to rate the dashboard (1–5 stars) and
   subscribe by email. Ratings and emails are **not stored** yet: the visitor only gets a thank-you message.

The round button at the bottom right takes you back to the map.

## Project structure

```
.
├── global.R                  # packages, data loading (MySQL or CSV), derived tables, shared helpers
├── ui.R                      # page layout, styles (app_css) and browser scripts (app_js)
├── server.R                  # filters, map, centre card and table, charts V3–V9, map–chart sync, comparison table
├── README.md
├── .gitignore
├── data/                     # CSV copy of the v3 database (used when MySQL is not running)
│   ├── centres.csv               # one row per hawker centre: location, status, stalls, rent, MRT, residents
│   ├── cuisine_mix.csv           # occupied stalls per centre and cuisine group (Hawker Guide)
│   ├── cuisine_engagement.csv    # share of Google Maps reviews by planning area and cuisine
│   ├── tenders.csv               # NEA tender listings (stall, month, rent, trade)
│   ├── vacancy.csv               # observed, occupied, vacant and renovation stalls per centre (Hawker Guide)
│   ├── mrt_access.csv            # nearest MRT exit, station tap-outs and subzone residents per centre (V8)
│   ├── mrt_exits.csv             # LTA MRT/LRT exit points for the map layer
│   ├── pa_population.csv         # residents per planning area
│   ├── land_use.csv              # land-use shares within a buffer around each centre
│   └── planning_areas.geojson    # URA planning-area boundaries
├── www/
│   └── slides/                   # opening photos of Lau Pa Sat (faces hidden)
├── charts/
│   └── team_charts.R             # the team's original chart scripts, one numbered section per chart (reference only)
├── database/
│   ├── export_v2_to_csv.py       # rebuilds data/*.csv from the SQL dump without MySQL
│   ├── SA1_Team03_Project_hawker_data_v1.sql, …_v1_mariadb.sql, …_v2-1.sql   # earlier dumps (v3 is kept outside the repo for size)
│   ├── hawker_project_data_dictionary_v1_0.pdf
│   └── V8_Lucien/                # Lucien's extraction query and LTA station files (now in the v3 database)
└── docs/
    └── research_plan.md          # research questions, visualisation plan, data requirements, open decisions
```

`charts/team_charts.R` holds, in numbered sections: 1. V3 and 2. V4–V7 (Ridhwan), 3. V8 (Lucien), 4. V9 (Casper).
The app does not source it; the live versions are in `global.R` (`v3_supply_for()`, `engagement`, `centre_gap`,
`v6_gap_for()`, `v8`, `v9_all` / `v9_status`) and `server.R` (`output$v3_plot` … `output$v9_plot`). Run one section at
a time.

## Run it

1. Install the packages once:

   ```r
   install.packages(c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "plotly",
                      "leaflet", "DT", "sf", "scales", "sortable", "DBI", "RMariaDB"))
   ```

2. Choose the data source at the top of `global.R`:
   - `USE_DATABASE <- TRUE` reads MySQL (schema `hawker_project`). Import the v3 dump
     (`SA1_Team03_Project_hawker_data_v3.sql`) into that schema, then set your password first:
     `Sys.setenv(HAWKER_DB_PASSWORD = "your password")`
   - `USE_DATABASE <- FALSE` (or a failed connection) uses the CSV files in `data/`.

3. Open `global.R` in RStudio and click **Run App**, or run `shiny::runApp("path/to/project")`.

The map tiles and fonts load from the internet, so you need a connection.

## Adding or changing a chart

Each chart is one `viz_row()` in the Data visualization section of `ui.R`: the chart frame on the left (`headline`,
`subtitle` "**Metric**, unit", `source`) and the story on the right (`lead` in bold, then `body`), then the toolbar and
the chart output; `detail =` adds an optional box under the story.

```r
viz_row("V8", "RQ4 · V8 · Access and residents",
        "MRT accessibility and local market potential", "<b>Exit distance, station tap-outs and subzone residents</b>",
        "Source: ...",
        uiOutput("v8_lead", inline = TRUE), "The chart compares ...",
        viz_toolbar("Centre", uiOutput("v8_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("v8_add"))),
        plotlyOutput("v8_plot", height = "42vh"),
        detail = uiOutput("v8_detail"))
```

In `server.R`, end the plotly code with `|> mck_plotly()` (from `global.R`) so the chart matches the house style: no
gridlines or axis lines, values labelled on the marks, one accent colour for the key point. For a centre a chart cannot
show, return `message_plot(title, text)` instead of an empty or zero chart.

## Redeploy to shinyapps.io

1. Once per computer: `install.packages("rsconnect")`, then in shinyapps.io go to Account → Tokens → Show, copy the
   `rsconnect::setAccountInfo(...)` line and run it in the RStudio console. Never commit or share it.
2. Keep `USE_DATABASE <- FALSE` in `global.R` (shinyapps.io cannot reach a local MySQL).
3. Run from the project folder:

   ```r
   rsconnect::deployApp(
     appName  = "hawker-site-finder",
     appFiles = c("global.R", "ui.R", "server.R",
                  list.files("data", pattern = "\\.(csv|geojson)$", full.names = TRUE),
                  list.files("www", recursive = TRUE, full.names = TRUE)))
   ```

   The link stays the same. If the live app errors: `rsconnect::showLogs(appName = "hawker-site-finder")`.

## Team

| Member | Role |
|---|---|
| Thanh Mai | Dashboard, map and comparison table |
| Zhen Rong | Database, rent and budget |
| Ridhwan | Cuisine supply and demand (V3, V4, V6, V7) |
| Lucien | Access to MRT and residents (V8) |
| Casper | The tender market (V9) |
