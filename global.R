# =============================================================================
# Hawker Site Finder — global.R
# DBA3702 Group Project (SA1 Team 03)
#
# Shiny runs this file first. It loads packages, reads the data (from the
# MySQL database, or from the CSV copies in data/ if the database is not
# available), and prepares the tables that every tab uses.
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(sf)
library(scales)
library(sortable)  # drag-to-reorder rows in the comparison table

sf_use_s2(FALSE)

# -----------------------------------------------------------------------------
# 1. Data source settings
# -----------------------------------------------------------------------------
# TRUE  = read from the MySQL database (import database/SA1_Team03_Project_hawker_data_v3.sql first)
# FALSE = read the CSV copies in data/ (no database needed)
# If the database connection fails, the app falls back to the CSV files.
USE_DATABASE <- FALSE

DB_CONFIG <- list(
  dbname   = "hawker_project",
  host     = "127.0.0.1",
  port     = 3306,
  user     = "root",
  # Set your password with Sys.setenv(HAWKER_DB_PASSWORD = "...") before
  # running the app, or type it here (do not commit a real password).
  password = Sys.getenv("HAWKER_DB_PASSWORD", "")
)

SQL_CENTRES <- "
  SELECT h.centre_id, h.centre_name, h.address, h.postal_code,
         h.latitude, h.longitude, h.nea_status,
         h.number_of_cooked_food_stalls AS stalls,
         s.subzone_name, s.planning_area_name, s.region_name,
         p.resident_population, p.hdb_population,
         m.nearest_exit_distance_m, m.exits_within_500m, m.stations_within_500m,
         e.station_name AS nearest_mrt_station
  FROM hawker_centres h
  LEFT JOIN subzones s            ON s.subzone_id = h.subzone_id
  LEFT JOIN v_subzone_population_2026 p ON p.subzone_id = h.subzone_id
  LEFT JOIN centre_mrt_access m   ON m.centre_id  = h.centre_id
  LEFT JOIN mrt_exits e           ON e.exit_id    = m.nearest_exit_id"

SQL_PA_POPULATION <- "
  SELECT s.planning_area_name, s.region_name,
         SUM(p.resident_population) AS resident_population,
         SUM(p.hdb_population)      AS hdb_population
  FROM subzones s
  LEFT JOIN v_subzone_population_2026 p ON p.subzone_id = s.subzone_id
  GROUP BY s.planning_area_name, s.region_name"

# Vacant units per centre: latest Hawker Guide observation per unit (same rule as
# v_cuisine_mix). Community-reported, not official NEA occupancy.
SQL_VACANCY <- "
  SELECT centre_id,
         SUM(occupancy_status = 'vacant')     AS vacant_units,
         SUM(occupancy_status = 'renovation') AS renovation_units,
         SUM(occupancy_status = 'occupied')   AS occupied_units,
         COUNT(*)                             AS observed_units
  FROM (SELECT o.centre_id, o.occupancy_status,
               ROW_NUMBER() OVER (PARTITION BY o.centre_id, o.unit_no_normalised
                                  ORDER BY o.observed_on DESC, o.observation_id DESC) AS obs_rank
        FROM stall_observations o
        WHERE o.unit_no_normalised IS NOT NULL) latest
  WHERE obs_rank = 1
  GROUP BY centre_id"

# Same query as database/export_v2_to_csv.py (data/cuisine_engagement.csv)
SQL_ENGAGEMENT <- "
  SELECT p.planning_area_name, e.cuisine_group, e.restaurant_count,
         e.total_reviews, e.avg_reviews, e.engagement_share
  FROM cuisine_engagement_by_planning_area e
  JOIN planning_areas p ON p.planning_area_id = e.planning_area_id"

# V8: nearest exit, its station's average daily tap-outs (latest month, weekday + weekend/holiday
# totals / 30) and subzone residents. Same query as database/export_v2_to_csv.py (data/mrt_access.csv)
SQL_MRT_ACCESS <- "
  SELECT h.centre_id, h.centre_name, me.station_name AS nearest_station_name,
         a.nearest_exit_distance_m AS straight_line_distance_m,
         p.resident_population AS subzone_population_2026,
         r.avg_daily_tap_outs, r.avg_daily_movements,
         CASE WHEN me.station_name IS NULL THEN 'NO_NEAREST_STATION'
              WHEN r.station_id IS NULL THEN 'UNMATCHED_STATION_OR_NO_RIDERSHIP'
              WHEN p.resident_population IS NULL THEN 'MISSING_POPULATION'
              ELSE 'MATCHED' END AS match_status
  FROM hawker_centres h
  LEFT JOIN centre_mrt_access a ON a.centre_id = h.centre_id
  LEFT JOIN mrt_exits me ON me.exit_id = a.nearest_exit_id
  LEFT JOIN v_subzone_population_2026 p ON p.subzone_id = h.subzone_id
  LEFT JOIN (SELECT station_id,
                    ROUND(SUM(period_tap_outs) / 30.0, 1) AS avg_daily_tap_outs,
                    ROUND(SUM(total_passenger_movements) / 30.0, 1) AS avg_daily_movements
             FROM station_ridership
             WHERE reference_month = (SELECT MAX(reference_month) FROM station_ridership)
             GROUP BY station_id) r ON r.station_id = me.station_id
  ORDER BY h.centre_id"

read_from_db <- function() {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RMariaDB", quietly = TRUE)) {
    stop("Packages DBI and RMariaDB are not installed")
  }
  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    dbname = DB_CONFIG$dbname, host = DB_CONFIG$host, port = DB_CONFIG$port,
    user = DB_CONFIG$user, password = DB_CONFIG$password
  )
  on.exit(DBI::dbDisconnect(con))
  # BIGINT / unsigned INT columns arrive as integer64; turn them into plain numbers
  q <- function(sql) {
    d <- DBI::dbGetQuery(con, sql)
    d[] <- lapply(d, function(x) if (inherits(x, "integer64")) as.numeric(x) else x)
    d
  }
  list(
    centres       = q(SQL_CENTRES),
    tenders       = q("SELECT * FROM v_tender_dashboard"),
    cuisine_mix   = q("SELECT * FROM v_cuisine_mix"),
    land_use      = q("SELECT centre_id, buffer_radius_m, land_use_category, land_use_share
                       FROM centre_land_use_metrics"),
    mrt_exits     = q("SELECT station_name, exit_code, latitude, longitude, rail_type
                       FROM mrt_exits"),
    pa_population = q(SQL_PA_POPULATION),
    engagement    = q(SQL_ENGAGEMENT),
    mrt_access    = q(SQL_MRT_ACCESS),
    vacancy       = q(SQL_VACANCY),
    source        = "MySQL database"
  )
}

read_from_csv <- function() {
  rd <- function(f) read.csv(file.path("data", f), stringsAsFactors = FALSE,
                             encoding = "UTF-8", na.strings = c("", "NA"))
  list(
    centres       = rd("centres.csv"),
    tenders       = rd("tenders.csv"),
    cuisine_mix   = rd("cuisine_mix.csv"),
    land_use      = rd("land_use.csv"),
    mrt_exits     = rd("mrt_exits.csv"),
    pa_population = rd("pa_population.csv"),
    engagement    = rd("cuisine_engagement.csv"),
    mrt_access    = rd("mrt_access.csv"),
    vacancy       = if (file.exists(file.path("data", "vacancy.csv"))) rd("vacancy.csv") else NULL,
    source        = "CSV files in data/"
  )
}

raw <- if (USE_DATABASE) {
  tryCatch(read_from_db(), error = function(e) {
    message("Database not available (", conditionMessage(e), ") - using CSV files instead.")
    read_from_csv()
  })
} else {
  read_from_csv()
}

# -----------------------------------------------------------------------------
# 2. Helper functions
# -----------------------------------------------------------------------------

# The database (v3, v_cuisine_mix and the engagement table) already uses nine main-meal
# cuisine groups, so there is no mapping step here.
CUISINE_GROUPS <- c("Chinese", "Malay / Indonesian", "Indian / South Asian", "Western / European",
                    "Japanese", "Korean", "Thai / Vietnamese", "Middle Eastern / Mediterranean",
                    "Other / Mixed")

group_land_use <- function(x) {
  case_when(
    grepl("^RESIDENTIAL|COMMERCIAL & RESIDENTIAL", x)              ~ "Residential",
    grepl("^COMMERCIAL|HOTEL|WHITE$", x) & !grepl("BUSINESS", x)   ~ "Commercial",
    grepl("BUSINESS", x)                                          ~ "Business & industry",
    grepl("EDUCATION|CIVIC|HEALTH|WORSHIP", x)                    ~ "Education & civic",
    grepl("PARK|OPEN SPACE|SPORTS|WATERBODY|BEACH|AGRICULTURE|CEMETERY", x) ~ "Green & open space",
    grepl("ROAD|RAPID TRANSIT|TRANSPORT|PORT|AIRPORT", x)         ~ "Roads & transport",
    TRUE                                                          ~ "Other"
  )
}
# ---------------------------------------------------------------------------
# Brand colours (template: FMI / Oliver Wyman "Imperative Issues 2022" report)
# ---------------------------------------------------------------------------
BRAND <- list(
  green_dark = "#0D743B",  # primary: headers, navbar, main series
  green      = "#40AD49",  # accent green
  lime       = "#99CA3C",  # second series
  blue       = "#4C83C2",  # third series
  orange     = "#FAA619",  # highlight / hawker centres
  slate      = "#4E5D68",  # lines, borders
  text       = "#404041",
  muted      = "#808285",
  light      = "#EDF0F2"
)
# Sequential (low -> high), used for maps: six steps (residents) and five steps (everything else)
BRAND_SEQ  <- c("#F6F8F9", "#DDECC1", "#BBD980", "#80C683", "#40AD49", "#0D743B")
BRAND_SEQ5 <- c("#F6F8F9", "#DDECC1", "#BBD980", "#40AD49", "#0D743B")

fmt_sgd <- function(x) ifelse(is.na(x), "n/a", paste0("S$", comma(round(x))))
fmt_num <- function(x) ifelse(is.na(x), "n/a", comma(round(x)))
plural  <- function(n, word) paste(comma(n), if (n == 1) word else paste0(word, "s"))

# McKinsey-style look for every plotly chart (V3-V9 and the centre card): no
# gridlines, axis lines or legend, quiet grey text, white hover cards. Label
# values directly on the marks and use one accent colour for the key point.
# Usage: plot_ly(...) |> mck_plotly()   (extra layout() settings can be passed)
mck_plotly <- function(p, ...) {
  axis <- list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE, ticks = "",
               tickfont = list(color = "#58595B"))
  p <- p |>
    layout(font = list(family = "IBM Plex Sans, sans-serif", size = 13, color = "#404041"),
           xaxis = axis, yaxis = axis, showlegend = FALSE,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = "#FFFFFF",
                             font = list(family = "IBM Plex Sans, sans-serif", color = "#1E2B22")),
           margin = list(l = 10, r = 10, t = 10, b = 10))
  if (length(list(...))) p <- layout(p, ...)
  config(p, displayModeBar = FALSE)
}

# UI helpers used by both ui.R and server.R (defined here so both can see them)
tag_note <- function(...) tags$p(class = "small text-muted mb-0", ...)
# "Reset filters" button for the map filters
reset_button <- function(id) {
  actionButton(id, "Reset filters", icon = icon("rotate-left"),
               class = "btn-outline-secondary btn-sm w-100 mb-2")
}

# Chart area with only a grey message, for a centre a chart cannot show (never a misleading zero)
message_plot <- function(title, text) {
  plot_ly(type = "scatter", mode = "markers", x = 0, y = 0, hoverinfo = "none", marker = list(opacity = 0)) |>
    mck_plotly(margin = list(l = 10, r = 10, t = 10, b = 10)) |>
    layout(xaxis = list(visible = FALSE, range = c(-1, 1)), yaxis = list(visible = FALSE, range = c(-1, 1)),
           annotations = list(list(x = 0, y = 0, xref = "x", yref = "y", showarrow = FALSE,
                                   text = paste0("<b>", title, "</b><br>", paste(strwrap(text, 46), collapse = "<br>")),
                                   font = list(size = 14, color = "#808285"))))
}

empty_plot <- function(msg) {
  plotly_empty(type = "scatter", mode = "markers") |>
    layout(annotations = list(text = paste(strwrap(msg, 28), collapse = "<br>"), showarrow = FALSE,
                              font = list(size = 14, color = "#58595B")),
           xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)) |>
    config(displayModeBar = FALSE)
}

# -----------------------------------------------------------------------------
# 3. Prepare tables
# -----------------------------------------------------------------------------

# Tender listings (one row per stall per monthly exercise)
tenders <- raw$tenders |>
  mutate(
    exercise_month = as.Date(exercise_month),
    across(c(approximate_area_m2, median_centre_amr_sgd), as.numeric)
  )

tender_by_centre <- tenders |>
  group_by(centre_id) |>
  summarise(
    n_listings      = n(),
    n_tender_stalls = n_distinct(stall_id),
    median_amr      = median(median_centre_amr_sgd, na.rm = TRUE),
    .groups = "drop"
  )

# Land use around each centre (grouped)
land_use <- raw$land_use |>
  mutate(land_use_share = as.numeric(land_use_share),
         land_group = group_land_use(land_use_category)) |>
  group_by(centre_id, buffer_radius_m, land_group) |>
  summarise(share = sum(land_use_share, na.rm = TRUE), .groups = "drop")

res_share <- land_use |>
  filter(land_group %in% c("Residential", "Commercial")) |>
  pivot_wider(id_cols = c(centre_id, buffer_radius_m), names_from = land_group,
              values_from = share, values_fill = 0) |>
  pivot_wider(id_cols = centre_id, names_from = buffer_radius_m,
              values_from = c(Residential, Commercial), values_fill = 0)
names(res_share) <- c("centre_id", tolower(gsub("_(\\d+)$", "_share_\\1m", names(res_share)[-1])))

# Cuisine supply (occupied, classified stalls from the scraped directory)
cuisine_by_centre <- raw$cuisine_mix |>
  mutate(cuisine_group = cuisine_category) |>
  group_by(centre_id, cuisine_group) |>
  summarise(stalls = sum(observed_stalls), .groups = "drop") |>
  group_by(centre_id) |>
  mutate(share = stalls / sum(stalls)) |>
  ungroup()

# Vacant units per centre (NULL if the vacancy file is missing)
vacancy <- if (is.null(raw$vacancy)) {
  data.frame(centre_id = numeric(0), vacant_units = numeric(0), observed_units = numeric(0))
} else {
  raw$vacancy |> mutate(across(everything(), as.numeric))
}

# Centres: one row per hawker centre with every indicator the app needs
centres <- raw$centres |>
  mutate(across(c(latitude, longitude, stalls, resident_population,
                  nearest_exit_distance_m, exits_within_500m, stations_within_500m),
                as.numeric),
         # "HARBOURFRONT MRT STATION" -> "Harbourfront MRT"
         nearest_mrt_station = tools::toTitleCase(tolower(nearest_mrt_station)) |>
           gsub(pattern = "\\bMrt\\b", replacement = "MRT") |>
           gsub(pattern = "\\bLrt\\b", replacement = "LRT") |>
           gsub(pattern = " Station$", replacement = "")) |>
  left_join(tender_by_centre, by = "centre_id") |>
  left_join(res_share, by = "centre_id") |>
  left_join(vacancy, by = "centre_id") |>
  mutate(
    n_listings      = coalesce(n_listings, 0L),
    n_tender_stalls = coalesce(n_tender_stalls, 0L),
    has_cuisine     = centre_id %in% cuisine_by_centre$centre_id,
    pa_title        = tools::toTitleCase(tolower(planning_area_name)),
    region_title    = tools::toTitleCase(tolower(region_name))
  )

# Top 3 cuisines per centre (map tooltip)
centre_mix <- cuisine_by_centre |>
  group_by(centre_id) |>
  arrange(desc(share), .by_group = TRUE) |>
  summarise(top3 = paste0(head(cuisine_group, 3), " ", percent(head(share, 3), 1), collapse = " · "),
            .groups = "drop")
centres <- centres |> left_join(centre_mix, by = "centre_id")

# Latest tender listing per centre, e.g. "#01-113 · 7.56 m² · May 2026" (comparison table)
latest_listing <- tenders |>
  group_by(centre_id) |>
  arrange(desc(exercise_month), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  transmute(centre_id, latest_month = exercise_month,
            latest_listing = paste0(stall_no_normalised, " · ",
                                    ifelse(is.na(approximate_area_m2), "size n/a",
                                           sprintf("%.2f m²", approximate_area_m2)), " · ",
                                    month.abb[as.integer(format(exercise_month, "%m"))], " ",
                                    format(exercise_month, "%Y")))
centres <- centres |> left_join(latest_listing, by = "centre_id")

# Trade marker of each tendered stall (from its latest listing). The comparison table counts the
# stalls a given cuisine may bid on: unmarked stalls take any one trade (other limits are in NEA's
# PDF notes), ^ = Indian only, + = halal only, +^ = either. Malay / Indonesian is treated as halal.
stall_hint <- tenders |>
  filter(!is.na(stall_id), !is.na(exercise_month)) |>
  group_by(centre_id, stall_id) |>
  arrange(desc(exercise_month), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(centre_id, stall_id, allowed_trade_hint)
open_hints <- function(cuisine) {
  switch(cuisine,
         "Indian / South Asian" = c("subject_to_pdf_notes", "indian_only", "halal_or_indian_only"),
         "Malay / Indonesian"   = c("subject_to_pdf_notes", "halal_only", "halal_or_indian_only"),
         "subject_to_pdf_notes")   # any other cuisine, and "All" (not reserved)
}

cuisine_by_centre <- cuisine_by_centre |>
  left_join(select(centres, centre_id, centre_name, planning_area_name), by = "centre_id")

# Planning areas: polygons + population + land area
pa_population <- raw$pa_population |>
  mutate(resident_population = as.numeric(resident_population))

planning_areas <- st_read(file.path("data", "planning_areas.geojson"), quiet = TRUE) |>
  st_make_valid() |>
  # make_valid() can return GEOMETRYCOLLECTIONs, which leaflet cannot draw
  st_collection_extract("POLYGON") |>
  st_cast("MULTIPOLYGON")
planning_areas$area_km2 <- as.numeric(st_area(st_transform(planning_areas, 3414))) / 1e6
planning_areas <- planning_areas |>
  rename(planning_area_name = planning_area) |>
  left_join(pa_population, by = "planning_area_name") |>
  mutate(pa_title = tools::toTitleCase(tolower(planning_area_name)))

# Singapore outline (union of all planning areas), drawn as a line on every map.
# The buffer out and back in closes slivers between neighbouring areas; keeping
# only each polygon's outer ring drops any holes left inside the country.
sg_land <- planning_areas |>
  st_transform(3414) |>
  st_buffer(50) |>
  st_union() |>
  st_buffer(-50) |>
  st_cast("POLYGON")
sg_border <- st_sfc(lapply(sg_land, function(p) st_polygon(p[1])), crs = st_crs(sg_land)) |>
  st_simplify(dTolerance = 20) |>
  st_boundary() |>
  st_transform(4326)
# Tiny islets can vanish when simplified; empty shapes break leaflet in the browser
sg_border <- sg_border[!st_is_empty(sg_border)]

# Border sits in its own pane above polygons and circles; it ignores the mouse
add_sg_border <- function(map) {
  map |>
    addPolylines(data = sg_border, group = "sg_border", color = "#404041", weight = 2,
                 opacity = 0.9, options = pathOptions(pane = "sg_border", pointerEvents = "none"))
}

# Base map used by every leaflet map in the app. It fits Singapore; pad_right
# leaves room (px) on the right for a panel that floats over the map.
base_map <- function(pad_right = 0) {
  # doubleClickZoom off: double-clicking a centre adds it to the comparison table
  leaflet(options = leafletOptions(minZoom = 10, zoomSnap = 0.25, doubleClickZoom = FALSE)) |>
    # CARTO basemaps now need an API key; Esri's light grey canvas does not
    addProviderTiles(providers$Esri.WorldGrayCanvas) |>
    addMapPane("sg_border", zIndex = 420) |>
    fitBounds(103.62, 1.235, 104.03, 1.47,
              options = list(paddingTopLeft = c(4, 4), paddingBottomRight = c(pad_right, 4))) |>
    add_sg_border()
}

mrt_exits <- raw$mrt_exits |>
  mutate(across(c(latitude, longitude), as.numeric))

# MRT icon: a white train front on a red rounded badge (the colour of Singapore's
# MRT signs). One SVG, used for the map markers, the legend and the filter label.
MRT_ICON_URI <- paste0(
  "data:image/svg+xml;utf8,",
  utils::URLencode(paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'>",
    "<rect x='1' y='1' width='22' height='22' rx='6' fill='#E2231A' stroke='#FFFFFF' stroke-width='1.5'/>",
    "<path d='M7 6.5h10a1.5 1.5 0 0 1 1.5 1.5v6.5a2 2 0 0 1-2 2h-9a2 2 0 0 1-2-2V8A1.5 1.5 0 0 1 7 6.5z' fill='#FFFFFF'/>",
    "<rect x='8' y='8.2' width='8' height='3.6' rx='0.8' fill='#E2231A'/>",
    "<circle cx='9.3' cy='14.3' r='1' fill='#E2231A'/><circle cx='14.7' cy='14.3' r='1' fill='#E2231A'/>",
    "<path d='M8.5 18.5l-1.5 2.2M15.5 18.5l1.5 2.2' stroke='#FFFFFF' stroke-width='1.6' stroke-linecap='round'/>",
    "</svg>"), reserved = TRUE))
mrt_marker_icon <- leaflet::makeIcon(MRT_ICON_URI, iconWidth = 17, iconHeight = 17)
mrt_icon_img <- function() tags$img(src = MRT_ICON_URI, class = "mrt-icon", alt = "MRT")

# -----------------------------------------------------------------------------
# V3 (Ridhwan, charts/V3_Ridhwan/V3.R): cuisine supply at one centre, in the database's
# nine main-meal groups (drinks, desserts and snacks are not counted).
# -----------------------------------------------------------------------------
V3_ORDER  <- CUISINE_GROUPS
# Stalls by cuisine group for every centre with Hawker Guide data (V3 search box picks one)
v3_supply_all <- raw$cuisine_mix |>
  mutate(cuisine_group = cuisine_category) |>
  group_by(centre_id, cuisine_group) |>
  summarise(stall_count = sum(observed_stalls), .groups = "drop")
V3_IDS <- intersect(centres$centre_id, v3_supply_all$centre_id[v3_supply_all$stall_count > 0])
v3_supply_for <- function(id) {
  v3_supply_all |>
    filter(centre_id == id) |>
    select(cuisine_group, stall_count) |>
    tidyr::complete(cuisine_group = V3_ORDER, fill = list(stall_count = 0)) |>
    mutate(supply_share = stall_count / sum(stall_count)) |>
    arrange(desc(stall_count))
}
# What V3 says for a centre with no Hawker Guide stalls (c = one row of `centres`)
v3_missing_reason <- function(c) {
  st <- c$nea_status
  if (identical(st, "Under Construction")) {
    list(chart = "Under construction",
         lead = "This centre is still under construction, so there are no stalls to count yet.",
         body = "Cuisine supply comes from Hawker Guide observations of occupied stalls. A centre that has not opened has none, so nothing is drawn rather than showing a misleading zero. Check back once it opens.")
  } else if (!is.na(c$stalls) && c$stalls == 0) {
    list(chart = "No cooked-food stalls",
         lead = "This centre has no cooked-food stalls on record, so there is no cuisine mix to show.",
         body = "NEA lists no cooked-food stalls here (it is a market or other food site), and Hawker Guide has no stall observations for it.")
  } else {
    list(chart = "Not yet observed",
         lead = "Hawker Guide has no stall observations for this centre yet, so its cuisine mix is unknown.",
         body = paste0("The centre is listed by NEA as ", tolower(st), " with ", c$stalls,
                       " cooked-food stalls, but no occupied stalls have been recorded and classified, so nothing is drawn rather than showing a misleading zero."))
  }
}
V3_SEARCH_JSON <- jsonlite::toJSON(
  centres |>
    filter(centre_id %in% V3_IDS) |>
    transmute(id = centre_id, name = centre_name, pa = tools::toTitleCase(tolower(planning_area_name))) |>
    arrange(name)
)

# -----------------------------------------------------------------------------
# V4 and V7 (Ridhwan, charts/V4_V7_Ridhwan/): Google Maps review engagement by
# planning area and cuisine, set against hawker supply and rent.
# engagement_pct = a cuisine's share of classified Google Maps reviews in the area;
# gap_pts = engagement_pct - the cuisine's share of occupied main-meal stalls at a centre.
# -----------------------------------------------------------------------------
V47_MIN_ESTABLISHMENTS <- 30   # areas with fewer classified places are left grey / out
# From the database table: share (0-1) -> %, and classified places = sum of the area's group counts
engagement <- raw$engagement |>
  mutate(across(c(restaurant_count, total_reviews, avg_reviews, engagement_share), as.numeric),
         engagement_pct = round(100 * engagement_share, 2)) |>
  group_by(planning_area_name) |>
  mutate(classified_establishments = sum(restaurant_count)) |>
  ungroup()

# Share of Google Maps reviews by planning area for one cuisine ("All" = each area's leading
# cuisine). Used by the map shading and by V4 so the two always agree. Areas with fewer than
# V47_MIN_ESTABLISHMENTS classified places get NA.
review_share_by_area <- function(cuisine) {
  ok <- engagement[engagement$classified_establishments >= V47_MIN_ESTABLISHMENTS, ]
  if (identical(cuisine, "All")) {
    top <- ok[order(ok$planning_area_name, -ok$engagement_pct), ]
    top <- top[!duplicated(top$planning_area_name), ]
    data.frame(planning_area_name = top$planning_area_name, reviews = top$engagement_pct,
               lead_cuisine = top$cuisine_group, restaurant_count = top$restaurant_count,
               total_reviews = top$total_reviews, stringsAsFactors = FALSE)
  } else {
    e <- ok[ok$cuisine_group == cuisine, ]
    data.frame(planning_area_name = e$planning_area_name, reviews = e$engagement_pct,
               lead_cuisine = cuisine, restaurant_count = e$restaurant_count,
               total_reviews = e$total_reviews, stringsAsFactors = FALSE)
  }
}
# Bins and labels of the shading (map and V4): darker = bigger share
REVIEW_SCALE <- list(
  one = list(bins = c(0, 10, 20, 30, 40, Inf), labels = c("Under 10%", "10–19%", "20–29%", "30–39%", "40% or more")),
  lead = list(bins = c(0, 30, 40, 50, 60, Inf), labels = c("Under 30%", "30–39%", "40–49%", "50–59%", "60% or more")))

centre_gap <- local({
  raw$cuisine_mix |>
    mutate(cuisine_group = cuisine_category) |>
    group_by(centre_id, cuisine_group) |>
    summarise(stalls = sum(observed_stalls), .groups = "drop") |>
    tidyr::complete(centre_id, cuisine_group = V3_ORDER, fill = list(stalls = 0)) |>
    group_by(centre_id) |>
    mutate(supply_pct = 100 * stalls / sum(stalls)) |>
    ungroup() |>
    left_join(select(centres, centre_id, centre_name, planning_area_name, median_amr), by = "centre_id") |>
    left_join(select(engagement, planning_area_name, cuisine_group, engagement_pct,
                     classified_establishments), by = c("planning_area_name", "cuisine_group")) |>
    mutate(gap_pts = engagement_pct - supply_pct,
           short_name = sub(" \\(.*$", "", centre_name)) |>
    filter(!is.na(gap_pts), !is.na(median_amr), classified_establishments >= V47_MIN_ESTABLISHMENTS)
})

# -----------------------------------------------------------------------------
# V6 (Ridhwan, charts/V4_V7_Ridhwan/): for one centre, each cuisine's share of Google Maps
# reviews in its planning area minus its share of occupied main-meal stalls (pts).
# Centres are listed only if their area has enough classified Google Maps places.
# -----------------------------------------------------------------------------
v6_gap_for <- function(id) {
  area <- centres$planning_area_name[centres$centre_id == id][1]
  eng <- engagement |>
    filter(planning_area_name == area) |>
    select(cuisine_group, engagement_pct, classified_establishments)
  v3_supply_for(id) |>
    mutate(supply_pct = 100 * supply_share) |>
    left_join(eng, by = "cuisine_group") |>
    mutate(engagement_pct = coalesce(engagement_pct, 0),
           gap_pts = engagement_pct - supply_pct) |>
    filter(supply_pct > 0 | engagement_pct > 0) |>   # as in V6.R: cuisines absent from both are left out
    arrange(gap_pts)
}
V6_IDS <- local({
  ok_areas <- engagement$planning_area_name[engagement$classified_establishments >= V47_MIN_ESTABLISHMENTS]
  V3_IDS[centres$planning_area_name[match(V3_IDS, centres$centre_id)] %in% ok_areas]
})
V6_SEARCH_JSON <- jsonlite::toJSON(
  centres |>
    filter(centre_id %in% V6_IDS) |>
    transmute(id = centre_id, name = centre_name, pa = tools::toTitleCase(tolower(planning_area_name))) |>
    arrange(name)
)

# -----------------------------------------------------------------------------
# V9 (Casper, charts/V9_Casper/CasperCode.R): where stalls get taken. A stall that was
# advertised once and never came back was most likely taken (the notices show
# advertisements, not awards). Stalls first advertised in the last V9_SETTLE_MONTHS
# months are left out: they have had no chance to return. Centres need V9_MIN_STALLS
# assessed stalls. Same logic as competition_summary() in the script.
# -----------------------------------------------------------------------------
V9_SETTLE_MONTHS <- 4
V9_MIN_STALLS    <- 3
month_index <- function(d) as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))
v9_all <- local({
  last_m <- max(month_index(tenders$exercise_month))
  tenders |>
    filter(!is.na(stall_id), !is.na(exercise_month)) |>
    group_by(centre_id, centre_name, stall_id) |>
    summarise(n_listings = n(), first_m = min(month_index(exercise_month)), .groups = "drop") |>
    filter(first_m <= last_m - V9_SETTLE_MONTHS) |>
    group_by(centre_id, centre_name) |>
    summarise(stalls_assessed = n(), taken_first = sum(n_listings == 1L), .groups = "drop") |>
    filter(stalls_assessed >= V9_MIN_STALLS) |>
    mutate(share_taken = taken_first / stalls_assessed) |>
    arrange(desc(share_taken), desc(stalls_assessed))
})
# Benchmarks for the gauge: the median of the assessed centres (not the pooled rate, which lets
# big centres dominate) and its bottom and top quarter, rounded to whole percent so the bands match
# what the page says (below 33% = worse than most, 70% and above = better than most)
V9_MEDIAN <- median(v9_all$share_taken)
V9_Q1 <- round(unname(quantile(v9_all$share_taken, 0.25)), 2)
V9_Q3 <- round(unname(quantile(v9_all$share_taken, 0.75)), 2)

# Why a centre can or cannot be assessed (every centre): none = no stall advertised,
# few = only 1-2 stalls, late = stalls advertised too recently, ok = assessed
v9_status <- local({
  last_m <- max(month_index(tenders$exercise_month))
  per <- tenders |>
    filter(!is.na(stall_id), !is.na(exercise_month)) |>
    group_by(centre_id, stall_id) |>
    summarise(first_m = min(month_index(exercise_month)), .groups = "drop") |>
    group_by(centre_id) |>
    summarise(n_stalls = n(), n_assessed = sum(first_m <= last_m - V9_SETTLE_MONTHS), .groups = "drop")
  centres |>
    select(centre_id) |>
    left_join(per, by = "centre_id") |>
    left_join(select(v9_all, centre_id, taken_first, stalls_assessed, share_taken), by = "centre_id") |>
    mutate(n_stalls = coalesce(n_stalls, 0L), n_assessed = coalesce(n_assessed, 0L),
           status = case_when(n_stalls == 0 ~ "none",
                              n_stalls <= 2 ~ "few",
                              n_assessed < V9_MIN_STALLS ~ "late",
                              TRUE ~ "ok"))
})

# -----------------------------------------------------------------------------
# V8 (Lucien): MRT access and market potential, from the database (SQL_MRT_ACCESS; the
# same numbers as his hawker_data_2026.csv). Centres need a matched station with ridership
# and a subzone population.
# -----------------------------------------------------------------------------
v8 <- local({
  d <- raw$mrt_access
  for (col in c("straight_line_distance_m", "avg_daily_tap_outs", "subzone_population_2026"))
    d[[col]] <- as.numeric(d[[col]])
  d <- subset(d, match_status == "MATCHED" & is.finite(straight_line_distance_m) &
                 is.finite(avg_daily_tap_outs) & is.finite(subzone_population_2026) &
                 straight_line_distance_m >= 0 & avg_daily_tap_outs >= 0 & subzone_population_2026 > 0)
  d$station <- tools::toTitleCase(tolower(sub(" (MRT|LRT) STATION$", "", d$nearest_station_name)))
  d
})

# Choices for inputs
REGIONS   <- sort(unique(na.omit(centres$region_name)))
PAS       <- sort(unique(na.omit(centres$planning_area_name)))
AMR_RANGE <- c(floor(min(centres$median_amr, na.rm = TRUE) / 50) * 50,
               ceiling(max(centres$median_amr, na.rm = TRUE) / 50) * 50)
MRT_MAX   <- ceiling(max(centres$nearest_exit_distance_m, na.rm = TRUE) / 100) * 100
MONTHS    <- range(tenders$exercise_month)

named_choices <- function(x) setNames(x, tools::toTitleCase(tolower(x)))

# Max of the "Min. vacant stalls" map filter
VAC_MAX   <- max(c(1, centres$vacant_units), na.rm = TRUE)

# Centre shown in the centre card when the page opens
DEFAULT_CENTRE <- centres$centre_id[grepl("^Buffalo Road Blk 665", centres$centre_name)][1]
if (is.na(DEFAULT_CENTRE)) DEFAULT_CENTRE <- centres$centre_id[1]

# JSON for the map search box (name, id, planning area)
CENTRES_SEARCH_JSON <- jsonlite::toJSON(
  centres |>
    transmute(id = centre_id, name = centre_name, pa = pa_title) |>
    arrange(name)
)

# Centre the V3 and V6 charts open on: the centre card's default when it has the data.
# After that, picking a centre on the map moves both charts to it (server.R).
V3_DEFAULT_ID <- if (DEFAULT_CENTRE %in% V3_IDS) DEFAULT_CENTRE else V3_IDS[1]
V6_DEFAULT_ID <- if (DEFAULT_CENTRE %in% V6_IDS) DEFAULT_CENTRE else V3_DEFAULT_ID
