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
# TRUE  = read from the MySQL database (import database/SA1_Team03_Project_hawker_data_v2-1.sql first)
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

# Optional synthetic survey (rids). Expected columns:
#   planning_area, age_group, cuisine_group, n_people
# planning_area must match planning_area_name (e.g. "WOODLANDS");
# cuisine_group must use the groups in group_cuisine() below.
SURVEY_FILE <- file.path("data", "cuisine_survey.csv")
survey <- if (file.exists(SURVEY_FILE)) {
  read.csv(SURVEY_FILE, stringsAsFactors = FALSE) |>
    mutate(planning_area = toupper(trimws(planning_area)))
} else {
  NULL
}
HAS_SURVEY <- !is.null(survey) && nrow(survey) > 0

# -----------------------------------------------------------------------------
# 2. Helper functions
# -----------------------------------------------------------------------------

# 35 scraped cuisine labels -> 11 groups (the survey must use the same groups)
group_cuisine <- function(x) {
  case_when(
    x %in% c("Chinese", "Cantonese", "Teochew", "Hokkien", "Hakka", "Hainanese",
             "Taiwanese", "Chinese Muslim")                       ~ "Chinese",
    x %in% c("Malay", "Indonesian")                               ~ "Malay & Indonesian",
    x %in% c("Indian", "Indian Muslim", "Pakistani", "Sri Lankan") ~ "Indian",
    x %in% c("Western", "Italian", "Mexican", "Mediterranean", "BBQ") ~ "Western",
    x %in% c("Japanese", "Korean")                                ~ "Japanese & Korean",
    x %in% c("Thai", "Vietnamese")                                ~ "Thai & Vietnamese",
    x %in% c("Turkish", "Middle Eastern", "Moroccan")             ~ "Middle Eastern",
    x %in% c("Peranakan", "Singaporean", "Fusion", "Seafood")     ~ "Local & Other",
    x %in% c("Dessert", "Bakery", "Snacks")                       ~ "Desserts & Snacks",
    x == "Beverages"                                              ~ "Beverages",
    x == "Vegetarian"                                             ~ "Vegetarian",
    TRUE                                                          ~ "Local & Other"
  )
}
CUISINE_GROUPS <- c("Chinese", "Malay & Indonesian", "Indian", "Western",
                    "Japanese & Korean", "Thai & Vietnamese", "Middle Eastern",
                    "Local & Other", "Desserts & Snacks", "Beverages", "Vegetarian")

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
# Sequential (low -> high), used for maps
BRAND_SEQ <- c("#F6F8F9", "#DDECC1", "#BBD980", "#80C683", "#40AD49", "#0D743B")

# One fixed colour per cuisine group, used by every Tab 2 chart (brand-based)
CUISINE_COLOURS <- c(
  "Chinese" = "#0D743B", "Malay & Indonesian" = "#40AD49", "Indian" = "#FAA619",
  "Western" = "#4C83C2", "Japanese & Korean" = "#99CA3C", "Thai & Vietnamese" = "#4E5D68",
  "Middle Eastern" = "#C27C0E", "Local & Other" = "#80C683", "Desserts & Snacks" = "#F6C76B",
  "Beverages" = "#9DB7DD", "Vegetarian" = "#BAC4CA"
)

fmt_sgd <- function(x) ifelse(is.na(x), "n/a", paste0("S$", comma(round(x))))
fmt_num <- function(x) ifelse(is.na(x), "n/a", comma(round(x)))
plural  <- function(n, word) paste(comma(n), if (n == 1) word else paste0(word, "s"))

# McKinsey-style look for every plotly chart (V3-V10 and the centre card): no
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

empty_plot <- function(msg) {
  plotly_empty(type = "scatter", mode = "markers") |>
    layout(annotations = list(text = msg, showarrow = FALSE,
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
    exercise_month    = as.Date(exercise_month),
    tender_close_date = as.Date(tender_close_date),
    across(c(approximate_area_m2, median_centre_amr_sgd, cleaning_fee_min_sgd,
             cleaning_fee_max_sgd, scc_min_sgd, scc_max_sgd), as.numeric)
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
  mutate(cuisine_group = group_cuisine(cuisine_category)) |>
  group_by(centre_id, cuisine_group) |>
  summarise(stalls = sum(observed_stalls), .groups = "drop") |>
  group_by(centre_id) |>
  mutate(share = stalls / sum(stalls)) |>
  ungroup()

# Vacant units per centre (NULL if the vacancy file is missing)
vacancy <- if (is.null(raw$vacancy)) {
  data.frame(centre_id = numeric(0), vacant_units = numeric(0), renovation_units = numeric(0),
             occupied_units = numeric(0), observed_units = numeric(0))
} else {
  raw$vacancy |> mutate(across(everything(), as.numeric))
}

# Centres: one row per hawker centre with every indicator the app needs
centres <- raw$centres |>
  mutate(across(c(latitude, longitude, stalls, resident_population, hdb_population,
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

# Main cuisine and top 3 cuisines per centre (Explore map tooltip and colours)
centre_mix <- cuisine_by_centre |>
  group_by(centre_id) |>
  arrange(desc(share), .by_group = TRUE) |>
  summarise(main_cuisine = first(cuisine_group),
            top3 = paste0(head(cuisine_group, 3), " ", percent(head(share, 3), 1), collapse = " · "),
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

cuisine_by_centre <- cuisine_by_centre |>
  left_join(select(centres, centre_id, centre_name, planning_area_name), by = "centre_id")

# Planning areas: polygons + population + land area
pa_population <- raw$pa_population |>
  mutate(across(c(resident_population, hdb_population), as.numeric))

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
    fitBounds(103.60, 1.205, 104.05, 1.475,
              options = list(paddingTopLeft = c(10, 10), paddingBottomRight = c(pad_right, 10))) |>
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
# V3 (Ridhwan, charts/V3_Ridhwan/V3.R): cuisine supply at one centre. Same nine
# groups as the script; labels outside them (Dessert, Beverages) are left out.
# -----------------------------------------------------------------------------
V3_CENTRE <- "Maxwell"
V3_ORDER  <- c("Chinese", "Malay / Indonesian", "Indian / South Asian", "Western / European",
               "Japanese", "Korean", "Thai / Vietnamese", "Middle Eastern / Mediterranean",
               "Other / Mixed")
group_cuisine_v3 <- function(x) {
  case_when(
    x %in% c("Chinese", "Chinese Muslim", "Cantonese", "Teochew", "Hokkien", "Taiwanese",
             "Hakka", "Hainanese")                                  ~ "Chinese",
    x %in% c("Malay", "Indonesian")                                 ~ "Malay / Indonesian",
    x %in% c("Indian", "Indian Muslim", "Pakistani", "Sri Lankan")  ~ "Indian / South Asian",
    x %in% c("Western", "Italian")                                  ~ "Western / European",
    x == "Japanese"                                                 ~ "Japanese",
    x == "Korean"                                                   ~ "Korean",
    x %in% c("Thai", "Vietnamese")                                  ~ "Thai / Vietnamese",
    x %in% c("Turkish", "Middle Eastern", "Moroccan", "Mediterranean") ~ "Middle Eastern / Mediterranean",
    x %in% c("Vegetarian", "Peranakan", "Seafood", "Singaporean", "Fusion", "Mexican", "BBQ") ~ "Other / Mixed",
    TRUE                                                            ~ NA_character_
  )
}
# Stalls by cuisine group for every centre with Hawker Guide data (V3 search box picks one)
v3_supply_all <- raw$cuisine_mix |>
  mutate(cuisine_group = group_cuisine_v3(cuisine_category)) |>
  filter(!is.na(cuisine_group)) |>
  group_by(centre_id, cuisine_group) |>
  summarise(stall_count = sum(observed_stalls), .groups = "drop")
V3_IDS <- intersect(centres$centre_id, v3_supply_all$centre_id[v3_supply_all$stall_count > 0])
v3_centre <- centres[centres$centre_id %in% V3_IDS & grepl(V3_CENTRE, centres$centre_name, ignore.case = TRUE), ][1, ]
v3_supply_for <- function(id) {
  v3_supply_all |>
    filter(centre_id == id) |>
    select(cuisine_group, stall_count) |>
    tidyr::complete(cuisine_group = V3_ORDER, fill = list(stall_count = 0)) |>
    mutate(supply_share = stall_count / sum(stall_count)) |>
    arrange(desc(stall_count))
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
ENGAGEMENT_FILE <- file.path("data", "google_maps_cuisine_engagement_by_planning_area_v3.csv")
engagement <- if (file.exists(ENGAGEMENT_FILE)) read.csv(ENGAGEMENT_FILE, stringsAsFactors = FALSE) else NULL
V47_MIN_ESTABLISHMENTS <- 30   # areas with fewer classified places are left grey / out

centre_gap <- if (is.null(engagement)) NULL else {
  raw$cuisine_mix |>
    mutate(cuisine_group = group_cuisine_v3(cuisine_category)) |>
    filter(!is.na(cuisine_group)) |>
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
}

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
v9_short_name <- function(x, max_chars = 42L) {
  inner <- sub("^[^(]*\\(([^)]*)\\).*$", "\\1", x)
  out   <- ifelse(grepl("\\(", x) & nchar(inner) > 0 & nchar(inner) < nchar(x), inner, x)
  ifelse(nchar(out) > max_chars, paste0(substr(out, 1, max_chars - 1L), "\u2026"), out)
}
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
    mutate(share_taken = taken_first / stalls_assessed,
           short_name = v9_short_name(centre_name)) |>
    arrange(desc(share_taken), desc(stalls_assessed))
})
V9_OVERALL <- sum(v9_all$taken_first) / sum(v9_all$stalls_assessed)

# -----------------------------------------------------------------------------
# V8 (Lucien, charts/V8_Lucien/V9_2026_graph.R): MRT access and market potential.
# Reads data/hawker_data_2026.csv (SQL export, V9_2026_EXTRACTION.sql). Equal-weight
# score: closer exit, busier station, more subzone residents, each min-max scaled.
# -----------------------------------------------------------------------------
v8_scale100 <- function(x) if (max(x) == min(x)) rep(50, length(x)) else 100 * (x - min(x)) / (max(x) - min(x))
v8 <- local({
  f <- file.path("data", "hawker_data_2026.csv")
  if (!file.exists(f)) return(NULL)
  d <- read.csv(f, stringsAsFactors = FALSE)
  for (col in c("straight_line_distance_m", "avg_daily_tap_outs", "subzone_population_2026"))
    d[[col]] <- as.numeric(d[[col]])
  d <- subset(d, match_status == "MATCHED" & is.finite(straight_line_distance_m) &
                 is.finite(avg_daily_tap_outs) & is.finite(subzone_population_2026) &
                 straight_line_distance_m >= 0 & avg_daily_tap_outs >= 0 & subzone_population_2026 > 0)
  d$distance_score   <- 100 - v8_scale100(d$straight_line_distance_m)
  d$activity_score   <- v8_scale100(d$avg_daily_tap_outs)
  d$population_score <- v8_scale100(d$subzone_population_2026)
  d$combined_score   <- (d$distance_score + d$activity_score + d$population_score) / 3
  d <- d[order(-d$combined_score, d$straight_line_distance_m, d$centre_name), ]
  d$overall_rank <- seq_len(nrow(d))
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

# Max of the "Min. vacant stalls" filter (Explore tab)
VAC_MAX   <- max(c(1, centres$vacant_units), na.rm = TRUE)

# Centre shown in the centre card when the page opens
DEFAULT_CENTRE <- centres$centre_id[centres$centre_name == "Ang Mo Kio Ave 4 Blk 628 (Ang Mo Kio 628 Market)"][1]
if (is.na(DEFAULT_CENTRE)) DEFAULT_CENTRE <- centres$centre_id[1]

# JSON for the map search box (name, id, planning area)
CENTRES_SEARCH_JSON <- jsonlite::toJSON(
  centres |>
    transmute(id = centre_id, name = centre_name, pa = pa_title) |>
    arrange(name)
)
