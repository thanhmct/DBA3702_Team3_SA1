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

sf_use_s2(FALSE)

# -----------------------------------------------------------------------------
# 1. Data source settings
# -----------------------------------------------------------------------------
# TRUE  = read from the MySQL database (import SA1_Team03_Project_hawker_data_v2.sql first)
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
DATA_SOURCE <- raw$source

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

LAND_USE_COLOURS <- c(
  "Residential" = "#0D743B", "Commercial" = "#FAA619",
  "Business & industry" = "#4E5D68", "Education & civic" = "#4C83C2",
  "Green & open space" = "#99CA3C", "Roads & transport" = "#BAC4CA",
  "Other" = "#EDF0F2"
)

RELEASE_LABELS <- c(
  first_release               = "First release",
  second_or_subsequent        = "Second or later release",
  previously_released_no_bids = "Released before, no bids"
)
RELEASE_COLOURS <- c("#0D743B", "#99CA3C", "#FAA619")

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
rq_header <- function(code, title, ...) {
  card_header(class = "d-flex flex-wrap gap-2 align-items-center",
              tags$span(class = "badge bg-primary", code), tags$strong(title),
              div(class = "ms-auto d-flex gap-2 align-items-center", ...))
}
# Small inline control for a card header
hdr_input <- function(x) div(class = "mb-0 small", style = "min-width: 180px;", x)
# One "Reset filters" button per tab sidebar
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
    release_label     = factor(RELEASE_LABELS[release_category], levels = RELEASE_LABELS),
    across(c(approximate_area_m2, median_centre_amr_sgd, cleaning_fee_min_sgd,
             cleaning_fee_max_sgd, scc_min_sgd, scc_max_sgd), as.numeric),
    cleaning_mid = rowMeans(cbind(cleaning_fee_min_sgd, cleaning_fee_max_sgd), na.rm = TRUE),
    scc_mid      = rowMeans(cbind(scc_min_sgd, scc_max_sgd), na.rm = TRUE),
    cleaning_mid = ifelse(is.nan(cleaning_mid), NA, cleaning_mid),
    scc_mid      = ifelse(is.nan(scc_mid), NA, scc_mid),
    est_monthly_cost = median_centre_amr_sgd + coalesce(cleaning_mid, 0) + coalesce(scc_mid, 0),
    needs_review = as.integer(needs_review) == 1
  )

tender_by_centre <- tenders |>
  group_by(centre_id) |>
  summarise(
    n_listings      = n(),
    n_tender_stalls = n_distinct(stall_id),
    n_retender      = sum(release_category != "first_release"),
    median_amr      = median(median_centre_amr_sgd, na.rm = TRUE),
    min_est_cost    = min(est_monthly_cost, na.rm = TRUE),
    last_month      = max(exercise_month),
    .groups = "drop"
  ) |>
  mutate(min_est_cost = ifelse(is.finite(min_est_cost), min_est_cost, NA))

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
         nearest_mrt_station = tools::toTitleCase(tolower(nearest_mrt_station))) |>
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

# Base map used by every leaflet map in the app
# (zoom 11.5 fills the map card at the 1920x1080 target screen)
base_map <- function(zoom = 11.5) {
  leaflet(options = leafletOptions(minZoom = 10, zoomSnap = 0.25)) |>
    # CARTO basemaps now need an API key; Esri's light grey canvas does not
    addProviderTiles(providers$Esri.WorldGrayCanvas) |>
    addMapPane("sg_border", zIndex = 420) |>
    setView(lng = 103.82, lat = 1.35, zoom = zoom) |>
    add_sg_border()
}

mrt_exits <- raw$mrt_exits |>
  mutate(across(c(latitude, longitude), as.numeric))

# Choices for inputs
REGIONS   <- sort(unique(na.omit(centres$region_name)))
PAS       <- sort(unique(na.omit(centres$planning_area_name)))
AMR_RANGE <- c(floor(min(centres$median_amr, na.rm = TRUE) / 50) * 50,
               ceiling(max(centres$median_amr, na.rm = TRUE) / 50) * 50)
MRT_MAX   <- ceiling(max(centres$nearest_exit_distance_m, na.rm = TRUE) / 100) * 100
MONTHS    <- range(tenders$exercise_month)
AREA_MAX  <- ceiling(max(tenders$approximate_area_m2, na.rm = TRUE))
AGE_GROUPS <- if (HAS_SURVEY) sort(unique(survey$age_group)) else character(0)

named_choices <- function(x) setNames(x, tools::toTitleCase(tolower(x)))

# Max of the "Min. vacant stalls" filter (Explore tab)
VAC_MAX   <- max(c(1, centres$vacant_units), na.rm = TRUE)
