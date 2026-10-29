# =============================================================================
# DBA3702 Team 03 - Hawker Site Finder
# The team's original chart scripts, in one file (reference only: the app does
# not source this file; its live versions of the charts are in global.R and
# server.R). Each section is a stand-alone script: run one section at a time,
# from its banner to the next.
#
#   1. V3      Cuisine supply at one centre .............. Ridhwan
#   2. V4-V7   Cuisine engagement vs supply .............. Ridhwan
#   3. V8      MRT exit distance vs station activity ..... Lucien
#   4. V9      Stall relisting / tenancy risk ............ Casper
# =============================================================================


# #############################################################################
# 1. V3 - Cuisine supply at one centre (Ridhwan)
#    (was charts/V3_Ridhwan/V3.R)
# #############################################################################


# ============================================================
# DBA3702 Team 03 - Hawker Cuisine Opportunity Finder
# V3: Cuisine Supply Breakdown (Ranked Horizontal Bar Chart)
# ============================================================

# 1. LOAD PACKAGES
library(DBI)
library(RMariaDB)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Resolve files relative to this script, whether run with Rscript or in RStudio.
script_path <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_path)) sub("^--file=", "", script_path[1]) else ""
if (!nzchar(script_path) && requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
}
project_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()

# ============================================================
# 2. CONNECT TO MYSQL
# ============================================================

db_password <- Sys.getenv("HAWKER_DB_PASSWORD")
if (!nzchar(db_password)) {
  if (!interactive()) stop("Set HAWKER_DB_PASSWORD before running non-interactively.")
  db_password <- rstudioapi::askForPassword("MySQL Password")
}
con <- dbConnect(
  MariaDB(),
  host = Sys.getenv("HAWKER_DB_HOST", "127.0.0.1"),
  port = as.integer(Sys.getenv("HAWKER_DB_PORT", "3306")),
  user = Sys.getenv("HAWKER_DB_USER", "root"),
  password = db_password,
  dbname = Sys.getenv("HAWKER_DB_NAME", "hawker_project")
)

# ============================================================
# 3. EXTRACT CUISINE SUPPLY
# ============================================================

hawker_data <- dbGetQuery(con, "
  SELECT
    c.centre_id,
    c.centre_name,
    v.cuisine_category,
    v.observed_stalls
  FROM v_cuisine_mix AS v
  JOIN hawker_centres AS c
    ON v.centre_id = c.centre_id
")

dbDisconnect(con)

# ============================================================
# 4. STANDARDISE CUISINE CATEGORIES
# ============================================================

cuisine_order <- c(
  "Chinese",
  "Malay / Indonesian",
  "Indian / South Asian",
  "Western / European",
  "Japanese",
  "Korean",
  "Thai / Vietnamese",
  "Middle Eastern / Mediterranean",
  "Other / Mixed"
)

cuisine_supply <- hawker_data |>
  mutate(
    cuisine_group = case_when(
      
      cuisine_category %in% c(
        "Chinese", "Chinese Muslim", "Cantonese",
        "Teochew", "Hokkien", "Taiwanese",
        "Hakka", "Hainanese"
      ) ~ "Chinese",
      
      cuisine_category %in% c(
        "Malay", "Indonesian"
      ) ~ "Malay / Indonesian",
      
      cuisine_category %in% c(
        "Indian", "Indian Muslim",
        "Pakistani", "Sri Lankan"
      ) ~ "Indian / South Asian",
      
      cuisine_category %in% c(
        "Western", "Italian"
      ) ~ "Western / European",
      
      cuisine_category == "Japanese" ~ "Japanese",
      
      cuisine_category == "Korean" ~ "Korean",
      
      cuisine_category %in% c(
        "Thai", "Vietnamese"
      ) ~ "Thai / Vietnamese",
      
      cuisine_category %in% c(
        "Turkish", "Middle Eastern",
        "Moroccan", "Mediterranean"
      ) ~ "Middle Eastern / Mediterranean",
      
      cuisine_category %in% c(
        "Vegetarian", "Peranakan", "Seafood",
        "Singaporean", "Fusion", "Mexican", "BBQ"
      ) ~ "Other / Mixed",
      
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(cuisine_group)) |>
  group_by(centre_id, centre_name, cuisine_group) |>
  summarise(
    stall_count = sum(observed_stalls),
    .groups = "drop"
  ) |>
  group_by(centre_id, centre_name) |>
  mutate(
    supply_share = stall_count / sum(stall_count)
  ) |>
  ungroup()

# ============================================================
# 5. SELECT HAWKER CENTRE
# ============================================================

selected_centre <- "Maxwell"

plot_data <- cuisine_supply |>
  filter(grepl(selected_centre, centre_name, ignore.case = TRUE))

# Prevent accidental aggregation of multiple centres
if (n_distinct(plot_data$centre_id) != 1) {
  stop("Centre selection must match exactly one hawker centre.")
}

centre_name <- unique(plot_data$centre_name)

plot_data <- plot_data |>
  select(cuisine_group, stall_count) |>
  complete(
    cuisine_group = cuisine_order,
    fill = list(stall_count = 0)
  ) |>
  mutate(
    supply_share = stall_count / sum(stall_count),
    cuisine_group = reorder(cuisine_group, stall_count),
    stall_label = if_else(
      stall_count == 0,
      "No occupied stalls",
      paste0(
        stall_count, " (",
        percent(supply_share, accuracy = 0.1),
        ")"
      )
    )
  )

total_stalls <- sum(plot_data$stall_count)

# ============================================================
# 6. VISUALISATION
# ============================================================

p <- ggplot(
  plot_data,
  aes(
    x = stall_count,
    y = cuisine_group
  )
) +
  geom_col(
    fill = "#2A78D6",
    width = 0.65
  ) +
  geom_text(
    aes(label = stall_label),
    hjust = -0.15,
    size = 3.5,
    colour = "#333333"
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.30))
  ) +
  labs(
    title = paste("Cuisine Supply at", centre_name),
    subtitle = paste(
      total_stalls,
      "occupied main-meal stalls"
    ),
    x = "Number of occupied stalls",
    y = NULL,
    caption = "Source: Hawker Guide | Occupied main-meal stalls only"
  ) +
  theme_minimal(
    base_size = 11,
    base_family = "sans"
  ) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 10,
      colour = "#666666"
    ),
    axis.text.y = element_text(
      size = 10,
      colour = "#333333"
    ),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      size = 8,
      colour = "#888888"
    ),
    plot.margin = margin(15, 25, 15, 15)
  )

print(p)
ggsave(file.path(project_dir, "V3_Cuisine_Supply_Breakdown.png"),
       p, width = 10, height = 6, dpi = 300)


# #############################################################################
# 2. V4-V7 - Cuisine engagement vs supply (Ridhwan)
#    (was charts/V4_V7_Ridhwan/V4_V7_Cuisine_Engagement_Charts.R)
# #############################################################################

# ============================================================
# DBA3702 Team 03 - Hawker Cuisine Opportunity Finder
# V4-V7: cuisine engagement (Google Maps reviews) vs hawker supply
#   V4  Cuisine demand heatmap (planning area x cuisine) + map for one cuisine
#   V5  Age-grouped bar chart for one planning area
#   V6  Diverging bar: engagement share minus supply share at one centre
#   V7  Scatter: engagement-supply gap vs rent, for one cuisine
#
# Reads the CSVs in the project's data/ folder (no database needed).
# Saves the PNGs in the output/ folder beside this script.
# Change the four settings in section 1 to look at another cuisine/area/centre.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
HAS_SF <- requireNamespace("sf", quietly = TRUE)

# Resolve files relative to this script, whether run with Rscript or in RStudio.
script_path <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_path)) sub("^--file=", "", script_path[1]) else ""
if (!nzchar(script_path) && requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
}
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()
data_dir   <- file.path(script_dir, "..", "..", "data")
out_dir    <- file.path(script_dir, "output")
dir.create(out_dir, showWarnings = FALSE)

# ============================================================
# 1. SETTINGS - change these to explore
# ============================================================

V4_CUISINE <- "Western / European"   # cuisine shown on the V4 map and V7 scatter
V5_AREA    <- "BEDOK"                # planning area for V5 (UPPER CASE, as in the data)
V6_CENTRE  <- "Maxwell"              # hawker centre for V6 (part of its name)
V7_CUISINE <- V4_CUISINE
MIN_ESTABLISHMENTS <- 100            # areas below this are marked * (thin Google Maps coverage)

cuisine_order <- c(
  "Chinese", "Malay / Indonesian", "Indian / South Asian", "Western / European",
  "Japanese", "Korean", "Thai / Vietnamese", "Middle Eastern / Mediterranean",
  "Other / Mixed"
)

GREEN_DARK <- "#0D743B"; GREEN <- "#40AD49"; ORANGE <- "#FAA619"; GREY <- "#BAC4CA"
SEQ <- c("#F6F8F9", "#DDECC1", "#BBD980", "#80C683", "#40AD49", "#0D743B")

theme_hawker <- function() {
  theme_minimal(base_size = 11, base_family = "sans") +
    theme(
      plot.title = element_text(size = 15, face = "bold", colour = "#1E2B22"),
      plot.subtitle = element_text(size = 10, colour = "#666666"),
      plot.caption = element_text(size = 8, colour = "#888888", hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      panel.grid.minor = element_blank(),
      plot.margin = margin(15, 25, 15, 15)
    )
}
wrap <- function(x, w = 110) paste(strwrap(x, w), collapse = "\n")
title_case <- function(x) tools::toTitleCase(tolower(x))
save_plot <- function(p, name, w, h) {
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = 200, bg = "white")
  message("saved ", name)
}

# ============================================================
# 2. LOAD DATA
# ============================================================

engagement <- read.csv(file.path(data_dir, "google_maps_cuisine_engagement_by_planning_area_v3.csv"),
                       stringsAsFactors = FALSE)
centres    <- read.csv(file.path(data_dir, "centres.csv"), stringsAsFactors = FALSE)
cuisine_mix <- read.csv(file.path(data_dir, "cuisine_mix.csv"), stringsAsFactors = FALSE)
tenders    <- read.csv(file.path(data_dir, "tenders.csv"), stringsAsFactors = FALSE)
pa_age     <- read.csv(file.path(data_dir, "pa_age_population.csv"), stringsAsFactors = FALSE)

# Same nine groups as V3 (Hawker Guide labels -> cuisine group; beverages/desserts left out)
group_cuisine_v3 <- function(x) {
  case_when(
    x %in% c("Chinese", "Chinese Muslim", "Cantonese", "Teochew", "Hokkien", "Taiwanese",
             "Hakka", "Hainanese")                                     ~ "Chinese",
    x %in% c("Malay", "Indonesian")                                    ~ "Malay / Indonesian",
    x %in% c("Indian", "Indian Muslim", "Pakistani", "Sri Lankan")     ~ "Indian / South Asian",
    x %in% c("Western", "Italian")                                     ~ "Western / European",
    x == "Japanese"                                                    ~ "Japanese",
    x == "Korean"                                                      ~ "Korean",
    x %in% c("Thai", "Vietnamese")                                     ~ "Thai / Vietnamese",
    x %in% c("Turkish", "Middle Eastern", "Moroccan", "Mediterranean") ~ "Middle Eastern / Mediterranean",
    x %in% c("Vegetarian", "Peranakan", "Seafood", "Singaporean", "Fusion", "Mexican", "BBQ") ~ "Other / Mixed",
    TRUE ~ NA_character_
  )
}

# Supply: share of occupied main-meal stalls by cuisine group, one row per centre x cuisine
supply <- cuisine_mix |>
  mutate(cuisine_group = group_cuisine_v3(cuisine_category)) |>
  filter(!is.na(cuisine_group)) |>
  group_by(centre_id, cuisine_group) |>
  summarise(stalls = sum(observed_stalls), .groups = "drop") |>
  complete(centre_id, cuisine_group = cuisine_order, fill = list(stalls = 0)) |>
  group_by(centre_id) |>
  mutate(supply_pct = 100 * stalls / sum(stalls)) |>
  ungroup()

# Rent: median NEA reference rent (AMR) per centre, from the tender listings
rent <- tenders |>
  group_by(centre_id) |>
  summarise(median_amr = median(median_centre_amr_sgd, na.rm = TRUE), .groups = "drop")

# Gap = engagement share in the centre's planning area - supply share at the centre
# (percentage points). Positive = the cuisine gets more review attention than it has stalls.
centre_gap <- supply |>
  left_join(select(centres, centre_id, centre_name, planning_area_name), by = "centre_id") |>
  left_join(select(engagement, planning_area_name, cuisine_group, engagement_pct,
                   classified_establishments, classification_rate),
            by = c("planning_area_name", "cuisine_group")) |>
  mutate(gap_pts = engagement_pct - supply_pct) |>
  left_join(rent, by = "centre_id")

# ============================================================
# 3. V4 - CUISINE DEMAND HEATMAP
# ============================================================

area_with_centres <- unique(centres$planning_area_name)

v4_data <- engagement |>
  filter(planning_area_name %in% area_with_centres) |>
  mutate(label_area = paste0(title_case(planning_area_name),
                             ifelse(classified_establishments < MIN_ESTABLISHMENTS, " *", "")))
area_order <- v4_data |> group_by(label_area) |>
  summarise(r = sum(total_reviews)) |> arrange(r) |> pull(label_area)
cuisine_rank <- engagement |> group_by(cuisine_group) |>
  summarise(r = sum(total_reviews)) |> arrange(desc(r)) |> pull(cuisine_group)

v4_data <- v4_data |>
  mutate(label_area = factor(label_area, levels = area_order),
         cuisine_group = factor(cuisine_group, levels = cuisine_rank),
         txt = ifelse(engagement_pct < 0.5, "", sprintf("%.0f", engagement_pct)))

p4 <- ggplot(v4_data, aes(cuisine_group, label_area, fill = engagement_pct)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = txt, colour = engagement_pct > 30), size = 3) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#333333"), guide = "none") +
  scale_fill_gradientn(colours = SEQ, limits = c(0, 60), oob = squish, name = "Share of reviews, %") +
  scale_x_discrete(position = "top", labels = function(x) gsub(" / ", " /\n", x)) +
  labs(title = "Estimated cuisine demand by planning area",
       subtitle = wrap("Share of Google Maps reviews (%) by cuisine, planning areas with hawker centres. Rows ordered by total reviews", 100),
       x = NULL, y = NULL,
       caption = wrap(paste0("Source: Google Maps establishments classified into nine cuisine groups (v3; about 49% of establishments classified). ",
                        "* fewer than ", MIN_ESTABLISHMENTS, " classified establishments. Review share is an engagement indicator, not measured demand."), 120)) +
  theme_hawker() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(size = 9), legend.position = "bottom")
save_plot(p4, "V4_cuisine_engagement_heatmap.png", 11, 9)

# V4 map: planning areas shaded by the share of reviews for one cuisine
if (HAS_SF) {
  areas <- sf::st_read(file.path(data_dir, "planning_areas.geojson"), quiet = TRUE) |>
    rename(planning_area_name = planning_area)
  map_data <- areas |>
    left_join(filter(engagement, cuisine_group == V4_CUISINE), by = "planning_area_name") |>
    mutate(no_data = is.na(engagement_pct) | classified_establishments < 30)
  p4_map <- ggplot(map_data) +
    geom_sf(aes(fill = ifelse(no_data, NA, engagement_pct)), colour = "white", linewidth = 0.2) +
    scale_fill_gradientn(colours = SEQ, na.value = "#E3E8EB", name = "Share of reviews, %") +
    labs(title = paste("Estimated demand for", V4_CUISINE, "food"),
         subtitle = wrap("Share of classified Google Maps reviews by planning area; grey = fewer than 30 classified establishments", 100),
         caption = wrap("Source: Google Maps establishments classified by cuisine (v3). Review share is an engagement indicator, not measured demand.", 120)) +
    theme_hawker() +
    theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "right")
  save_plot(p4_map, "V4_cuisine_engagement_map.png", 10, 6)
}

# ============================================================
# 4. V5 - AGE-GROUPED BAR CHART (one planning area)
# ============================================================
# The Google Maps data has no age information, so cuisine preference by age cannot be
# measured. This chart shows the real age profile of the area's residents (SingStat,
# June 2026) against Singapore, i.e. WHO lives there. When a survey with age x cuisine
# preference is delivered, replace `v5_data` with it (age_group, share).

age_levels <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")
age_share <- function(d) d |> group_by(age_group) |>
  summarise(residents = sum(residents, na.rm = TRUE), .groups = "drop") |>
  mutate(share = 100 * residents / sum(residents))
v5_data <- bind_rows(
  age_share(filter(pa_age, planning_area_name == V5_AREA)) |> mutate(group = title_case(V5_AREA)),
  age_share(pa_age) |> mutate(group = "Singapore")
) |>
  mutate(age_group = factor(age_group, levels = age_levels),
         group = factor(group, levels = c(title_case(V5_AREA), "Singapore")))
stopifnot(nrow(v5_data) > 7)

p5 <- ggplot(v5_data, aes(age_group, share, fill = group)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", share)), position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3, colour = "#333333") +
  scale_fill_manual(values = setNames(c(GREEN_DARK, GREY), levels(v5_data$group)), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)), labels = function(x) paste0(x, "%")) +
  labs(title = paste("Age profile of residents:", title_case(V5_AREA)),
       subtitle = wrap("Share of residents by age group, June 2026. Cuisine preference by age is not observable in the current data", 100),
       x = "Age group", y = NULL,
       caption = wrap("Source: SingStat resident population by single year of age, 2026 (v2 database). Not a cuisine preference chart: needs the survey.", 120)) +
  theme_hawker() +
  theme(panel.grid.major.x = element_blank(), legend.position = "top", legend.justification = "left")
save_plot(p5, "V5_age_profile.png", 10, 6)

# ============================================================
# 5. V6 - DIVERGING BAR: ENGAGEMENT SHARE MINUS SUPPLY SHARE
# ============================================================

c6 <- centre_gap |> filter(grepl(V6_CENTRE, centre_name, ignore.case = TRUE))
if (n_distinct(c6$centre_id) != 1) stop("V6_CENTRE must match exactly one hawker centre.")
v6_data <- c6 |>
  mutate(side = ifelse(gap_pts >= 0, "More review attention than stalls", "More stalls than review attention"),
         cuisine_group = reorder(cuisine_group, gap_pts),
         label = sprintf("%+.1f", gap_pts))

p6 <- ggplot(v6_data, aes(gap_pts, cuisine_group, fill = side)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, colour = "#58595B") +
  geom_text(aes(label = label, hjust = ifelse(gap_pts >= 0, -0.15, 1.15)), size = 3.4, colour = "#333333") +
  scale_fill_manual(values = c("More review attention than stalls" = GREEN_DARK,
                               "More stalls than review attention" = ORANGE), name = NULL) +
  scale_x_continuous(expand = expansion(mult = 0.18), labels = function(x) paste0(x, " pts")) +
  labs(title = paste("Cuisine engagement vs hawker supply at", sub(" \\(.*$", "", unique(v6_data$centre_name))),
       subtitle = wrap(paste0("Share of Google Maps reviews in ", title_case(unique(v6_data$planning_area_name)),
                         " minus share of occupied main-meal stalls at the centre, percentage points"), 100),
       x = NULL, y = NULL,
       caption = wrap("Sources: Google Maps reviews by cuisine (v3); Hawker Guide stalls. An exploratory gap, not unmet demand: Google Maps covers restaurants as well as hawkers.", 120)) +
  theme_hawker() +
  theme(panel.grid.major.y = element_blank(), legend.position = "top", legend.justification = "left")
save_plot(p6, "V6_engagement_supply_gap.png", 10, 6)

# ============================================================
# 6. V7 - GAP vs RENT SCATTER (one cuisine)
# ============================================================

v7_data <- centre_gap |>
  filter(cuisine_group == V7_CUISINE, !is.na(gap_pts), !is.na(median_amr),
         classified_establishments >= 30)
med_rent <- median(v7_data$median_amr)
v7_data <- v7_data |>
  mutate(opportunity = gap_pts > 0 & median_amr <= med_rent,
         short_name = sub(" \\(.*$", "", centre_name))
top_labels <- v7_data |> filter(opportunity) |> arrange(desc(gap_pts)) |> head(8)

p7 <- ggplot(v7_data, aes(gap_pts, median_amr)) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = med_rent, fill = "#E8F7EF", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#8B96A5") +
  geom_hline(yintercept = med_rent, linetype = "dashed", colour = "#8B96A5") +
  geom_point(aes(colour = opportunity), size = 3, alpha = 0.85) +
  geom_text_repel(data = top_labels, aes(label = short_name), size = 3, colour = "#1E2B22",
                  min.segment.length = 0, box.padding = 0.4, max.overlaps = 20) +
  scale_colour_manual(values = c(`TRUE` = GREEN_DARK, `FALSE` = GREY), guide = "none") +
  scale_y_continuous(labels = function(x) paste0("S$", comma(x))) +
  scale_x_continuous(labels = function(x) paste0(x, " pts")) +
  annotate("text", x = 0, y = -Inf, hjust = -0.05, vjust = -0.6, colour = GREEN_DARK, fontface = "bold",
           size = 3.6, label = "UNDER-SUPPLIED + BELOW-MEDIAN RENT") +
  labs(title = paste("Where", V7_CUISINE, "food is under-supplied and rent is lower"),
       subtitle = wrap(paste0(nrow(v7_data), " hawker centres with tender listings. Across: engagement share minus supply share (pts). ",
                         "Up: median NEA reference rent (AMR). Dashed: zero gap and median rent"), 100),
       x = "Engagement - supply gap for the cuisine (percentage points)",
       y = "Median assessed market rent (AMR, S$ per month)",
       caption = wrap("Sources: Google Maps reviews by cuisine (v3); Hawker Guide; NEA tender notices. AMR is a reference rent, not the awarded bid. Areas with fewer than 30 classified establishments excluded.", 120)) +
  theme_hawker()
save_plot(p7, "V7_gap_vs_rent.png", 10, 6.5)

# ============================================================
# 7. CONSOLE SUMMARY
# ============================================================
cat("\nV6 gaps at", unique(v6_data$centre_name), "\n")
print(v6_data |> arrange(desc(gap_pts)) |>
        transmute(cuisine_group, engagement_pct, supply_pct = round(supply_pct, 1), gap_pts = round(gap_pts, 1)),
      row.names = FALSE)
cat("\nV7 opportunity centres for", V7_CUISINE, "(gap > 0 and rent <= median S$", med_rent, ")\n")
print(v7_data |> filter(opportunity) |> arrange(desc(gap_pts)) |>
        transmute(centre_name = short_name, planning_area_name, gap_pts = round(gap_pts, 1), median_amr),
      row.names = FALSE)
cat("\nDone. PNGs saved in:", out_dir, "\n")


# #############################################################################
# 3. V8 - MRT exit distance vs station activity (Lucien)
#    (was charts/V8_Lucien/V9_2026_graph.R)
# #############################################################################

# DBA3702 V8 - reproducible bubble chart + equal-weight ranking
# Reads hawker_data_2026.csv from the project's data/ folder (or beside this
# script). Running this script produces V9_2026_GRAPH.png and
# V9_2026_RANKING.csv in the same folder as the script.
for (pkg in c('ggplot2','patchwork')) {
  if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')
}
library(ggplot2)
library(patchwork)

# Resolve the checked-in CSV beside this script from RStudio or Rscript.
script_path <- grep('^--file=', commandArgs(FALSE), value=TRUE)
script_path <- if (length(script_path)) sub('^--file=', '', script_path[1]) else ''
if (!nzchar(script_path) && requireNamespace('rstudioapi', quietly=TRUE) &&
    rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
}
outdir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()
input <- file.path(outdir, 'hawker_data_2026.csv')
if (!file.exists(input)) input <- file.path(outdir, '..', '..', 'data', 'hawker_data_2026.csv')
if (!file.exists(input)) stop('Missing hawker_data_2026.csv (looked beside V9_2026_graph.R and in data/)')
df <- read.csv(input, stringsAsFactors=FALSE)
required <- c('centre_id','centre_name','nearest_station_name','straight_line_distance_m',
              'avg_daily_tap_outs','subzone_population_2026','match_status')
stopifnot(all(required %in% names(df)))
for (col in c('straight_line_distance_m','avg_daily_tap_outs','subzone_population_2026')) df[[col]] <- as.numeric(df[[col]])
df <- subset(df, match_status=='MATCHED' & is.finite(straight_line_distance_m) &
                 is.finite(avg_daily_tap_outs) & is.finite(subzone_population_2026) &
                 straight_line_distance_m>=0 & avg_daily_tap_outs>=0 & subzone_population_2026>0)
stopifnot(nrow(df)>=5, !anyDuplicated(df$centre_id))
scale100 <- function(x) if (max(x)==min(x)) rep(50,length(x)) else 100*(x-min(x))/(max(x)-min(x))
df$distance_score <- 100-scale100(df$straight_line_distance_m)
df$activity_score <- scale100(df$avg_daily_tap_outs)
df$population_score <- scale100(df$subzone_population_2026)
df$combined_score <- (df$distance_score + df$activity_score + df$population_score)/3
df <- df[order(-df$combined_score,df$straight_line_distance_m,df$centre_name),]
df$overall_rank <- seq_len(nrow(df))
write.csv(df,file.path(outdir,'V9_2026_RANKING.csv'),row.names=FALSE)
top5 <- head(df,5)
mid_x <- median(df$straight_line_distance_m)
mid_y <- median(df$avg_daily_tap_outs)
df$zone <- ifelse(df$straight_line_distance_m<=mid_x & df$avg_daily_tap_outs>=mid_y,'Close + busy','Other')
fmt <- function(x) format(round(x),big.mark=',',trim=TRUE,scientific=FALSE)
max_x <- max(1600,max(top5$straight_line_distance_m)+100)
max_y <- max(df$avg_daily_tap_outs)*1.12
p1 <- ggplot(df,aes(straight_line_distance_m,avg_daily_tap_outs)) +
  annotate('rect',xmin=0,xmax=mid_x,ymin=mid_y,ymax=Inf,fill='#E8F7EF',alpha=.75) +
  geom_vline(xintercept=mid_x,linetype='dashed',colour='#8B96A5') +
  geom_hline(yintercept=mid_y,linetype='dashed',colour='#8B96A5') +
  geom_point(data=subset(df,zone=='Other'),aes(size=subzone_population_2026),colour='#A9B4C3',alpha=.48) +
  geom_point(data=subset(df,zone=='Close + busy'),aes(size=subzone_population_2026),colour='#17A673',alpha=.87) +
  geom_point(data=top5,shape=21,fill='#123B69',colour='white',stroke=1.1,size=6.8) +
  geom_text(data=top5,aes(label=overall_rank),colour='white',fontface='bold',size=4) +
  annotate('text',x=35,y=max_y*.97,label='CLOSE + BUSY',colour='#087A55',fontface='bold',hjust=0,size=5) +
  coord_cartesian(xlim=c(0,max_x),ylim=c(0,max_y)) +
  scale_x_continuous(breaks=seq(0,ceiling(max_x/400)*400,400),labels=fmt) +
  scale_y_continuous(labels=function(x) paste0(round(x/1000),'k')) +
  scale_size_area(max_size=12,name='Subzone residents (2026)',labels=fmt) +
  labs(title='MRT Accessibility & Local Market Potential',
       subtitle=paste0(nrow(df),' hawker centres | Green = close + busy | Numbers = top 5 by equal-weight score'),
       x='Distance to nearest MRT/LRT exit (m)',y='Average daily MRT/LRT station exits') +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face='bold',size=19,colour='#172B43'),
        plot.subtitle=element_text(size=10.5,colour='#536477'),
        axis.title=element_text(face='bold'),panel.grid.minor=element_blank(),
        panel.grid.major=element_line(colour='#E8EDF2'),legend.position='bottom')
top5$panel_y <- 5:1
top5$display_name <- vapply(top5$centre_name,function(s) paste(head(strwrap(s,width=38),2),collapse='\n'),character(1))
top5$station_display <- gsub(' (MRT|LRT) STATION$','',top5$nearest_station_name)
top5$detail <- paste0(fmt(top5$straight_line_distance_m),' m | ',fmt(top5$avg_daily_tap_outs),
                        ' exits/day | Score ',sprintf('%.1f',top5$combined_score))
p2 <- ggplot() +
  geom_text(data=top5,aes(x=.17,y=panel_y+.20,label=display_name),hjust=0,vjust=.5,
            fontface='bold',size=3.5,colour='#172B43',lineheight=.95) +
  geom_text(data=top5,aes(x=.17,y=panel_y-.10,label=station_display),hjust=0,size=3.0,colour='#365B7C') +
  geom_text(data=top5,aes(x=.17,y=panel_y-.31,label=detail),hjust=0,size=2.7,colour='#5D6C7D') +
  geom_point(data=top5,aes(x=.06,y=panel_y),size=8,colour='#123B69') +
  geom_text(data=top5,aes(x=.06,y=panel_y,label=overall_rank),colour='white',fontface='bold',size=4) +
  annotate('text',x=0,y=5.8,label='TOP 5 HAWKER CENTRES',fontface='bold',hjust=0,size=5,colour='#172B43') +
  annotate('text',x=0,y=5.5,label='Ranked across all centres',hjust=0,size=3.4,colour='#536477') +
  annotate('text',x=0,y=.25,label='Score = (Distance + Activity + Population) / 3',hjust=0,size=3.1,colour='#087A55') +
  coord_cartesian(xlim=c(-.02,1.1),ylim=c(.05,5.98),clip='off')+theme_void()+theme(plot.margin=margin(18,25,10,10))
final <- (p1+p2+plot_layout(widths=c(1.9,1.25))) +
  plot_annotation(caption=paste0('Equal weights (1/3 each), min-max normalized across ', nrow(df), ' centres. Straight-line distance; station tap-outs are not hawker footfall. Population: 2026; station activity: Sep 2026.'))
print(final)
ggsave(file.path(outdir,'V9_2026_GRAPH.png'),final,width=19,height=10,units='in',dpi=300,bg='white',limitsize=FALSE)
cat('Done. Files saved in:',outdir,'\n')
print(top5[,c('overall_rank','centre_name','combined_score')],row.names=FALSE)


# #############################################################################
# 4. V9 - Stall relisting / tenancy risk (Casper)
#    (was charts/V9_Casper/CasperCode.R)
# #############################################################################

# =============================================================================
# DBA3702 Team 03 - Hawker Project
# V9: Relisting frequency ranked bar  (RQ5 - tenancy risk)
# Owner: Casper
#
# Single self-contained script. Paste the whole thing into RStudio and run it
# top to bottom (Ctrl+Shift+Enter / Cmd+Shift+Enter).
#
# It will:
#   1. install any missing packages
#   2. ask for your MySQL password in a popup
#   3. pull the tender listings from v_tender_dashboard
#   4. validate them against data dictionary v2.0 and print a report
#   5. save data/tender_listings.csv for the submission zip
#   6. compute stall-level relisting history and centre-level churn
#   7. draw V9 and save it to output/v9_relisting.png
#
# EDIT THE DB SETTINGS IN SECTION 1 BEFORE RUNNING.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Packages
# -----------------------------------------------------------------------------
required <- c("DBI", "RMariaDB", "dplyr", "tidyr", "ggplot2", "scales")
missing  <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
# Optional: hides your typing if RStudio's own popup is unavailable.
if (!requireNamespace("getPass", quietly = TRUE)) {
  try(install.packages("getPass"), silent = TRUE)
}

suppressPackageStartupMessages({
  library(DBI); library(RMariaDB)
  library(dplyr); library(tidyr)
  library(ggplot2); library(scales)
})


# -----------------------------------------------------------------------------
# 1. Database settings   <<< EDIT THESE >>>
# -----------------------------------------------------------------------------
# The password is NOT stored here. You are prompted for it below, so this file
# is safe to commit and to hand in with the project.
DB <- list(
  host     = Sys.getenv("HAWKER_DB_HOST", "127.0.0.1"),
  port     = as.integer(Sys.getenv("HAWKER_DB_PORT", "3306")),
  dbname   = Sys.getenv("HAWKER_DB_NAME", "hawker_project"),
  username = Sys.getenv("HAWKER_DB_USER", "root")
)

script_path <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_path)) sub("^--file=", "", script_path[1]) else ""
if (!nzchar(script_path) && requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
}
project_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()


# -----------------------------------------------------------------------------
# 2. Connection
# -----------------------------------------------------------------------------
ask_db_password <- function(prompt = "MySQL password for hawker_project: ") {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(rstudioapi::askForPassword(prompt))
  }
  if (requireNamespace("getPass", quietly = TRUE)) {
    return(getPass::getPass(prompt))
  }
  if (interactive()) {
    message("Note: install.packages('getPass') to hide your typing.")
    return(readline(prompt))
  }
  pw <- Sys.getenv("HAWKER_DB_PASSWORD")
  if (nzchar(pw)) return(pw)
  stop("Not interactive and HAWKER_DB_PASSWORD is unset.", call. = FALSE)
}

hawker_connect <- function(settings = DB, password = NULL) {
  if (is.null(password)) password <- ask_db_password()
  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = settings$host, port = settings$port,
    dbname = settings$dbname, username = settings$username,
    password = password
  )
  DBI::dbExecute(con, "SET NAMES utf8mb4")   # centre names contain non-ASCII
  message("Connected to ", settings$dbname, " on ", settings$host)
  con
}


# -----------------------------------------------------------------------------
# 3. The query
# -----------------------------------------------------------------------------
# v_tender_dashboard is the dictionary's recommended tender-level starting
# point: one row per listing_id, already joined to the official centre.
#
# We deliberately do not pull the population / zoning / MRT columns the view
# also carries. Those belong to V8 and the map, and the dictionary warns that
# subzone population must never be summed across tender rows.
SQL_TENDER_LISTINGS <- "
  SELECT
    listing_id,
    centre_id,
    centre_name,
    stall_id,
    stall_no_normalised,
    exercise_month,
    tender_open_date,
    tender_close_date,
    release_category,
    approximate_area_m2,
    median_centre_amr_sgd,
    needs_review
  FROM v_tender_dashboard
  ORDER BY centre_id, stall_id, exercise_month
"

pull_tender_listings <- function(con) {
  d <- DBI::dbGetQuery(con, SQL_TENDER_LISTINGS)
  d$exercise_month    <- as.Date(d$exercise_month)
  d$tender_open_date  <- as.Date(d$tender_open_date)
  d$tender_close_date <- as.Date(d$tender_close_date)
  d
}


# -----------------------------------------------------------------------------
# 4. Validation against data dictionary v2.0
# -----------------------------------------------------------------------------
# If reality disagrees with the dictionary we want to know now, not during Q&A.
validate_tender_listings <- function(listings,
                                     expected_listings = 487L,
                                     expected_centres  = 129L) {
  cat("\n=== Validating tender listings against dictionary v2.0 ===\n")
  
  n_rows <- nrow(listings)
  cat(sprintf("Rows pulled ................. %d (dictionary expects %d)\n",
              n_rows, expected_listings))
  
  n_ids <- length(unique(listings$listing_id))
  if (n_ids != n_rows) {
    stop(sprintf("Grain broken: %d rows but %d distinct listing_id. Stop here.",
                 n_rows, n_ids), call. = FALSE)
  }
  cat("listing_id unique ........... yes\n")
  
  if (n_rows != expected_listings) {
    warning(sprintf("Row count %d differs from the documented %d - has the database been reloaded?",
                    n_rows, expected_listings), call. = FALSE)
  }
  
  # Centres in tenders are a SUBSET of all 129: a centre with no tender in the
  # window simply does not appear.
  n_centres <- length(unique(listings$centre_id))
  cat(sprintf("Centres with >=1 listing .... %d (of %d official centres)\n",
              n_centres, expected_centres))
  
  # The dictionary requires BOTH stall_id and centre_id to identify a stall.
  reuse <- listings %>% distinct(stall_id, centre_id) %>% count(stall_id) %>% filter(n > 1)
  if (nrow(reuse) > 0) {
    cat(sprintf("stall_id reused across centres: %d -> always join on both keys\n",
                nrow(reuse)))
  } else {
    cat("stall_id unique across centres: yes (still join on both keys)\n")
  }
  
  months <- sort(unique(listings$exercise_month))
  cat(sprintf("Exercise months ............. %d, %s to %s\n", length(months),
              format(min(months), "%b %Y"), format(max(months), "%b %Y")))
  
  cat("\nrelease_category levels (check these against the colour mapping below):\n")
  print(table(listings$release_category, useNA = "ifany"))
  
  cat("\nMissing values in key fields:\n")
  print(c(stall_id         = sum(is.na(listings$stall_id)),
          exercise_month   = sum(is.na(listings$exercise_month)),
          release_category = sum(is.na(listings$release_category))))
  if (any(is.na(listings$exercise_month))) {
    stop("exercise_month is NULL in some rows; V9/V10 cannot use them.", call. = FALSE)
  }
  
  flagged <- sum(listings$needs_review == 1, na.rm = TRUE)
  cat(sprintf("\nFlagged needs_review ........ %d (%.1f%%)\n",
              flagged, 100 * flagged / n_rows))
  cat("=== Validation complete ===\n\n")
  invisible(TRUE)
}

cache_tender_listings <- function(listings, path = "data/tender_listings.csv") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(listings, path, row.names = FALSE, na = "")
  message("Cached ", nrow(listings), " listings to ", path)
  invisible(path)
}


# -----------------------------------------------------------------------------
# 5. Relisting metrics
# -----------------------------------------------------------------------------

# Month as an integer so consecutive months differ by exactly 1.
month_index <- function(d) {
  as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))
}

# Longest run of CONSECUTIVE months. A stall listed Jan-Feb-Mar and again in
# Aug has a longest run of 3. This separates "stuck vacant for a quarter" from
# "came up twice, far apart" - the plain listing count cannot tell them apart.
longest_consecutive_run <- function(idx) {
  idx <- sort(unique(idx))
  if (length(idx) == 0L) return(0L)
  if (length(idx) == 1L) return(1L)
  max(tabulate(cumsum(c(1L, diff(idx) != 1L))))
}

# Which release_category values mean "nobody bid last time"? Matched on text,
# not hard-coded, because the dictionary gives only one example value. Check
# the levels printed by validate_tender_listings() against this.
is_no_bid_category <- function(x) {
  x <- tolower(trimws(as.character(x)))
  grepl("no[ _-]?bid|unbid|un[ _-]?bid|no[ _-]?offer|unsuccessful", x)
}

# One row per stall (centre_id + stall_id) describing its tender history.
stall_relisting_history <- function(listings) {
  listings %>%
    filter(!is.na(stall_id), !is.na(exercise_month)) %>%
    mutate(.m = month_index(exercise_month)) %>%
    group_by(centre_id, centre_name, stall_id) %>%
    summarise(
      stall_no    = dplyr::first(stall_no_normalised),
      n_listings  = dplyr::n(),
      n_months    = dplyr::n_distinct(exercise_month),
      first_month = min(exercise_month),
      last_month  = max(exercise_month),
      span_months = max(.m) - min(.m) + 1L,
      longest_run = longest_consecutive_run(.m),
      n_no_bid    = sum(is_no_bid_category(release_category)),
      .groups = "drop"
    ) %>%
    mutate(
      relisted   = n_months > 1L,
      continuous = n_months == span_months & n_months > 1L   # never let go
    )
}

# One row per centre: the headline V9 numbers.
#   repeat_ratio = listings / distinct stalls
#   1.0 = every stall listed once; 3.0 = average stall came back three times.
# min_stalls keeps a centre with one stall listed twice out of the ranking -
# that is a sample of one, not evidence.
centre_relisting_summary <- function(listings, min_stalls = 3L) {
  stall_relisting_history(listings) %>%
    group_by(centre_id, centre_name) %>%
    summarise(
      n_stalls          = dplyr::n(),
      n_listings        = sum(n_listings),
      n_relisted_stalls = sum(relisted),
      n_continuous      = sum(continuous),
      max_run           = max(longest_run),
      n_no_bid          = sum(n_no_bid),
      first_month       = min(first_month),
      last_month        = max(last_month),
      .groups = "drop"
    ) %>%
    mutate(
      repeat_ratio   = n_listings / n_stalls,
      share_relisted = n_relisted_stalls / n_stalls,
      no_bid_share   = n_no_bid / n_listings,
      rankable       = n_stalls >= min_stalls
    ) %>%
    arrange(desc(repeat_ratio), desc(n_stalls))
}

# Split each centre's repeat ratio by release_category.
#
# Why stacking a RATIO is legitimate here:
#   repeat_ratio = sum_over_categories(listings_in_category) / distinct_stalls
# so each category contributes listings_cat / distinct_stalls and the
# contributions sum exactly to the bar length. The "no bids" segment is
# therefore literally the portion of a centre's churn that is failed tenders
# rather than ordinary turnover.
centre_ratio_by_category <- function(listings, min_stalls = 3L) {
  centres <- centre_relisting_summary(listings, min_stalls = min_stalls)
  
  listings %>%
    filter(!is.na(stall_id), !is.na(exercise_month)) %>%
    mutate(release_category = ifelse(is.na(release_category) | release_category == "",
                                     "unstated", as.character(release_category))) %>%
    count(centre_id, release_category, name = "listings_in_category") %>%
    left_join(centres %>% select(centre_id, centre_name, n_stalls, n_listings,
                                 repeat_ratio, no_bid_share, share_relisted,
                                 max_run, rankable),
              by = "centre_id") %>%
    mutate(ratio_contribution = listings_in_category / n_stalls,
           category_share     = listings_in_category / n_listings) %>%
    arrange(desc(repeat_ratio), centre_id)
}

# How much of the data CANNOT show relisting because the window ran out?
# A stall first listed in the final month has had no chance to reappear; its
# centre's ratio is pulled toward 1.0 for a reason unrelated to demand.
# Report this, do not correct for it.
censoring_report <- function(listings, tail_months = 2L) {
  stalls <- stall_relisting_history(listings)
  last_m <- max(month_index(listings$exercise_month))
  late <- stalls %>%
    mutate(months_remaining = last_m - month_index(first_month)) %>%
    filter(months_remaining < tail_months)
  list(window_last_month = max(listings$exercise_month),
       n_stalls_total    = nrow(stalls),
       n_stalls_censored = nrow(late),
       pct_censored      = 100 * nrow(late) / nrow(stalls),
       centres_affected  = sort(unique(late$centre_name)))
}

# Numbers to quote in the report.
relisting_overview <- function(listings) {
  stalls  <- stall_relisting_history(listings)
  centres <- centre_relisting_summary(listings)
  list(n_listings           = nrow(listings),
       n_stalls             = nrow(stalls),
       n_centres            = nrow(centres),
       overall_ratio        = nrow(listings) / nrow(stalls),
       pct_stalls_relisted  = 100 * mean(stalls$relisted),
       median_centre_ratio  = stats::median(centres$repeat_ratio),
       max_centre_ratio     = max(centres$repeat_ratio),
       longest_run_anywhere = max(stalls$longest_run))
}


# -----------------------------------------------------------------------------
# 6. V9 chart
# -----------------------------------------------------------------------------
# Colour is assigned to the CATEGORY, never to rank, so filtering the chart
# does not repaint the surviving segments. The no-bid slot gets the warm hue:
# it is the part of the churn the entrepreneur should worry about. "unstated"
# is deliberately neutral grey - it marks absent data, not a fourth series.
V9_SURFACE <- "#fcfcfb"; V9_INK <- "#0b0b0b"
V9_INK_2   <- "#52514e"; V9_MUTED <- "#898781"
# Used by the funnel and the two competition charts further down.
V9_BLUE    <- "#2a78d6"; V9_GREEN <- "#1baf7a"; V9_ORANGE <- "#eb6834"

v9_category_colours <- function(levels_present) {
  pal <- c("unstated"             = "#898781",
           "no_bid"               = "#eb6834",
           "second_or_subsequent" = "#2a78d6",
           "first_release"        = "#1baf7a")
  out <- pal[levels_present]
  out[is.na(out)] <- V9_MUTED      # unanticipated level -> grey, never a cycled hue
  names(out) <- levels_present
  out
}

# Map raw text onto the four display categories, so the chart survives the
# database spelling the levels differently from the dictionary's example.
v9_classify_category <- function(x) {
  raw <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    is.na(x) | raw %in% c("", "na", "null")            ~ "unstated",
    grepl("no[ _-]?bid|unbid|unsuccessful", raw)       ~ "no_bid",
    grepl("second|subsequent|repeat|re-?release", raw) ~ "second_or_subsequent",
    grepl("first", raw)                                ~ "first_release",
    TRUE                                               ~ raw
  )
}

V9_CATEGORY_LABELS <- c(
  unstated             = "Not stated",
  no_bid               = "Previously released, no bids",
  second_or_subsequent = "Second or subsequent release",
  first_release        = "First release"
)

# ggplot stacks the LAST level at the baseline, so this renders as:
# first_release at the axis, then second_or_subsequent, then no_bid, with
# unstated at the far end.
#
# NOTE on reading the green block: it is NOT always 1.0 wide. A stall whose
# first appearance INSIDE the window is already labelled second_or_subsequent
# was released before Oct 2025, so it contributes no first_release at all.
# Centres where the green block falls well short of the dashed line are
# therefore carrying vacancies that predate the data - which is a stronger
# tenancy-risk signal than the ratio alone, not a flaw in the chart.
V9_STACK_ORDER <- c("unstated", "no_bid", "second_or_subsequent", "first_release")

# Official NEA centre names carry a block-and-street prefix and the common name
# in brackets ("Upper Cross Street Blk 531A (Hong Lim Food Centre and Market)").
# Printed in full they squeeze the plot panel to nothing. Prefer the bracketed
# common name - it is also what a Singaporean reader recognises - and fall back
# to a truncation.
v9_short_name <- function(x, max_chars = 42L) {
  inner <- sub("^[^(]*\\(([^)]*)\\).*$", "\\1", x)
  out   <- ifelse(grepl("\\(", x) & nchar(inner) > 0 & nchar(inner) < nchar(x), inner, x)
  ifelse(nchar(out) > max_chars, paste0(substr(out, 1, max_chars - 1L), "…"), out)
}

plot_v9_relisting <- function(listings, top_n = 15L, min_stalls = 3L,
                              show_counts = TRUE) {
  
  dat <- centre_ratio_by_category(listings, min_stalls = min_stalls) %>%
    filter(rankable) %>%
    mutate(cat = v9_classify_category(release_category)) %>%
    group_by(centre_id, centre_name, n_stalls, n_listings, repeat_ratio,
             no_bid_share, max_run, cat) %>%
    summarise(ratio_contribution = sum(ratio_contribution), .groups = "drop")
  
  if (nrow(dat) == 0L) {
    stop("No centres meet min_stalls = ", min_stalls,
         ". Lower the threshold or check the pull.", call. = FALSE)
  }
  
  keep <- dat %>% distinct(centre_id, repeat_ratio, n_stalls) %>%
    arrange(desc(repeat_ratio), desc(n_stalls)) %>% head(top_n)
  dat <- dat %>% filter(centre_id %in% keep$centre_id)
  
  # The axis label carries the sample size, so a high ratio built on four
  # stalls cannot be mistaken for one built on forty.
  dat <- dat %>%
    mutate(centre_label = if (show_counts)
      sprintf("%s  (%d stalls, %d listings)",
              v9_short_name(centre_name), n_stalls, n_listings)
      else v9_short_name(centre_name),
      cat = factor(cat, levels = intersect(V9_STACK_ORDER, unique(cat))))
  
  lvls <- levels(dat$cat)
  cols <- v9_category_colours(lvls)
  labs_cat <- ifelse(lvls %in% names(V9_CATEGORY_LABELS),
                     V9_CATEGORY_LABELS[lvls], lvls)
  
  bar_totals <- dat %>% group_by(centre_label, repeat_ratio) %>%
    summarise(total = sum(ratio_contribution), .groups = "drop")
  
  ggplot(dat, aes(x = ratio_contribution,
                  y = reorder(centre_label, repeat_ratio),
                  fill = cat)) +
    # Surface-coloured border = a clean gap between stacked segments.
    geom_col(width = 0.68, colour = V9_SURFACE, linewidth = 0.7) +
    geom_vline(xintercept = 1, linetype = "dashed",
               colour = V9_MUTED, linewidth = 0.5) +
    geom_text(data = bar_totals,
              aes(x = total, y = reorder(centre_label, repeat_ratio),
                  label = sprintf("%.1f", total)),
              inherit.aes = FALSE, hjust = -0.25, size = 3.1, colour = V9_INK_2) +
    scale_fill_manual(values = cols, labels = labs_cat, name = NULL,
                      guide = guide_legend(reverse = TRUE, nrow = 1)) +
    # No multiplication glyph: it is missing from the default device font on
    # some machines and renders as "..".
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      title    = "Which hawker centres keep putting the same stalls back out to tender?",
      subtitle = paste0("Listings per distinct stall over the tender window. Dashed line at 1.0 = every ",
                        "stall listed once.\nEverything to the right of it is repeat activity, split by ",
                        "how NEA released each listing."),
      x = "Listings per distinct stall", y = NULL,
      caption = paste0("Source: NEA monthly tender notices via hawker_project.v_tender_dashboard. ",
                       "Centres with fewer than ", min_stalls, " distinct stalls excluded.\n",
                       "Assessed Market Rent is a reference value, not a winning bid. Stalls first ",
                       "listed near the end of the window have had less opportunity to reappear.")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, colour = V9_INK),
      plot.subtitle = element_text(colour = V9_INK_2, size = 9.5, lineheight = 1.2),
      plot.caption  = element_text(colour = V9_MUTED, size = 7.5, hjust = 0, lineheight = 1.2),
      plot.caption.position = "plot", plot.title.position = "plot",
      axis.text.y   = element_text(colour = V9_INK, size = 9),
      axis.text.x   = element_text(colour = V9_MUTED),
      axis.title.x  = element_text(colour = V9_INK_2, size = 9),
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#e8e7e3", linewidth = 0.4),
      # byrow + left justification keeps the legend from running off the right
      # edge once the real (long) centre names widen the y axis.
      legend.position = "top", legend.justification = "left",
      legend.direction = "horizontal", legend.box.just = "left",
      legend.margin = margin(0, 0, 4, 0),
      legend.key.size = unit(10, "pt"),
      legend.text   = element_text(size = 9, colour = V9_INK_2),
      plot.background  = element_rect(fill = V9_SURFACE, colour = NA),
      panel.background = element_rect(fill = V9_SURFACE, colour = NA),
      plot.margin = margin(14, 18, 10, 14)
    )
}

# The numbers behind V9, for the appendix and for anyone who cannot read colour.
v9_table <- function(listings, min_stalls = 3L) {
  centre_relisting_summary(listings, min_stalls = min_stalls) %>%
    filter(rankable) %>%
    transmute(Centre = centre_name,
              Stalls = n_stalls,
              Listings = n_listings,
              `Listings/stall` = round(repeat_ratio, 2),
              `% stalls relisted` = round(100 * share_relisted, 1),
              `% listings no-bid` = round(100 * no_bid_share, 1),
              `Longest run (months)` = max_run) %>%
    arrange(desc(`Listings/stall`))
}


# -----------------------------------------------------------------------------
# V9a - the funnel
# -----------------------------------------------------------------------------
# Shows how 129 official centres become the 15 bars in V9, so nobody has to
# wonder what was left out. Single series, so no legend: the labels carry it.

funnel_counts <- function(listings, total_centres = 129L,
                          min_stalls = 3L, settle_months = 4L) {
  per_centre <- listings %>%
    group_by(centre_id) %>%
    summarise(stalls = n_distinct(stall_id), .groups = "drop")
  
  n_listed   <- nrow(per_centre)
  n_rankable <- sum(per_centre$stalls >= min_stalls)
  
  # The final step must match whatever chart follows the funnel. These days
  # that is the competition pair, which additionally drops stalls advertised
  # too late to have had a fair chance to return - and with them, any centre
  # that falls back under min_stalls as a result.
  comp       <- competition_summary(listings, min_stalls, settle_months)
  n_assessed <- nrow(comp)
  pooled     <- sum(comp$taken_first) / sum(comp$stalls_assessed)
  n_best     <- sum(comp$share_taken >= pooled)
  n_worst    <- n_assessed - n_best
  
  data.frame(
    step = factor(1:4, levels = 4:1),
    label = c(
      "All hawker centres in the database",
      "Advertised at least one stall",
      paste0("Advertised ", min_stalls, " or more different stalls"),
      "Enough history left to judge"
    ),
    note = c(
      "",
      sprintf("%d centres advertised nothing at all", total_centres - n_listed),
      sprintf("%d dropped: too few stalls to rank fairly", n_listed - n_rankable),
      # No arrow glyph: missing from the default device font, renders as "...".
      sprintf("%d dropped: too few stalls once the last %d months are set aside  |  split %d competitive / %d below the rate",
              n_rankable - n_assessed, settle_months, n_best, n_worst)
    ),
    n = c(total_centres, n_listed, n_rankable, n_assessed),
    stringsAsFactors = FALSE
  )
}

#' The centres that drop out at the final step, for the appendix.
#'
#' These are centres that had enough stalls to rank, but not enough once the
#' stalls advertised in the final months - which have had no fair chance to
#' return - are set aside.
funnel_dropped_centres <- function(listings, min_stalls = 3L, settle_months = 4L) {
  last_m <- max(month_index(listings$exercise_month))
  
  per_stall <- listings %>%
    group_by(centre_id, centre_name, stall_id) %>%
    summarise(first_m = min(month_index(exercise_month)), .groups = "drop") %>%
    mutate(too_recent = first_m > last_m - settle_months)
  
  rankable <- per_stall %>% count(centre_id, name = "stalls") %>%
    filter(stalls >= min_stalls) %>% pull(centre_id)
  
  assessed <- competition_summary(listings, min_stalls, settle_months)$centre_id
  
  per_stall %>%
    filter(centre_id %in% setdiff(rankable, assessed)) %>%
    group_by(centre_name) %>%
    summarise(stalls = n(),
              advertised_too_recently = sum(too_recent),
              stalls_left = stalls - advertised_too_recently,
              .groups = "drop") %>%
    arrange(desc(stalls))
}

plot_v9a_funnel <- function(listings, total_centres = 129L,
                            min_stalls = 3L, settle_months = 4L) {
  f <- funnel_counts(listings, total_centres, min_stalls, settle_months)
  
  ggplot(f, aes(x = n, y = step)) +
    geom_col(width = 0.5, fill = V9_BLUE) +
    # Count inside the bar where there is room, so the eye lands on it first.
    geom_text(aes(label = n), hjust = 1.4, colour = "white",
              fontface = "bold", size = 5) +
    # Clear of the bar edge (half-width 0.25), or the descenders collide.
    geom_text(aes(x = 0, label = label), hjust = 0, nudge_y = 0.40,
              colour = V9_INK, size = 3.6) +
    geom_text(aes(x = n, label = note), hjust = -0.06, nudge_y = -0.02,
              colour = V9_MUTED, size = 3.1) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
    labs(
      title    = "Which hawker centres the next two charts actually cover",
      subtitle = "Not every centre advertised a stall, and not every one advertised enough, early enough, to judge fairly.",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, colour = V9_INK),
      plot.subtitle = element_text(colour = V9_INK_2, size = 9.5),
      plot.title.position = "plot",
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      plot.background  = element_rect(fill = V9_SURFACE, colour = NA),
      panel.background = element_rect(fill = V9_SURFACE, colour = NA),
      plot.margin = margin(14, 18, 10, 14)
    )
}


# -----------------------------------------------------------------------------
# V9b - where stalls get taken straight away
# -----------------------------------------------------------------------------
# The mirror image of V9. A stall advertised once that never comes back was
# almost certainly taken - we cannot see the award itself, only the silence
# afterwards, so the chart says "did not return", not "was won".
#
# Censoring matters MORE here than in V9: a stall first advertised in the last
# months of the window has had no chance to return, and would be scored as a
# success for free. Those stalls are excluded, not counted.
#
# WHY settle_months = 4. Measured on this data, of all the stalls that did come
# back to tender, 91% were back within 4 months (76% within 3, 94% within 5).
# So four months of silence is strong evidence a stall was taken; three months
# is not - a stall judged "taken" after only three months still had roughly a
# one-in-four chance of returning. Four is the point where the gain levels off,
# and it costs nothing here: 28 centres qualify at both 3 and 4 months, and the
# top and bottom of the ranking are unchanged at 3, 4 and 5.

competition_summary <- function(listings, min_stalls = 3L, settle_months = 4L) {
  last_m <- max(month_index(listings$exercise_month))
  
  listings %>%
    filter(!is.na(stall_id), !is.na(exercise_month)) %>%
    group_by(centre_id, centre_name, stall_id) %>%
    summarise(n_listings = n(),
              first_m    = min(month_index(exercise_month)),
              .groups = "drop") %>%
    # Only stalls with a fair chance to reappear.
    filter(first_m <= last_m - settle_months) %>%
    group_by(centre_id, centre_name) %>%
    summarise(stalls_assessed = n(),
              taken_first     = sum(n_listings == 1L),
              .groups = "drop") %>%
    filter(stalls_assessed >= min_stalls) %>%
    mutate(share_taken = taken_first / stalls_assessed) %>%
    arrange(desc(share_taken), desc(stalls_assessed))
}

plot_v9b_competition <- function(listings, top_n = 15L, min_stalls = 3L,
                                 settle_months = 4L) {
  d <- competition_summary(listings, min_stalls, settle_months)
  if (nrow(d) == 0L) stop("No centres qualify. Loosen min_stalls or settle_months.",
                          call. = FALSE)
  
  d <- head(d, top_n) %>%
    mutate(lab = sprintf("%s  (%d of %d stalls)",
                         v9_short_name(centre_name), taken_first, stalls_assessed))
  
  ggplot(d, aes(x = share_taken, y = reorder(lab, share_taken))) +
    geom_col(width = 0.68, fill = V9_GREEN) +
    geom_text(aes(label = scales::percent(share_taken, accuracy = 1)),
              hjust = -0.2, size = 3.1, colour = V9_INK_2) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1.08),
                       expand = expansion(mult = c(0, 0))) +
    labs(
      title    = "Where does a stall get taken the first time it is advertised?",
      subtitle = paste0("Share of stalls advertised once that never returned to tender. ",
                        "High = competitive, stalls do not stay empty."),
      x = "Share of stalls that did not return to tender", y = NULL,
      caption = paste0("Source: NEA monthly tender notices via hawker_project.v_tender_dashboard.\n",
                       "The data records advertisements, not awards: a stall that does not return was ",
                       "most likely taken, but this cannot be confirmed from the tender notices.\n",
                       "Stalls first advertised in the final ", settle_months,
                       " months are excluded - they have not had a fair chance to return. ",
                       "Centres with fewer than ", min_stalls, " assessed stalls excluded.")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, colour = V9_INK),
      plot.subtitle = element_text(colour = V9_INK_2, size = 9.5),
      plot.caption  = element_text(colour = V9_MUTED, size = 7.5, hjust = 0, lineheight = 1.2),
      plot.caption.position = "plot", plot.title.position = "plot",
      axis.text.y   = element_text(colour = V9_INK, size = 9),
      axis.text.x   = element_text(colour = V9_MUTED),
      axis.title.x  = element_text(colour = V9_INK_2, size = 9),
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#e8e7e3", linewidth = 0.4),
      plot.background  = element_rect(fill = V9_SURFACE, colour = NA),
      panel.background = element_rect(fill = V9_SURFACE, colour = NA),
      plot.margin = margin(14, 18, 10, 14)
    )
}


# -----------------------------------------------------------------------------
# V9c - both ends of the same measure, in one chart
# -----------------------------------------------------------------------------
# One metric, one axis: the share of a centre's stalls that were advertised
# once and never came back. High = stalls get taken. Low = stalls keep
# returning to tender.
#
# This replaces the need to read V9 and V9b side by side, and it uses the
# competition logic for BOTH ends rather than mixing a ratio with a share.
#
# The reference line is the overall rate across every assessed stall - not the
# mean of the centre percentages, which would weight a 4-stall centre the same
# as a 31-stall one.


#' @param n_each how many centres at each end
plot_v9c_both_ends <- function(listings, n_each = 8L, min_stalls = 3L,
                               settle_months = 4L) {
  d <- competition_summary(listings, min_stalls, settle_months)
  if (nrow(d) < 2L * n_each) {
    warning("Fewer than ", 2L * n_each, " centres qualify; showing what there is.",
            call. = FALSE)
    n_each <- max(1L, floor(nrow(d) / 2))
  }
  
  overall <- sum(d$taken_first) / sum(d$stalls_assessed)
  
  best  <- head(d, n_each)
  worst <- tail(d, n_each)
  
  both <- bind_rows(
    best  %>% mutate(group = "Stalls get taken and stay taken"),
    worst %>% mutate(group = "Stalls keep coming back to tender")
  ) %>%
    mutate(
      group = factor(group, levels = c("Stalls get taken and stay taken",
                                       "Stalls keep coming back to tender")),
      lab   = sprintf("%s  (%d of %d stalls)",
                      v9_short_name(centre_name), taken_first, stalls_assessed)
    )
  
  ggplot(both, aes(x = share_taken, y = reorder(lab, share_taken), fill = group)) +
    geom_col(width = 0.68) +
    geom_vline(xintercept = overall, linetype = "dashed",
               colour = V9_MUTED, linewidth = 0.5) +
    # Direct labels on every bar: the relief the palette check requires, and
    # the only way to read 0% bars, which have no length at all.
    geom_text(aes(label = scales::percent(share_taken, accuracy = 1)),
              hjust = -0.2, size = 3.1, colour = V9_INK_2) +
    scale_fill_manual(values = c("Stalls get taken and stay taken"   = V9_GREEN,
                                 "Stalls keep coming back to tender" = V9_ORANGE),
                      guide = "none") +   # the facet strips name the groups
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1.1), expand = expansion(mult = c(0, 0))) +
    facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(
      title    = "The best and worst hawker centres to bid at, on one measure",
      subtitle = paste0("Share of a centre's stalls that were advertised once and never returned to tender.\n",
                        "Dashed line = ", scales::percent(overall, accuracy = 1),
                        ", the rate across all assessed stalls."),
      x = "Share of stalls that did not return to tender", y = NULL,
      caption = paste0("Source: NEA monthly tender notices via hawker_project.v_tender_dashboard.\n",
                       "The data records advertisements, not awards: a stall that does not return was ",
                       "most likely taken, but this cannot be confirmed from the tender notices.\n",
                       "Stalls first advertised in the final ", settle_months,
                       " months are excluded. Centres with fewer than ", min_stalls,
                       " assessed stalls excluded. Read the stall counts: a small centre moves to either extreme easily.")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, colour = V9_INK),
      plot.subtitle = element_text(colour = V9_INK_2, size = 9.5, lineheight = 1.2),
      plot.caption  = element_text(colour = V9_MUTED, size = 7.5, hjust = 0, lineheight = 1.2),
      plot.caption.position = "plot", plot.title.position = "plot",
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 90, face = "bold",
                                       colour = V9_INK_2, size = 9),
      axis.text.y   = element_text(colour = V9_INK, size = 9),
      axis.text.x   = element_text(colour = V9_MUTED),
      axis.title.x  = element_text(colour = V9_INK_2, size = 9),
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#e8e7e3", linewidth = 0.4),
      panel.spacing.y = unit(14, "pt"),
      plot.background  = element_rect(fill = V9_SURFACE, colour = NA),
      panel.background = element_rect(fill = V9_SURFACE, colour = NA),
      plot.margin = margin(14, 18, 10, 14)
    )
}


# -----------------------------------------------------------------------------
# V9d - the same measure, split into two charts that between them show EVERY
#       qualifying centre
# -----------------------------------------------------------------------------
# v9c shows only the extremes. These two show all 28 centres that clear the
# thresholds: one chart for those at or above the pooled rate, one for those
# below it. Nothing is cropped, so there is no "why isn't my centre here?".
#
# The split point is the pooled rate (total stalls taken / total assessed),
# which is the same dashed line used in v9c - the two charts stay comparable.

#' @param half "best" (at or above the pooled rate) or "worst" (below it)
plot_competition_half <- function(listings, half = c("best", "worst"),
                                  min_stalls = 3L, settle_months = 4L) {
  half <- match.arg(half)
  
  all_c   <- competition_summary(listings, min_stalls, settle_months)
  overall <- sum(all_c$taken_first) / sum(all_c$stalls_assessed)
  
  d <- if (half == "best") filter(all_c, share_taken >= overall)
  else                filter(all_c, share_taken <  overall)
  
  if (nrow(d) == 0L) stop("No centres on the '", half, "' side.", call. = FALSE)
  
  fill_col <- if (half == "best") V9_GREEN else V9_ORANGE
  
  ttl <- if (half == "best")
    "Where stalls get taken: centres at or above the overall rate"
  else
    "Where stalls keep coming back: centres below the overall rate"
  
  lead <- if (half == "best")
    "These are the competitive centres. A stall that comes free here tends to go, and stay gone."
  else
    "These centres re-advertise more of their stalls than the national rate. Treat them as tenancy risk."
  
  d <- d %>% mutate(lab = sprintf("%s  (%d of %d stalls)",
                                  v9_short_name(centre_name),
                                  taken_first, stalls_assessed))
  
  ggplot(d, aes(x = share_taken, y = reorder(lab, share_taken))) +
    geom_col(width = 0.68, fill = fill_col) +
    geom_vline(xintercept = overall, linetype = "dashed",
               colour = V9_MUTED, linewidth = 0.5) +
    # Every bar labelled: 0% bars have no length to read, and the green fill
    # sits under 3:1 contrast on this surface.
    geom_text(aes(label = scales::percent(share_taken, accuracy = 1)),
              hjust = -0.2, size = 3.1, colour = V9_INK_2) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1.1), expand = expansion(mult = c(0, 0))) +
    labs(
      title    = ttl,
      subtitle = paste0(lead, "\n", nrow(d), " of ", nrow(all_c),
                        " centres that clear the thresholds. Dashed line = ",
                        scales::percent(overall, accuracy = 1),
                        ", the rate across all assessed stalls."),
      x = "Share of stalls that did not return to tender", y = NULL,
      caption = paste0("Source: NEA monthly tender notices via hawker_project.v_tender_dashboard.\n",
                       "The data records advertisements, not awards: a stall that does not return was ",
                       "most likely taken, but this cannot be confirmed from the tender notices.\n",
                       "Stalls first advertised in the final ", settle_months,
                       " months are excluded. Centres with fewer than ", min_stalls,
                       " assessed stalls excluded. Read the stall counts: a small centre moves to either extreme easily.")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, colour = V9_INK),
      plot.subtitle = element_text(colour = V9_INK_2, size = 9.5, lineheight = 1.2),
      plot.caption  = element_text(colour = V9_MUTED, size = 7.5, hjust = 0, lineheight = 1.2),
      plot.caption.position = "plot", plot.title.position = "plot",
      axis.text.y   = element_text(colour = V9_INK, size = 9),
      axis.text.x   = element_text(colour = V9_MUTED),
      axis.title.x  = element_text(colour = V9_INK_2, size = 9),
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#e8e7e3", linewidth = 0.4),
      plot.background  = element_rect(fill = V9_SURFACE, colour = NA),
      panel.background = element_rect(fill = V9_SURFACE, colour = NA),
      plot.margin = margin(14, 18, 10, 14)
    )
}


# -----------------------------------------------------------------------------
# 8. Run: connect, pull, validate, draw every chart
# -----------------------------------------------------------------------------
con <- hawker_connect()                      # <- password popup appears here
tender_listings <- pull_tender_listings(con)
DBI::dbDisconnect(con)

validate_tender_listings(tender_listings)    # check before trusting any chart
cache_tender_listings(tender_listings,
                      file.path(project_dir, "data", "tender_listings.csv"))

cat("\n--- Overview ---\n");            print(relisting_overview(tender_listings))
cat("\n--- Centres cut at the last funnel step ---\n")
print(as.data.frame(funnel_dropped_centres(tender_listings)))
cat("\n--- V9 table ---\n")
print(head(as.data.frame(v9_table(tender_listings)), 20))

output_dir <- file.path(project_dir, "output")
dir.create(output_dir, showWarnings = FALSE)

# In presentation order.
p_funnel <- plot_v9a_funnel(tender_listings)                  # 1. who we look at
p_best   <- plot_competition_half(tender_listings, "best")    # 2. stalls get taken
p_worst  <- plot_competition_half(tender_listings, "worst")   # 3. stalls come back
p_v9     <- plot_v9_relisting(tender_listings, top_n = 15)    # 4. relisting detail

print(p_funnel); print(p_best); print(p_worst); print(p_v9)

ggsave(file.path(output_dir, "v9a_funnel.png"),   p_funnel, width = 11.5, height = 4.6, dpi = 300, bg = V9_SURFACE)
ggsave(file.path(output_dir, "v9d_best.png"),     p_best,   width = 10.5, height = 5.8, dpi = 300, bg = V9_SURFACE)
ggsave(file.path(output_dir, "v9d_worst.png"),    p_worst,  width = 10.5, height = 8.2, dpi = 300, bg = V9_SURFACE)
ggsave(file.path(output_dir, "v9_relisting.png"), p_v9,     width = 11.0, height = 7.2, dpi = 300, bg = V9_SURFACE)

message("Done. Four charts written to output/")

