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
