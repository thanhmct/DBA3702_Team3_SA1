
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
