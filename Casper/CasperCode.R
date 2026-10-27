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
