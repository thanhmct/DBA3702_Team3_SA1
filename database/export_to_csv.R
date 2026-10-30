# =============================================================================
# Hawker Site Finder — database/export_to_csv.R
#
# Rebuilds the CSV copies in data/ from the MySQL dump, without MySQL: the dump
# is loaded into an in-memory SQLite database, the dump's own views are created,
# and the app's queries are run on it.
#
# Run from the project folder (where global.R is):
#   Rscript database/export_to_csv.R                        # the v3 dump in database/ -> data/
#   Rscript database/export_to_csv.R <dump.sql> <out_dir>   # another dump or folder
#   add --raw to also write every table of the dump to <out_dir>/raw/
# or open this file in RStudio and click Source (uses the defaults).
# Needs the DBI and RSQLite packages.
# =============================================================================

for (p in c("DBI", "RSQLite")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Install the ", p, " package first: install.packages(\"", p, "\")")
}

args <- commandArgs(trailingOnly = TRUE)
raw_copies <- "--raw" %in% args
args <- setdiff(args, "--raw")
dump <- if (length(args) >= 1) args[1] else file.path("database", "SA1_Team03_Project_hawker_data_v3.sql")
out  <- if (length(args) >= 2) args[2] else "data"
if (!file.exists(dump) || !file.exists("global.R")) {
  stop("Run this from the project folder (where global.R is), or pass the dump: ",
       "Rscript database/export_to_csv.R <dump.sql> <out_dir>")
}
dir.create(out, showWarnings = FALSE, recursive = TRUE)
lines <- readLines(dump, encoding = "UTF-8", warn = FALSE)

# ---- 1. column names from the CREATE TABLE blocks ---------------------------
tables <- list()
for (s in grep("^CREATE TABLE `\\w+` \\(", lines)) {
  e <- s + which(startsWith(lines[(s + 1):length(lines)], ") ENGINE"))[1]
  body <- lines[(s + 1):(e - 1)]
  tables[[sub("^CREATE TABLE `(\\w+)`.*", "\\1", lines[s])]] <-
    sub("^\\s*`(\\w+)`\\s.*", "\\1", grep("^\\s*`\\w+`\\s", body, value = TRUE))
}

# ---- 2. MySQL string literals -> SQLite ones --------------------------------
# MySQL escapes with a backslash (\' \\ \n ...), SQLite only doubles quotes (''). A dump
# has backslashes only inside text values, so the whole line is rewritten at once.
# Numbers and NULL are left as written, so SQLite stores each value with the same
# type as the dump (whole numbers as integers, decimals as reals).
sqlite_strings <- function(x) {
  if (!grepl("\\", x, fixed = TRUE)) return(x)
  x <- gsub("\\\\", "\001", x, fixed = TRUE)     # escaped backslash, put back last
  x <- gsub("\\'", "''", x, fixed = TRUE)        # escaped quote: doubled for SQLite
  x <- gsub("\\n", "\n", x, fixed = TRUE)
  x <- gsub("\\r", "\r", x, fixed = TRUE)
  x <- gsub("\\t", "\t", x, fixed = TRUE)
  x <- gsub("\\0", "", x, fixed = TRUE)
  x <- gsub("\\Z", "\032", x, fixed = TRUE)
  x <- gsub("\\\\(.)", "\\1", x, perl = TRUE)    # any other escaped character stands for itself
  gsub("\001", "\\", x, fixed = TRUE)
}

# ---- 3. load into SQLite ------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
quote_id <- function(x) paste0("`", x, "`")
for (t in names(tables)) {
  DBI::dbExecute(con, sprintf("CREATE TABLE %s (%s)", quote_id(t), paste(quote_id(tables[[t]]), collapse = ", ")))
}
for (i in grep("^INSERT INTO `\\w+` VALUES ", lines)) {
  DBI::dbExecute(con, sqlite_strings(sub(";$", "", lines[i])))
}
cat("Raw table row counts:\n")
for (t in names(tables)) {
  cat(sprintf("  %s: %d\n", t, DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", quote_id(t)))$n))
}

# ---- 4. views from the dump's "Final view" definitions ----------------------
vl <- lines[startsWith(lines, "/*!50001 VIEW")]
v <- regmatches(vl, regexec("^/\\*!50001 VIEW `(\\w+)` AS (select .*) \\*/;$", vl))
v <- Filter(length, v)
view_sql <- setNames(vapply(v, `[`, "", 3), vapply(v, `[`, "", 2))
# SQLite divides integers as integers; MySQL's `/` is decimal
view_sql["v_cuisine_mix"] <- gsub("count(0) / sum(", "count(0) * 1.0 / sum(", view_sql["v_cuisine_mix"], fixed = TRUE)
# in dependency order: subzone population before the tender dashboard
for (vw in c("v_cuisine_mix", "v_subzone_population_2026", "v_tender_dashboard")) {
  DBI::dbExecute(con, sprintf("CREATE VIEW %s AS %s", quote_id(vw), view_sql[[vw]]))
}

# ---- 5. the app's queries -------------------------------------------------------
# SQL_CENTRES, SQL_VACANCY ... are read from global.R, so the CSVs always come from
# the same queries as the app's database mode (only those assignments are run)
app_sql <- new.env()
for (e in parse("global.R", encoding = "UTF-8")) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && startsWith(as.character(e[[2]]), "SQL_")) {
    eval(e, app_sql)
  }
}
need <- c("SQL_MRT_ACCESS", "SQL_ENGAGEMENT", "SQL_VACANCY", "SQL_CENTRES", "SQL_PA_POPULATION")
if (!all(need %in% ls(app_sql))) stop("global.R no longer defines ", paste(setdiff(need, ls(app_sql)), collapse = ", "))
exports <- list(
  "mrt_access.csv"         = app_sql$SQL_MRT_ACCESS,
  "cuisine_engagement.csv" = app_sql$SQL_ENGAGEMENT,
  "vacancy.csv"            = app_sql$SQL_VACANCY,
  "centres.csv"            = app_sql$SQL_CENTRES,
  # the other tables, as read_from_db() in global.R reads them
  "tenders.csv"            = "SELECT * FROM v_tender_dashboard",
  "cuisine_mix.csv"        = "SELECT * FROM v_cuisine_mix",
  "land_use.csv"           = "SELECT centre_id, buffer_radius_m, land_use_category, land_use_share FROM centre_land_use_metrics",
  "mrt_exits.csv"          = "SELECT station_name, exit_code, latitude, longitude, rail_type FROM mrt_exits",
  "pa_population.csv"      = app_sql$SQL_PA_POPULATION
)

# ---- 6. CSV writing ---------------------------------------------------------------
# Values are written as the earlier Python exporter wrote them, so a rebuild changes
# nothing that the data did not change: whole numbers as they are, decimals with
# the fewest digits that read back to the same number (".0" on whole ones, an
# exponent below 1e-4), empty for NULL, quotes only where a field needs them.
py_float <- function(x) {
  out <- character(length(x))
  todo <- seq_along(x)
  for (k in 1:17) {
    s <- sprintf("%.*e", k - 1L, x[todo])
    # read back by SQLite, which reads decimals exactly (R's as.numeric() can be one step off)
    hit <- DBI::dbGetQuery(con, "SELECT CAST(? AS REAL) = ? AS ok", params = list(s, x[todo]))$ok == 1
    out[todo[hit]] <- s[hit]
    todo <- todo[!hit]
    if (!length(todo)) break
  }
  neg <- startsWith(out, "-")
  dig <- gsub("[^0-9]", "", sub("e.*$", "", out))      # significant digits
  ex  <- as.integer(sub("^.*e", "", out))              # power of ten of the first digit
  n   <- nchar(dig)
  sci <- ex < -4 | ex >= 16
  plain <- ifelse(ex >= 0,
                  paste0(substr(paste0(dig, strrep("0", pmax(0, ex + 1 - n))), 1, ex + 1), ".",
                         ifelse(n > ex + 1, substr(dig, ex + 2, n), "0")),
                  paste0("0.", strrep("0", pmax(0, -ex - 1)), dig))
  expo <- paste0(substr(dig, 1, 1), ifelse(n > 1, paste0(".", substr(dig, 2, n)), ""),
                 "e", ifelse(ex < 0, "-", "+"), formatC(abs(ex), width = 2, flag = "0"))
  paste0(ifelse(neg, "-", ""), ifelse(sci, expo, plain))
}
csv_field <- function(v) {
  q <- grepl("[\",\r\n]", v)
  v[q] <- paste0("\"", gsub("\"", "\"\"", v[q], fixed = TRUE), "\"")
  v
}
# Each value with its SQLite type: reals are formatted here, everything else is SQLite's own text
export_csv <- function(sql, path) {
  cols <- names(DBI::dbGetQuery(con, paste0("SELECT * FROM (", sql, ") LIMIT 0")))
  sel <- unlist(lapply(seq_along(cols), function(i) {
    col <- quote_id(cols[i])
    c(sprintf("typeof(%s) AS t%d", col, i),
      sprintf("CASE WHEN typeof(%s) = 'real' THEN %s END AS r%d", col, col, i),
      sprintf("CASE WHEN typeof(%s) <> 'real' THEN CAST(%s AS TEXT) END AS s%d", col, col, i))
  }))
  d <- DBI::dbGetQuery(con, paste0("SELECT ", paste(sel, collapse = ", "), " FROM (", sql, ")"))
  vals <- lapply(seq_along(cols), function(i) {
    type <- d[[3 * i - 2]]
    v <- ifelse(type == "null", "", as.character(d[[3 * i]]))
    real <- type == "real"
    v[real] <- py_float(as.numeric(d[[3 * i - 1]][real]))
    csv_field(v)
  })
  rows <- if (nrow(d)) do.call(paste, c(vals, sep = ",")) else character(0)
  fh <- file(path, open = "wb")                        # "\n" line ends on every system
  writeLines(enc2utf8(c(paste(csv_field(cols), collapse = ","), rows)), fh, sep = "\n", useBytes = TRUE)
  close(fh)
  c(nrow(d), length(cols))
}

cat("App CSV row counts:\n")
for (f in names(exports)) {
  n <- export_csv(exports[[f]], file.path(out, f))
  cat(sprintf("  %s: %d rows, %d cols\n", f, n[1], n[2]))
}

# Full copies of every table of the dump, for the team and for later use (only with --raw)
if (raw_copies) {
  dir.create(file.path(out, "raw"), showWarnings = FALSE)
  for (t in names(tables)) export_csv(paste("SELECT * FROM", quote_id(t)), file.path(out, "raw", paste0(t, ".csv")))
  cat("Raw tables written to", file.path(out, "raw"), "\n")
}
DBI::dbDisconnect(con)
cat("Done.\n")
