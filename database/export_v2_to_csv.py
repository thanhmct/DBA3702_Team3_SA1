"""Load the MySQL dump (v3; v2 and v2-1 also work) into SQLite in memory, run the dump's
view SQL, and export the tables and views the Shiny app reads as CSV (copy them into data/).

Usage: python -I database/export_v2_to_csv.py <dump.sql> <out_dir>
"""
import csv
import os
import re
import sqlite3
import sys

DUMP, OUT = sys.argv[1], sys.argv[2]
os.makedirs(OUT, exist_ok=True)

with open(DUMP, encoding="utf-8") as fh:
    text = fh.read()
lines = text.splitlines()

# ---- 1. column names from CREATE TABLE blocks -------------------------------
tables = {}
for m in re.finditer(r"CREATE TABLE `(\w+)` \((.*?)\n\) ENGINE", text, re.S):
    name, body = m.group(1), m.group(2)
    cols = []
    for line in body.splitlines():
        cm = re.match(r"\s*`(\w+)`\s", line)
        if cm:
            cols.append(cm.group(1))
    tables[name] = cols

# ---- 2. INSERT rows: character scanner (handles quotes, '' and backslash) ----
ESC = {"n": "\n", "r": "\r", "t": "\t", "0": "", "Z": "\x1a"}


def parse_values(s):
    out = []
    i, n = 0, len(s)
    while i < n:
        if s[i] != "(":
            i += 1
            continue
        i += 1
        row = []
        while True:
            while s[i] == " ":
                i += 1
            if s[i] == "'":
                j, buf = i + 1, []
                while True:
                    ch = s[j]
                    if ch == "\\":
                        nxt = s[j + 1]
                        buf.append(ESC.get(nxt, nxt))
                        j += 2
                    elif ch == "'":
                        if j + 1 < n and s[j + 1] == "'":
                            buf.append("'")
                            j += 2
                        else:
                            break
                    else:
                        buf.append(ch)
                        j += 1
                row.append("".join(buf))
                i = j + 1
            else:
                j = i
                while s[j] not in ",)":
                    j += 1
                tok = s[i:j].strip()
                if tok == "NULL":
                    row.append(None)
                elif any(x in tok for x in ".eE"):
                    row.append(float(tok))
                else:
                    row.append(int(tok))
                i = j
            while s[i] == " ":
                i += 1
            if s[i] == ",":
                i += 1
                continue
            if s[i] == ")":
                i += 1
                break
            raise ValueError(f"unexpected char {s[i]!r} at {i}")
        out.append(row)
    return out


rows = {t: [] for t in tables}
for line in lines:
    m = re.match(r"INSERT INTO `(\w+)` VALUES ", line)
    if not m:
        continue
    t = m.group(1)
    for vals in parse_values(line[m.end():].rstrip(";")):
        assert len(vals) == len(tables[t]), (t, len(vals), len(tables[t]))
        rows[t].append(vals)

# ---- 3. load into SQLite -----------------------------------------------------
con = sqlite3.connect(":memory:")
for t, cols in tables.items():
    con.execute(f"CREATE TABLE `{t}` ({', '.join('`' + c + '`' for c in cols)})")
    con.executemany(
        f"INSERT INTO `{t}` VALUES ({', '.join('?' * len(cols))})", rows[t])

print("Raw table row counts:")
for t in tables:
    print(f"  {t}: {len(rows[t])}")

# ---- 4. views from the dump's "Final view" definitions -----------------------
view_sql = {}
for line in lines:
    m = re.match(r"/\*!50001 VIEW `(\w+)` AS (select .*) \*/;$", line)
    if m:
        view_sql[m.group(1)] = m.group(2)

# SQLite does integer division: MySQL's `/` is decimal, so make it real.
view_sql["v_cuisine_mix"] = view_sql["v_cuisine_mix"].replace(
    "count(0) / sum(", "count(0) * 1.0 / sum(")

# Create in dependency order: subzone population before tender dashboard
for v in ["v_cuisine_mix", "v_subzone_population_2026", "v_tender_dashboard"]:
    con.execute(f"CREATE VIEW `{v}` AS {view_sql[v]}")

# ---- 5. app queries (same SQL as global.R) ----------------------------------
SQL_CENTRES = """
  SELECT h.centre_id, h.centre_name, h.address, h.postal_code,
         h.latitude, h.longitude, h.nea_status,
         h.number_of_cooked_food_stalls AS stalls,
         s.subzone_name, s.planning_area_name, s.region_name,
         p.resident_population, p.hdb_population,
         m.nearest_exit_distance_m, m.exits_within_500m, m.stations_within_500m,
         e.station_name AS nearest_mrt_station
  FROM hawker_centres h
  LEFT JOIN subzones s                    ON s.subzone_id = h.subzone_id
  LEFT JOIN v_subzone_population_2026 p   ON p.subzone_id = h.subzone_id
  LEFT JOIN centre_mrt_access m           ON m.centre_id  = h.centre_id
  LEFT JOIN mrt_exits e                   ON e.exit_id    = m.nearest_exit_id"""

SQL_PA_POPULATION = """
  SELECT s.planning_area_name, s.region_name,
         SUM(p.resident_population) AS resident_population,
         SUM(p.hdb_population)      AS hdb_population
  FROM subzones s
  LEFT JOIN v_subzone_population_2026 p ON p.subzone_id = s.subzone_id
  GROUP BY s.planning_area_name, s.region_name"""

# Same query as SQL_VACANCY in global.R
SQL_VACANCY = """
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
  GROUP BY centre_id"""


# Google Maps review engagement per planning area and cuisine group (v3), for V4, V6, V7
SQL_ENGAGEMENT = """
  SELECT p.planning_area_name, e.cuisine_group, e.restaurant_count,
         e.total_reviews, e.avg_reviews, e.engagement_share
  FROM cuisine_engagement_by_planning_area e
  JOIN planning_areas p ON p.planning_area_id = e.planning_area_id"""

# V8: nearest exit, its station's average daily tap-outs (latest month, weekday + weekend/holiday
# totals / 30, as in Lucien's V9_2026_EXTRACTION.sql) and subzone residents, for every centre
SQL_MRT_ACCESS = """
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
  ORDER BY h.centre_id"""

EXPORTS = {
    "mrt_access.csv": SQL_MRT_ACCESS,
    "cuisine_engagement.csv": SQL_ENGAGEMENT,
    "vacancy.csv": SQL_VACANCY,
    "centres.csv": SQL_CENTRES,
    "tenders.csv": "SELECT * FROM v_tender_dashboard",
    "cuisine_mix.csv": "SELECT * FROM v_cuisine_mix",
    "land_use.csv": ("SELECT centre_id, buffer_radius_m, land_use_category, land_use_share "
                     "FROM centre_land_use_metrics"),
    "mrt_exits.csv": "SELECT station_name, exit_code, latitude, longitude, rail_type FROM mrt_exits",
    "pa_population.csv": SQL_PA_POPULATION,
}

print("App CSV row counts:")
for fname, sql in EXPORTS.items():
    cur = con.execute(sql)
    header = [d[0] for d in cur.description]
    data = cur.fetchall()
    with open(os.path.join(OUT, fname), "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(header)
        for r in data:
            w.writerow(["" if v is None else v for v in r])
    print(f"  {fname}: {len(data)} rows, {len(header)} cols")

# Full copies of every raw table, for the team and for later use
raw_dir = os.path.join(OUT, "raw")
os.makedirs(raw_dir, exist_ok=True)
for t, cols in tables.items():
    with open(os.path.join(raw_dir, t + ".csv"), "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(cols)
        for r in rows[t]:
            w.writerow(["" if v is None else v for v in r])
print("Done.")
