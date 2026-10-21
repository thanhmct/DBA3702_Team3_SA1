"""Load the v2 MySQL dump into SQLite (in memory), run the dump's view SQL,
and export the tables/views the Shiny app reads as CSV.

Usage: python -I build_v2.py <dump.sql> <out_dir>
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

# ---- 5. app queries (same as global.R, with v2 names) ----------------------
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

EXPORTS = {
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
