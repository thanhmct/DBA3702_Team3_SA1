DBA3702 Team 03 - Lucien V9 (2026 population)

Files
- V9_2026_GRAPH.png: graph generated in RStudio.
- V9_2026_graph.R: R script to reproduce the graph and ranking.
- hawker_data_2026.csv: exported SQL results, 129 centres.
- V9_2026_RANKING.csv: ranking independently calculated from the export using the same score.
- V9_2026_EXTRACTION.sql: query against the team v2 database.

Reproduce
1. Import the team database v2 into MySQL and select it as the default schema.
2. Run V9_2026_EXTRACTION.sql and export the result as hawker_data_2026.csv. An export is included.
3. Open V9_2026_graph.R in RStudio and click Source. Select the CSV when prompted. The script needs ggplot2 and patchwork and saves the graph and ranking alongside the CSV.

Method and limitations
127 of 129 centres included. Amoy Street and Maxwell have missing population and are excluded. Each component is min-max scaled across eligible centres; the score gives equal weight to distance (closer scores higher), station tap-outs and subzone resident population. Distance is straight-line, not walking distance. MRT tap-outs are station-level, not actual hawker visitors. Population is 2026; station activity is September 2026, divided by 30 days. Ranking is a relative indicator, not a forecast of demand or profitability.
The LTA source link still needs verification for the data-source reference. The full team SQL database is included as SA1_Team03_Project_hawker_data_v2-1.sql. Import it into the hawker_project schema before running the extraction query.

Original LTA reference files are in LTA_reference_originale: original September 2026 ZIP and station summary, station-code mapping, and the old V8 import script. They are retained for traceability. The v2 database already contains station_ridership, so the old import script is not needed to run V9 and should not be run against the v2 database. V8 population, graph, ranking, R code and SQL view have been replaced by the V9 versions.
