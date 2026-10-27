-- Run after selecting the project database containing the v2 tables.
SELECT h.centre_id, h.centre_name, me.station_name AS nearest_station_name,
 a.nearest_exit_distance_m AS straight_line_distance_m,
 p.resident_population AS subzone_population_2026,
 r.station_codes AS pt_code, r.avg_daily_tap_outs, r.avg_daily_movements,
 CASE WHEN me.station_name IS NULL THEN 'NO_NEAREST_STATION'
 WHEN r.station_id IS NULL THEN 'UNMATCHED_STATION_OR_NO_RIDERSHIP'
 WHEN p.resident_population IS NULL THEN 'MISSING_POPULATION'
 ELSE 'MATCHED' END AS match_status
FROM hawker_centres h
LEFT JOIN centre_mrt_access a ON a.centre_id = h.centre_id
LEFT JOIN mrt_exits me ON me.exit_id = a.nearest_exit_id
LEFT JOIN v_subzone_population_2026 p ON p.subzone_id = h.subzone_id
LEFT JOIN (SELECT station_id,
 GROUP_CONCAT(DISTINCT source_pt_code ORDER BY source_pt_code SEPARATOR ', ') AS station_codes,
 ROUND(SUM(period_tap_outs)/30, 1) AS avg_daily_tap_outs,
 ROUND(SUM(total_passenger_movements)/30, 1) AS avg_daily_movements
 FROM station_ridership WHERE reference_month = '2026-09-01' GROUP BY station_id
) r ON r.station_id = me.station_id
ORDER BY h.centre_id;
