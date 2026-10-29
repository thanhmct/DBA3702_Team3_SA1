# Research plan — SA1 Team 03 (hawker centres)

Status: revised after team discussion (Oct 2026). "KIV" = keep in view, not yet selected.

## 1. Chosen audience & decisions
- **Audience:** prospective entrepreneur looking to rent a cooked-food stall at an NEA market / hawker centre.
- **What they already know:** available stalls (NEA tender), rental costs (NEA tender), available cuisine (Hawker Guide).
- **Insight they lack:** how cuisine preferences vary across location and age groups, and how these compare to the current cuisine offerings.
- **Decisions:** which hawker centre to operate at? What cuisine to sell?
- **Information gap:** no integrated platform lets prospective hawkers compare existing cuisine supply with estimated local cuisine demand based on surrounding population demographics.

## 2. Research objective
To recommend an optimal hawker centre location and cuisine type for a prospective entrepreneur bidding in the upcoming NEA Hawker tender, aiming to capture an underserved customer base within their maximum rental budget.

> Open: the wording "(better target the scope and remove the method)" is still to be settled. Known conflict: demand data is by **planning area** (synthetic survey), not the "surrounding" population.

## 3. Research questions
| RQ | Question | Why it matters | Selected | Owner |
|---|---|---|---|---|
| RQ1 | How does existing cuisine supply vary across hawker centres? | Understand the existing competition | Yes | Ridhwan |
| RQ2 | How does estimated cuisine demand vary across locations and age groups? | Identify cuisines that may appeal to the surrounding population | KIV | Ridhwan |
| RQ3 | Which hawker centres are over-saturated or under-represented for specific cuisines relative to their local population? | Identify the best hawker centres for different cuisines | KIV | Ridhwan |
| RQ4 | Which hawker centres offer the strongest combination of local residential density and public transit accessibility? | Potential customer base + accessibility | Yes | Lucien |
| RQ5 | Which centres exhibit high tenancy risk through repeated un-bid tender listings? | Tenancy risk | Yes | Casper |
| RQ6 | How do AMR and baseline operating cost compare across centres, and which fit the entrepreneur's budget? | Answers the "maximum rental budget" scope | Yes | Zhen Rong |

## 4. Visualisation plan
| RQ | V | Visual | Selected | Owner | Status in app |
|---|---|---|---|---|---|
| RQ1 | V1 | Interactive cuisine composition map | Yes (combined with V2) | Thanh Mai + Zhen Rong | Built: the map in the Map section (tooltip shows cuisine mix and vacant stalls) |
| RQ6 | V2 | Leaflet map with budget slider (add/remove constraints) | Yes (combined with V1) | Thanh Mai + Zhen Rong | Built as filters beside the map: max rent (budget) and min. vacant stalls |
| RQ1 | V3 | Cuisine supply at a chosen centre (ranked bar) | Yes | Ridhwan | Built live in the Data visualization section from `cuisine_mix` (9 database groups); says why when a centre has no stall data; script in `charts/team_charts.R` (section 1) |
| RQ2 | V4 | Cuisine engagement map (choose a cuisine, planning areas shaded by share of Google Maps reviews) | Yes (Google Maps review engagement replaces the synthetic survey) | Ridhwan | Built live in Data visualization; its cuisine follows the map filter and can be changed; also a shading option on the map; script in `charts/team_charts.R` (section 2) |
| RQ2 | V5 | Age-grouped cuisine preference (grouped bar) | Dropped | Ridhwan | Removed from the app (no survey data) |
| RQ3 | V6 | Diverging bar: Google Maps review share minus stall share at a chosen centre | Yes (Google Maps engagement) | Ridhwan | Built live in Data visualization with a centre search box; script in `charts/team_charts.R` (section 2) |
| RQ3 | V7 | Scatter: engagement–supply gap vs rent, one dot per centre | Yes (Google Maps engagement) | Ridhwan | Built live in Data visualization with a cuisine dropdown; script in `charts/team_charts.R` (section 2) |
| RQ4 | V8 | MRT exit distance vs station activity (bubble = subzone population) | Yes | Lucien | Built live in Data visualization from the database (`SQL_MRT_ACCESS`), with a fact box for the chosen centre; no ranking or score; script in `charts/team_charts.R` (section 3) |
| RQ5 | V9 | Share of assessed stalls not returning to tender at the chosen centre | Yes | Casper | Built live as a gauge against the median centre (bands below 33% / 70% and above); grey message for the 101 centres that cannot be assessed; script in `charts/team_charts.R` (section 4) |
| RQ5 | V10 | Tender timeline heatmap (exercise_month × centre) | Dropped | Casper | Removed from the app (only about a quarter of centre-month cells have a listing) |
| — | V11 | Detail panel for first chosen centre + comparison table | Yes | Thanh | Built: centre card beside the map (details) + "Compare your shortlist" section (comparison table: add via Compare button or double-click on the map, drag to reorder, sort, remove, CSV) |

## 5. Data requirements
| V | Data / variables | Sources | Missing data / issue |
|---|---|---|---|
| V1–V2 | Centre name, coordinates, centre_id, cuisine category, occupied stalls per cuisine, AMR | NEA hawker centre GeoJSON, Hawker Guide, tender notices | 22 of 129 centres have no cuisine data; AMR only for 69 centres |
| V3 | Centre_id, cuisine category, number of stalls, supply share | Hawker Guide (`v_cuisine_mix`) | Nine main-meal groups from the v3 database (drinks, desserts and snacks not counted) |
| V4, V6, V7 | Planning area, cuisine group, Google Maps review share, stall share, rent | `cuisine_engagement_by_planning_area` (v3), Hawker Guide, tender notices | Review share is online attention, not demand; areas with fewer than 30 classified places are left out |
| V8 | Centre ID, name, subzone, residential population, MRT distance and station | NEA centres, URA subzones, SingStat 2026, LTA MRT/LRT exits | Population is subzone-level, not walking distance; MRT distance is straight-line |
| V9 | stall_id, exercise_month | NEA tender notices (`tender_opportunities`) | Only 28 centres have enough assessed stalls |
| V11 | Latest listing, monthly cost (rent + fees), MRT, residents, cuisine stalls | Tender notices, MRT data, SingStat, Hawker Guide | Fees are ranges; AMR is a reference rent, not the awarded bid |

## 6. Open decisions for the team
1. RQ2 and RQ3 now use Google Maps review engagement (V4, V6, V7) instead of the survey; V5 is dropped.
2. Whether the "estimated monthly cost" (V11) is kept, given the cost section was removed earlier.
3. Numbering: the Data visualization rows use this plan's numbers (V3, V4, V6–V9; V5 and V10 dropped).
