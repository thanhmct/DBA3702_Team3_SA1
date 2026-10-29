# Research plan — SA1 Team 03 (hawker centres)

Status: revised Oct 2026 to match the app as built on the v3 database. "KIV" = keep in view, not yet selected.
Changes since the last version are tagged **[updated]**, **[new]** or **[dropped]**.

## 1. Chosen audience & decisions
- **Audience:** prospective entrepreneur looking to rent a cooked-food stall at an NEA market / hawker centre.
- **What they already know:** available stalls (NEA tender), rental costs (NEA tender), available cuisine (Hawker Guide).
- **Insight they lack:** how interest in each cuisine varies across locations, and how it compares to the current cuisine
  offerings. **[updated]** Interest is measured by Google Maps review engagement; the age-group angle is dropped (no survey data).
- **Decisions:** which hawker centre to operate at? What cuisine to sell?
- **Information gap:** no integrated platform lets prospective hawkers compare existing cuisine supply with local cuisine
  interest, rent, transit access and tender history in one place. **[updated]** (was "estimated demand based on surrounding
  population demographics")

## 2. Research objective
To recommend an optimal hawker centre location and cuisine type for a prospective entrepreneur bidding in the upcoming NEA Hawker tender, aiming to capture an underserved customer base within their maximum rental budget.

> Open: the wording "(better target the scope and remove the method)" is still to be settled. **[updated]** Known limitation:
> cuisine interest is the share of Google Maps reviews by **planning area**, which is online attention, not measured demand,
> and not the population within walking distance of a centre.

## 3. Research questions
| RQ | Question | Why it matters | Selected | Owner |
|---|---|---|---|---|
| RQ1 | How does existing cuisine supply vary across hawker centres? | Understand the existing competition | Yes | Ridhwan |
| RQ2 | How does cuisine engagement (share of Google Maps reviews) vary across planning areas? **[updated]** (was "estimated cuisine demand across locations and age groups") | Identify cuisines that may appeal to the surrounding population | Yes **[updated]** (was KIV) | Ridhwan |
| RQ3 | Which hawker centres are over-saturated or under-represented for specific cuisines relative to local engagement? **[updated]** (was "relative to their local population") | Identify the best hawker centres for different cuisines | Yes **[updated]** (was KIV) | Ridhwan |
| RQ4 | Which hawker centres offer the strongest combination of local residential density and public transit accessibility? | Potential customer base + accessibility | Yes | Lucien |
| RQ5 | Which centres exhibit high tenancy risk through repeated un-bid tender listings? | Tenancy risk | Yes | Casper |
| RQ6 | How do AMR and baseline operating cost compare across centres, and which fit the entrepreneur's budget? | Answers the "maximum rental budget" scope | Yes | Zhen Rong |

## 4. Visualisation plan
| RQ | V | Visual | Selected | Owner | Status in app |
|---|---|---|---|---|---|
| RQ1 | V1 | Interactive cuisine composition map | Yes (combined with V2) | Thanh Mai + Zhen Rong | Built: the map in the Map section. Centres as circles (size = cooked-food stalls); click opens the centre card (cuisine mix, key numbers). **[updated]** Planning areas shaded by number of centres, centres per 10 km², stalls, residents or **[new]** share of Google Maps reviews (for the cuisine in "Centres that sell"; with "Any cuisine", the top cuisine's share, named on hover). **[new]** Two-way sync with the charts below |
| RQ6 | V2 | Leaflet map with budget slider (add/remove constraints) | Yes (combined with V1) | Thanh Mai + Zhen Rong | Built as filters beside the map: region, planning area, **[new]** "Centres that sell" (cuisine), median rent range (budget), **[new]** distance to MRT exit, min. vacant stalls, **[new]** "Only centres with tender listings" / "Only centres with vacant stalls", MRT/LRT exits |
| RQ1 | V3 | Cuisine supply at a chosen centre (ranked bar) | Yes | Ridhwan | Built live in the Data visualization section from `cuisine_mix` (9 database groups); says why when a centre has no stall data; script in `charts/team_charts.R` (section 1) |
| RQ2 | V4 | Cuisine engagement map (choose a cuisine, planning areas shaded by share of Google Maps reviews) | Yes (Google Maps review engagement replaces the synthetic survey) | Ridhwan | Built live in Data visualization; its cuisine follows the map filter and can be changed; also a shading option on the map; script in `charts/team_charts.R` (section 2) |
| RQ2 | V5 | Age-grouped cuisine preference (grouped bar) | **[dropped]** | Ridhwan | Removed from the app (no survey data) |
| RQ3 | V6 | Diverging bar: Google Maps review share minus stall share at a chosen centre | Yes (Google Maps engagement) | Ridhwan | Built live in Data visualization with a centre search box; script in `charts/team_charts.R` (section 2) |
| RQ3 | V7 | Scatter: engagement–supply gap vs rent, one dot per centre | Yes (Google Maps engagement) | Ridhwan | Built live in Data visualization with a cuisine dropdown (follows "Centres that sell"); script in `charts/team_charts.R` (section 2) |
| RQ4 | V8 | MRT exit distance vs station activity (bubble = subzone population) | Yes | Lucien | Built live in Data visualization from the database (`SQL_MRT_ACCESS`), with a fact box for the chosen centre against the median centre; no ranking or score **[updated]** (the original equal-weight ranking is dropped); script in `charts/team_charts.R` (section 3) |
| RQ5 | V9 | Share of assessed stalls not returning to tender at the chosen centre **[updated]** (was a relisting-frequency ranked bar) | Yes | Casper | Built live as a gauge against the median centre (bands below 33% / 70% and above); grey message for the 101 centres that cannot be assessed; script in `charts/team_charts.R` (section 4) |
| RQ5 | V10 | Tender timeline heatmap (exercise_month × centre) | **[dropped]** | Casper | Removed from the app (only about a quarter of centre-month cells have a listing) |
| — | V11 | Detail panel for first chosen centre + comparison table | Yes | Thanh | Built: centre card on the map + "Compare your shortlist" section. **[updated]** Add with **Add to comparison** (centre card or any chart; a second click removes) or double-click on the map. Columns: median rent, **[new]** Open to you? (stalls your cuisine may bid on), **[new]** vacant stalls, MRT distance (station), residents, latest listing; with a cuisine chosen, **[new]** that cuisine's stalls and engagement–supply gap. Drag to reorder, sort, remove, CSV with every column we hold per centre. **[new]** Quick starts for an empty shortlist (5 best centres by one criterion). **[dropped]** Estimated monthly cost column |

## 5. Data requirements
All data now comes from the **v3 database** (`SA1_Team03_Project_hawker_data_v3.sql`), or its CSV copy in `data/` when MySQL is not running. **[updated]**

| V | Data / variables | Sources | Missing data / issue |
|---|---|---|---|
| V1–V2 | Centre name, coordinates, centre_id, cuisine category, occupied stalls per cuisine, AMR, **[new]** vacant stalls, MRT exit distance, review share by planning area | NEA hawker centre GeoJSON, Hawker Guide, tender notices, **[new]** LTA MRT/LRT exits, Google Maps reviews | 22 of 129 centres have no cuisine data; AMR only for 69 centres |
| V3 | Centre_id, cuisine category, number of stalls, supply share | Hawker Guide (`v_cuisine_mix`) | Nine main-meal groups from the v3 database (drinks, desserts and snacks not counted) |
| V4, V6, V7 | Planning area, cuisine group, Google Maps review share, stall share, rent | `cuisine_engagement_by_planning_area` (v3), Hawker Guide, tender notices | Review share is online attention, not demand; areas with fewer than 30 classified places are left out |
| V8 | Centre ID, name, subzone, residential population, MRT distance and station, **[new]** station tap-outs | NEA centres, URA subzones, SingStat 2026, LTA MRT/LRT exits, **[new]** LTA passenger volume (Sep 2026) | Population is subzone-level, not walking distance; MRT distance is straight-line |
| V9 | stall_id, exercise_month | NEA tender notices (`tender_opportunities`) | Only 28 centres have enough assessed stalls |
| V11 | Latest listing, MRT, residents, cuisine stalls, **[new]** vacant stalls, trades open to bid, review share **[updated]** (monthly cost dropped) | Tender notices, MRT data, SingStat, Hawker Guide, Google Maps reviews | AMR is a reference rent, not the awarded bid |

## 6. Open decisions for the team
1. ~~RQ2 and RQ3 use Google Maps review engagement instead of the survey~~ — **decided:** V4, V6, V7 use Google Maps engagement; V5 is dropped.
2. ~~Whether the "estimated monthly cost" (V11) is kept~~ — **decided:** dropped; the comparison table shows median rent only.
3. ~~Numbering~~ — **decided:** the Data visualization rows use this plan's numbers (V3, V4, V6–V9; V5 and V10 dropped), as do the sections of `charts/team_charts.R`.
4. **[new]** Final wording of the research objective (section 2).
5. ~~Whether to keep the old database dumps (v1, v2-1) and `database/V8_Lucien/`~~ — **decided:** removed; the app uses the v3 database only.
