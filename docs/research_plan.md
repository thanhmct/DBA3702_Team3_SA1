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
| RQ1 | V1 | Interactive cuisine composition map | Yes (combined with V2) | Thanh Mai + Zhen Rong | Built: the one map on Tab 1 Explore (tooltip shows cuisine mix and vacant stalls) |
| RQ6 | V2 | Leaflet map with budget slider (add/remove constraints) | Yes (combined with V1) | Thanh Mai + Zhen Rong | Built as filters on the Explore map: max rent (budget) and min. vacant stalls |
| RQ1 | V3 | Cuisine supply comparison (stacked bar) | Yes | Ridhwan | Empty placeholder in Tab 2 (earlier chart code kept in server.R) |
| RQ2 | V4 | Cuisine demand heatmap | KIV (synthetic) | Ridhwan | Empty placeholder in Tab 2; code kept, needs survey data |
| RQ2 | V5 | Age-grouped cuisine preference (grouped bar) | KIV (synthetic) | Ridhwan | Empty placeholder in Tab 2; code kept, needs survey + age-group standardisation |
| RQ3 | V6 | Diverging bar: demand share vs supply share, top n centres | KIV | Ridhwan | Empty placeholder in Tab 2; code kept, needs survey data |
| RQ3 | V7 | Scatter: demand–supply gap vs rent | KIV (is market rent the right x-axis?) | Ridhwan | Empty placeholder in Tab 2; code kept, needs survey data |
| RQ4 | V8 | Quadrant chart: MRT walking distance vs subzone population (bubble = population) | Yes | Lucien | Empty placeholder in Tab 2 |
| RQ5 | V9 | Relisting frequency ranked bar (distinct stall_id vs listings, by release_category) | Yes | Casper | Empty placeholder in Tab 2; data ready in `tenders` |
| RQ5 | V10 | Tender timeline heatmap (exercise_month × centre) | KIV (depends on V9) | Casper | Empty placeholder in Tab 2; data ready in `tenders` |
| — | V11 | Detail panel for first chosen centre + comparison table | Yes | Thanh | Built: Tab 3 summary card and comparison table |

## 5. Data requirements
| V | Data / variables | Sources | Missing data / issue |
|---|---|---|---|
| V1–V2 | Centre name, coordinates, centre_id, cuisine category, occupied stalls per cuisine, AMR | NEA hawker centre GeoJSON, Hawker Guide, tender notices | 22 of 129 centres have no cuisine data; AMR only for 69 centres |
| V3 | Centre_id, cuisine category, number of stalls, supply share | Hawker Guide (`cuisine_mix`) | 35 labels grouped into 11 cuisine groups |
| V4–V7 | Planning area, age group, cuisine category, demand share | Synthetic cuisine demand + SingStat June 2026 + URA MP2019 | Survey not yet delivered; age groups must be standardised; SingStat 2026 age data is in the v2 database |
| V8 | Centre ID, name, subzone, residential population, MRT distance and station | NEA centres, URA subzones, SingStat 2026, LTA MRT/LRT exits | Population is subzone-level, not walking distance; MRT distance is straight-line |
| V9–V10 | stall_id, listing_id, release_category, exercise_month | NEA tender notices (`tender_opportunities`) | Only 69 centres have listings |
| V11 | Latest listing, monthly cost (rent + fees), MRT, residents, cuisine stalls | Tender notices, MRT data, SingStat, Hawker Guide | Fees are ranges; AMR is a reference rent, not the awarded bid |

## 6. Open decisions for the team
1. RQ2, RQ3 and V4–V7 are KIV until the survey is delivered.
2. Whether RQ4 and RQ5 (Lucien's MRT and residents questions) stay in the final app.
3. Whether the "estimated monthly cost" (V11) is kept, given the cost section was removed earlier.
4. Numbering: resolved — the app's Tab 2 cards use this plan's V3–V10 numbers.
