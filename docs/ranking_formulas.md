# HawkerRadar: "Find Your Best Fit" ranking formulas

How the Centre table's **Find Your Best Fit** tab scores each hawker centre. The code is in `global.R` (`rank_access`, `budget_score`, `cuisine_score_for`) and `server.R` (`rank_data`).

## 1. Overall score

The overall score has three components. Each one is scored out of 10, and they carry equal weights:

$$
\text{Overall} = \frac{\text{Accessibility} + \text{Budget} + \text{Cuisine}}{3}
$$

Because the weights are equal, each component adds at most 3.33 points to the overall score. That is why the bars in the chart are drawn as score ÷ 3.

A centre is ranked **only if all three components can be computed**, which means it needs rent, MRT and cuisine data. A centre with missing data is left out; it is never given 0. In the current data, 66 of 129 centres can be scored.

## 2. Accessibility (0–10)

Accessibility is the average of a distance score and a station-activity score:

$$
\text{Accessibility} = \frac{S_{\text{distance}} + S_{\text{activity}}}{2}
$$

**Distance score.** It uses the straight-line distance $d$ (in metres) from the centre to its nearest MRT/LRT exit. The cap is 1,000 m, about a 12-minute walk:

$$
S_{\text{distance}} = 10 \times \max\left(0,\; 1 - \frac{d}{1000}\right)
$$

An exit at 0 m scores 10, one at 500 m scores 5, and anything at 1,000 m or more scores 0.

**Station-activity score.** It uses $T$, the average daily tap-outs at that exit's station: the September 2026 monthly tap-outs, weekday plus weekend/holiday, divided by 30. The score is min–max scaled on a log scale:

$$
S_{\text{activity}} = 10 \times \frac{\ln(T+1) - \ln(T_{\min}+1)}{\ln(T_{\max}+1) - \ln(T_{\min}+1)}
$$

Here $T_{\min}$ and $T_{\max}$ are the quietest and busiest stations among all centres (about 1,809 and 69,309 tap-outs a day). The log scale keeps a few very busy interchanges from pushing every other station towards 0.

## 3. Budget (0–10)

$A$ is the centre's median assessed market rent (AMR), and $B$ is the user's budget. The **default budget is S$1,110**, the median AMR across Singapore's hawker centres. The user can type their own in the Budget box.

$$
\text{gap} =
\begin{cases}
\dfrac{B - A}{B} & \text{if } A \le B \quad \text{(under budget)} \\[2ex]
2 \times \dfrac{A - B}{B} & \text{if } A > B \quad \text{(over budget)}
\end{cases}
$$

$$
\text{Budget} = 10 \times \max(0,\; 1 - \text{gap})
$$

- A rent exactly on budget scores 10.
- Going **over** the budget costs **twice** as much as being under it. With a S$1,200 budget, a rent of S$1,100 scores 9.2 but a rent of S$1,300 scores 8.3.
- The score reaches 0 when rent is 50% above the budget, or when it is 100% below it (a rent of 0).

Being far under budget is not a perfect 10 either. The score rewards a close fit to the budget the user plans for, not the cheapest centre.

## 4. Cuisine opportunity (0–10)

The score is built from the **engagement–supply gap** for a cuisine $c$ at a centre:

$$
\text{gap}_c = \underbrace{\text{engagement share}_c}_{\text{\% of Google Maps reviews in the planning area}} - \underbrace{\text{supply share}_c}_{\text{\% of occupied main-meal stalls at the centre}}
$$

The gap is measured in percentage points. Planning areas with fewer than 30 classified food establishments are left out.

The gap is then scaled around 5:

$$
\text{Cuisine} = 5 + 5 \times \frac{\text{gap}_c}{G}
$$

- No gap scores 5. A positive gap (more online engagement than hawker supply) scores above 5, and a negative gap scores below 5.
- **A specific cuisine chosen:** $G$ is the largest absolute gap for that cuisine across all centres. The centre with the biggest gap for that cuisine therefore scores 10 (or 0, if that biggest gap is negative).
- **"Any cuisine (best fit)":** each centre is scored on its own best cuisine, the one with its largest positive gap. $G$ is the largest absolute gap across **all** cuisines (about 81 points). A small cuisine's few-point gap therefore cannot look as big as a large one's.

## 5. Which centres are ranked

- **Area = "Nearest to the selected centre"** (default): the selected centre plus its **5 nearest scored centres**. Distance is the straight line (haversine) between the centres' coordinates.
- **Area = a planning area:** every scored centre in that area.
- Rows are sorted by overall score, highest first.

The ranking uses its own Centre, Area, Cuisine and Budget boxes. It does **not** apply the map's rent and MRT sliders.

## 6. Worked example: Buffalo Road Blk 665 and its 5 nearest centres

These scores use the default settings: budget S$1,110 and Any cuisine.

| Rank | Centre | km away | MRT exit (m) | Daily tap-outs | Access | Rent (S$) | Budget | Best cuisine (gap, pts) | Cuisine | **Overall** |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | Buffalo Road Blk 665 | 0.0 | 100 | 29,462 | 8.33 | 1,250 | 7.48 | Western / European (+13.0) | 5.80 | **7.2** |
| 2 | Golden Mile Food Centre | 1.5 | 331 | 5,020 | 4.74 | 1,055 | 9.50 | Indian / South Asian (+15.3) | 5.95 | **6.7** |
| 3 | North Bridge Road Market | 1.5 | 195 | 18,468 | 7.21 | 600 | 5.41 | Western / European (+23.5) | 6.45 | **6.4** |
| 4 | Berseh Food Centre | 0.7 | 277 | 11,086 | 6.10 | 550 | 4.95 | Indian / South Asian (+45.6) | 7.81 | **6.3** |
| 5 | Bendemeer Road Blk 29 | 2.0 | 124 | 22,127 | 7.81 | 1,460 | 3.69 | Western / European (+23.5) | 6.45 | **6.0** |
| 6 | Newton Food Centre | 1.4 | 151 | 54,953 | 8.92 | 2,350 | 0.00 | Western / European (+20.6) | 6.27 | **5.1** |

**Buffalo Road, step by step:**

- Distance score:
$$
10 \times \left(1 - \frac{100}{1000}\right) = 9.00
$$
- Activity score: 7.65, so
$$
\text{Accessibility} = \frac{9.00 + 7.65}{2} = 8.33
$$
- Budget: the rent is S$140 over the S$1,110 budget, so
$$
\text{gap} = 2 \times \frac{140}{1110} = 0.252, \qquad \text{Budget} = 10 \times (1 - 0.252) = 7.48
$$
- Cuisine: its best gap is Western / European at +13.0 points, so
$$
\text{Cuisine} = 5 + 5 \times \frac{13.0}{81.0} = 5.80
$$
- Overall:
$$
\frac{8.33 + 7.48 + 5.80}{3} = 7.20
$$

## 7. Caveats

- AMR is NEA's reference rent, not the winning tender bid.
- MRT distance is a straight line, not a walking route.
- Station tap-outs measure station traffic, not hawker-centre visitors.
- Google Maps review share is online engagement, not measured demand.
- The ranking is a **screening aid** to build a shortlist, not a sales forecast.
