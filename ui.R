# =============================================================================
# Hawker Site Finder — ui.R
# Layout: 3 tabs, each with its own sidebar of filters (no shared filters).
# Controls that change only one chart sit in that chart's card header.
# =============================================================================

# Helpers tag_note(), rq_header(), hdr_input(), reset_button() are defined in global.R

ui <- page_navbar(
  title = "Hawker Site Finder",
  id = "tabs",
  theme = bs_theme(
    version = 5,
    # Colour template: FMI / Oliver Wyman "Imperative Issues 2022" report
    primary = BRAND$green_dark,
    secondary = BRAND$orange,
    success = BRAND$green,
    info = BRAND$blue,
    fg = BRAND$text,
    bg = "#FFFFFF",
    "navbar-bg" = BRAND$green_dark
  ),
  fillable = FALSE,
  header = tags$style(HTML(".bslib-value-box .value-box-value { font-weight: 700; }")),

  # ---------------------------------------------------------------------------
  # Tab 1: Explore Map
  # ---------------------------------------------------------------------------
  nav_panel(
    "1 Explore",
    layout_sidebar(
      fillable = FALSE, border = FALSE,
      sidebar = sidebar(
        title = "Filters", width = 300,
        selectInput("region", "Region", choices = c("All regions" = "All", named_choices(REGIONS))),
        selectInput("pa", "Planning area", choices = c("All planning areas" = "All", named_choices(PAS))),
        selectInput("cuisine", "Centres that sell", choices = c("Any cuisine" = "All", CUISINE_GROUPS)),
        sliderInput("max_amr", "Max median rent (AMR, S$)", min = AMR_RANGE[1], max = AMR_RANGE[2],
                    value = AMR_RANGE[2], step = 50, pre = "S$"),
        sliderInput("max_mrt", "Max distance to MRT exit (m)", min = 0, max = MRT_MAX,
                    value = MRT_MAX, step = 100),
        checkboxInput("only_tenders", "Only centres with tender listings", FALSE),
        uiOutput("t1_count"),
        reset_button("reset_t1"),
        tag_note("Filters on this tab only. A region limits the planning areas; a planning area sets its region. Cuisines and slider ranges follow the chosen area.")
      ),
      layout_columns(
        fill = FALSE,
        value_box("Hawker centres", textOutput("vb_centres"), theme = "primary"),
        value_box("Tender listings", textOutput("vb_listings"), textOutput("vb_listings_sub")),
        value_box("Median centre rent", textOutput("vb_rent"), "per month (AMR)"),
        value_box("Avg distance to MRT exit", textOutput("vb_mrt"), "straight line")
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex flex-wrap gap-3 align-items-center",
          tags$strong("Map of hawker centres: location, density and residents"),
          div(class = "ms-auto",
              radioButtons("map_metric", NULL, inline = TRUE,
                           choices = c("Number of centres" = "n",
                                       "Centres per 10 km²" = "density",
                                       "Cooked-food stalls" = "stalls",
                                       "Residents " = "residents")),
              checkboxInput("map_exits", "Show MRT/LRT exits", FALSE))
        ),
        leafletOutput("map", height = 560),
        card_footer(tag_note("Circle size = cooked-food stalls. Dark line = Singapore border. Click a circle to see the centre card below."))
      ),
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header(tags$strong("Centre card")),
          uiOutput("centre_card"),
          plotlyOutput("centre_cuisine", height = 220),
          actionButton("add_compare", "Add to compare →", class = "btn-outline-dark")
        ),
        card(
          card_header(tags$strong("Centre table")),
          DTOutput("centre_table")
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # Tab 2: Cuisine Gap
  # ---------------------------------------------------------------------------
  nav_panel(
    "2 Cuisine Gap",
    layout_sidebar(
      fillable = FALSE, border = FALSE,
      sidebar = sidebar(
        title = "Filters", width = 300,
        selectInput("cg_cuisine", "Cuisine to analyse", choices = CUISINE_GROUPS, selected = "Western"),
        selectInput("cg_centre", "Hawker centre", choices = NULL),
        tag_note("Or click a centre on the map or a dot in the gap vs rent chart."),
        tags$hr(),
        tags$strong(class = "small", "Map constraints (V1+V2)"),
        sliderInput("cg_budget", "Budget: max median rent (S$)", min = AMR_RANGE[1],
                    max = AMR_RANGE[2], value = AMR_RANGE[2], step = 50, pre = "S$"),
        sliderInput("cg_min_vacant", "Min. vacant stalls (Hawker Guide)", min = 0, max = VAC_MAX,
                    value = 0, step = 1),
        reset_button("reset_t2"),
        uiOutput("survey_status")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE,
             rq_header("RQ1 · V1+V2", "Cuisine composition map with budget and vacancy constraints",
                       hdr_input(selectInput("cg_map_mode", NULL,
                                             choices = c("Colour: share of chosen cuisine" = "share",
                                                         "Colour: main cuisine of centre" = "dominant")))),
             leafletOutput("cg_map", height = 460),
             card_footer(tag_note("Coloured = fits your budget and vacancy constraints. Grey = above budget, no rent data, or too few vacant stalls. Hover for cuisine mix and vacant stalls; click a centre to see its mix in the charts below."))),
        card(full_screen = TRUE,
             rq_header("RQ1 · V2", "Cuisine supply at the selected centre"),
             plotlyOutput("cg_supply_centre", height = 460),
             card_footer(tag_note("100% stacked bars: the centre vs its planning area vs all of Singapore.")))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             rq_header("RQ2 · V3", "Cuisine demand heatmap by planning area"),
             leafletOutput("cg_demand_map", height = 400),
             card_footer(tag_note("Estimated share of residents who prefer the chosen cuisine (synthetic survey)."))),
        card(full_screen = TRUE,
             rq_header("RQ2 · V4", "Cuisine preference by age group",
                       hdr_input(selectInput("cg_pa", NULL,
                                             choices = c("All planning areas" = "All", named_choices(PAS))))),
             plotlyOutput("cg_age_plot", height = 400),
             card_footer(tag_note("Grouped bars: each age group adds up to 100%. Pick a planning area in the card header.")))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             rq_header("RQ3 · V5", "Demand vs supply share at the selected centre"),
             plotlyOutput("cg_diverging", height = 400),
             card_footer(tag_note("Right = under-supplied (more demand than stalls). Left = over-supplied."))),
        card(full_screen = TRUE,
             rq_header("RQ3 · V6", "Demand–supply gap vs rent"),
             plotlyOutput("cg_gap_rent", height = 400),
             card_footer(tag_note("One dot per centre for the chosen cuisine. Bottom-right = big unmet demand and low rent. Click a dot to select the centre.")))
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # Tab 3: Compare Centres
  # ---------------------------------------------------------------------------
  nav_panel(
    "3 Compare Centres",
    layout_sidebar(
      fillable = FALSE, border = FALSE,
      sidebar = sidebar(
        title = "Filters", width = 300,
        tags$strong(class = "small", "My stall profile"),
        selectInput("s_cuisine", "I want to sell", choices = CUISINE_GROUPS, selected = "Western"),
        selectizeInput("s_pa", "Planning area(s)", choices = named_choices(PAS_TENDER), multiple = TRUE,
                       options = list(placeholder = "All planning areas")),
        selectizeInput("s_station", "Distance from MRT station(s)", choices = STATIONS, multiple = TRUE,
                       options = list(placeholder = "Nearest MRT exit (any station)")),
        sliderInput("s_mrt_max", "Max distance to chosen MRT (m)", min = 0, max = MRT_MAX,
                    value = MRT_MAX, step = 50),
        sliderInput("s_max_amr", "Max median rent (AMR, S$)", min = AMR_RANGE[1], max = AMR_RANGE[2],
                    value = AMR_RANGE[2], step = 50, pre = "S$"),
        sliderInput("s_pop", "Residents nearby (subzone)", min = POP_RANGE[1], max = POP_RANGE[2],
                    value = POP_RANGE, step = 1000, sep = ","),
        sliderInput("s_comp", "Competitors: stalls of this cuisine",
                    min = 0, max = COMP_MAX, value = c(0, COMP_MAX), step = 1),
        checkboxInput("s_keep_na", "Keep centres with no cuisine data", TRUE),
        reset_button("reset_t3"),
        tag_note("Filters on this tab only. Only planning areas with a tender listing are offered; the sliders span what exists in the chosen area(s).")
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(full_screen = TRUE,
             card_header(uiOutput("s_map_title")),
             leafletOutput("s_map", height = 460)),
        card(card_header(tags$strong("Summary for the first chosen centre")),
             uiOutput("s_summary"))
      ),
      card(full_screen = TRUE,
           card_header(class = "d-flex flex-wrap gap-2 align-items-center",
                       tags$strong("Comparison table (no limit)"),
                       div(class = "ms-auto", tags$span(class = "badge bg-light text-dark", "Drag rows to reorder"))),
           selectizeInput("s_compare", NULL, choices = NULL, multiple = TRUE, width = "100%",
                          options = list(placeholder = "Add any number of centres", plugins = list("remove_button"))),
           DTOutput("s_compare_table"),
           card_footer(tag_note("Any number of centres · remove one with × in the box above · drag rows to reorder (first row = the summary beside the map) · click a column header to sort · Download CSV")))
    )
  )
)
