# =============================================================================
# Hawker Site Finder — ui.R
# Layout: 3 tabs, each with its own sidebar of filters (no shared filters).
# Controls that change only one chart sit in that chart's card header.
# =============================================================================

# Helpers tag_note(), rq_header(), hdr_input(), reset_button() are defined in global.R

# Styles from the Figma wireframe: light green header with pill tabs, soft green
# gradient page, white cards with a slate border, IBM Plex type
app_css <- "
body { background: linear-gradient(180deg, #E9F3E0 0%, #F4F9EF 40%, #FFFFFF 100%); background-attachment: fixed; }
.navbar { background: #E8F2DE !important; border-bottom: 2px solid #0D743B; }
.app-title { font-weight: 600; font-size: 1.45rem; line-height: 1.2; color: #1E2B22; }
.app-question { font-size: 0.8rem; color: #404041; }
.navbar .nav-link { color: #404041 !important; border-radius: 8px; padding: 0.45rem 0.9rem !important; margin-left: 4px; }
.navbar .nav-link.active { background: #0D743B; color: #FFFFFF !important; }
.navbar .navbar-nav { justify-content: flex-end; }
.card, .bslib-value-box { border: 1.5px solid #4E5D68 !important; border-radius: 8px; box-shadow: none; background: #FFFFFF; }
.card-header, .card-footer { background: #FFFFFF; border: none; }
.bslib-value-box .value-box-title { font-size: 0.85rem; color: #404041; }
.bslib-value-box .value-box-value { font-weight: 700; font-size: 1.9rem; color: #1E2B22; }
.vb-sub { font-size: 0.8rem; color: #404041; margin-bottom: 0; }
.mono, .tag-mono { font-family: 'IBM Plex Mono', monospace; }
.tag-mono { font-size: 0.72rem; padding: 2px 8px; border-radius: 4px; }
.tag-green { background: #DDECC1; color: #0D743B; }
.tag-grey { background: #EDF0F2; color: #4E5D68; }
.leaflet-container { background: #EDF0F2; border-radius: 6px; }
.map-info .info-title { font-weight: 600; font-size: 0.85rem; margin: 0.75rem 0 0.5rem; }
.legend-row { display: flex; align-items: center; gap: 8px; font-size: 0.82rem; margin-bottom: 6px; }
.legend-swatch { width: 18px; height: 12px; border-radius: 3px; border: 1px solid #C8CDD2; flex: none; }
.placeholder-body { height: 300px; background: #F6F8F9; border: 1px dashed #C8CDD2; border-radius: 6px; }
.legend-dot { width: 12px; height: 12px; border-radius: 50%; background: #FAA619; flex: none; }
.legend-exit { width: 7px; height: 7px; border-radius: 50%; background: #4C83C2; margin: 0 2.5px; flex: none; }
.top-row { display: flex; justify-content: space-between; font-size: 0.82rem; margin-bottom: 6px; }
.top-row .num { font-family: 'IBM Plex Mono', monospace; }
"

# Empty card that reserves space for a visual, labelled with its V number
placeholder_card <- function(code, title) {
  card(card_header(class = "d-flex gap-2 align-items-center",
                   tags$span(class = "badge bg-primary", code), tags$strong(title)),
       div(class = "placeholder-body"))
}

# Month label that does not depend on the computer's language, e.g. "Oct 25"
month_label <- function(d) paste(month.abb[as.integer(format(d, "%m"))], format(d, "%y"))

ui <- page_navbar(
  title = div(div(class = "app-title", "Hawker Site Finder"),
              div(class = "app-question", "Where should a new hawker open, and what should they sell?")),
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
    "navbar-bg" = "#E8F2DE",
    base_font = font_google("IBM Plex Sans", local = FALSE),
    code_font = font_google("IBM Plex Mono", local = FALSE)
  ),
  fillable = FALSE,
  header = tags$style(HTML(app_css)),

  # ---------------------------------------------------------------------------
  # Tab 1: Explore Map
  # ---------------------------------------------------------------------------
  nav_panel(
    "1 Explore",
    layout_sidebar(
      fillable = FALSE, border = FALSE,
      sidebar = sidebar(
        title = "Filters", width = 340,
        selectInput("region", "Region", choices = c("All regions" = "All", named_choices(REGIONS))),
        selectInput("pa", "Planning area", choices = c("All planning areas" = "All", named_choices(PAS))),
        selectInput("cuisine", "Centres that sell", choices = c("Any cuisine" = "All", CUISINE_GROUPS)),
        sliderInput("max_amr", "Max median rent (AMR, S$)", min = AMR_RANGE[1], max = AMR_RANGE[2],
                    value = AMR_RANGE[2], step = 50, pre = "S$"),
        sliderInput("max_mrt", "Max distance to MRT exit (m)", min = 0, max = MRT_MAX,
                    value = MRT_MAX, step = 100),
        sliderInput("min_vacant", "Min. vacant stalls (Hawker Guide)", min = 0, max = VAC_MAX,
                    value = 0, step = 1),
        checkboxInput("only_tenders", "Only centres with tender listings", FALSE),
        uiOutput("t1_count"),
        reset_button("reset_t1"),
        tag_note("Filters on this tab only. A region limits the planning areas; a planning area sets its region. Cuisines and slider ranges follow the chosen area.")
      ),
      layout_columns(
        fill = FALSE,
        value_box("Hawker centres", textOutput("vb_centres"),
                  tags$p(class = "vb-sub mono", "hawker_centres")),
        value_box(paste0("Tender listings (", month_label(MONTHS[1]), " – ", month_label(MONTHS[2]), ")"),
                  textOutput("vb_listings"),
                  tags$p(class = "vb-sub", textOutput("vb_listings_sub", inline = TRUE))),
        value_box("Median centre rent", textOutput("vb_rent"), tags$p(class = "vb-sub", "per month (AMR)")),
        value_box("Avg distance to MRT exit", textOutput("vb_mrt"), tags$p(class = "vb-sub", "straight line"))
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex flex-wrap gap-2 align-items-center",
          tags$strong("Map of hawker centres: location, density and residents"),
          div(class = "ms-auto d-flex gap-2",
              tags$span(class = "tag-mono tag-green", "leaflet"),
              tags$span(class = "tag-mono tag-grey", "hawker_centres"))
        ),
        layout_columns(
          col_widths = c(9, 3), fill = FALSE,
          leafletOutput("map", height = 580),
          # Information panel beside the map (Figma "Legend & details")
          div(class = "map-info",
              selectInput("map_metric", "Shade planning areas by", width = "100%",
                          choices = c("Number of centres" = "n",
                                      "Centres per 10 km²" = "density",
                                      "Cooked-food stalls" = "stalls",
                                      "Residents" = "residents")),
              checkboxInput("map_exits", "Show MRT/LRT exits", FALSE),
              uiOutput("map_legend"),
              uiOutput("map_top"),
              tag_note("Hover a circle: name, planning area, stalls, tender listings, top cuisines, vacant stalls and rent. Click a circle to see the centre card below. Budget (max rent) and vacant-stall filters are in the sidebar. Data: NEA hawker centres via team SQL database."))
        )
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
        title = "Filters", width = 340,
        selectInput("cg_cuisine", "Cuisine to analyse", choices = CUISINE_GROUPS, selected = "Western"),
        selectInput("cg_centre", "Hawker centre", choices = NULL),
        tag_note("Or click a centre on the Explore map."),
        reset_button("reset_t2"),
        uiOutput("survey_status")
      ),
      # Empty, named spaces for the teammates' visuals (V1+V2 is the Explore map)
      layout_column_wrap(
        width = 1/2, fill = FALSE,
        placeholder_card("V3", "Cuisine supply comparison"),
        placeholder_card("V4", "Cuisine demand heatmap"),
        placeholder_card("V5", "Age-grouped cuisine preference"),
        placeholder_card("V6", "Demand vs supply share"),
        placeholder_card("V7", "Demand–supply gap vs rent"),
        placeholder_card("V8", "MRT access vs residents (quadrant)"),
        placeholder_card("V9", "Relisting frequency"),
        placeholder_card("V10", "Tender timeline heatmap")
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
        title = "Filters", width = 340,
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
