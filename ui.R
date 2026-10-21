# =============================================================================
# Hawker Site Finder — ui.R
# Layout: 2 tabs (Explore, Cuisine Gap), each with its own sidebar of filters.
# Controls that change only one chart sit in that chart's card header.
# =============================================================================

# Helpers tag_note(), rq_header(), hdr_input(), reset_button() are defined in global.R

# Styles from the Figma wireframe: light green header with pill tabs, soft green
# gradient page, white cards with a soft shadow (no borders), IBM Plex type
app_css <- "
body { background: linear-gradient(180deg, #E9F3E0 0%, #F4F9EF 40%, #FFFFFF 100%); background-attachment: fixed; }
.navbar { background: #E8F2DE !important; border-bottom: 2px solid #0D743B; }
.app-title { font-weight: 600; font-size: 1.45rem; line-height: 1.2; color: #1E2B22; }
.app-question { font-size: 0.8rem; color: #404041; }
.navbar .nav-link { color: #404041 !important; border-radius: 8px; padding: 0.45rem 0.9rem !important; margin-left: 4px; }
.navbar .nav-link.active { background: #0D743B; color: #FFFFFF !important; }
.navbar .navbar-nav { justify-content: flex-end; }
.card, .bslib-value-box { border: none !important; border-radius: 8px; background: #FFFFFF;
  box-shadow: 0 6px 16px rgba(13, 116, 59, 0.10), 0 2px 4px rgba(64, 64, 65, 0.08) !important; }
/* Expanded (full screen) map card: the map fills the card */
.bslib-card[data-full-screen='true'] #map { height: calc(100vh - 190px) !important; }
/* Centre table: header in the app's light green with a dark green rule */
table.dataTable thead th { background: #E8F2DE !important; color: #0D743B; font-weight: 600; border-bottom: 2px solid #0D743B !important; }
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
.placeholder-body { height: 300px; background: #F6F8F9; border-radius: 6px; }
.legend-dot { width: 12px; height: 12px; border-radius: 50%; background: #FAA619; flex: none; }
.legend-exit { width: 7px; height: 7px; border-radius: 50%; background: #4C83C2; margin: 0 2.5px; flex: none; }
.centre-card-box { border-radius: 8px; padding: 12px 14px; background: #FFFFFF;
  box-shadow: 0 4px 14px rgba(13, 116, 59, 0.16), 0 1px 3px rgba(64, 64, 65, 0.12); }
.map-legend-below > div { display: flex; flex-wrap: wrap; align-items: center; gap: 4px 18px; margin-top: 10px; }
.map-legend-below .info-title { width: 100%; margin: 0; }
.map-legend-below .legend-row { margin-bottom: 0; }
"

# When the map card is expanded or collapsed, resize the map and fit Singapore
app_js <- "
$(function () {
  var fit = function () {
    var w = HTMLWidgets.find('#map');
    var m = w && w.getMap();
    if (!m) return;
    m.invalidateSize();
    m.fitBounds([[1.205, 103.60], [1.475, 104.05]]);
  };
  new MutationObserver(function () { setTimeout(fit, 300); })
    .observe(document.body, { attributes: true, subtree: true, attributeFilter: ['data-full-screen'] });
});
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
  header = tagList(tags$style(HTML(app_css)), tags$script(HTML(app_js))),

  # ---------------------------------------------------------------------------
  # Tab 1: Explore Map
  # ---------------------------------------------------------------------------
  nav_panel(
    "1 Explore",
    layout_sidebar(
      fillable = FALSE, border = FALSE,
      sidebar = sidebar(
        title = "Filters", width = 340,
        # Filters grouped by type: dropdowns, then sliders, then checkboxes
        selectInput("region", "Region", choices = c("All regions" = "All", named_choices(REGIONS))),
        selectInput("pa", "Planning area", choices = c("All planning areas" = "All", named_choices(PAS))),
        selectInput("cuisine", "Centres that sell", choices = c("Any cuisine" = "All", CUISINE_GROUPS)),
        selectInput("map_metric", "Shade planning areas by",
                    choices = c("Number of centres" = "n",
                                "Centres per 10 km²" = "density",
                                "Cooked-food stalls" = "stalls",
                                "Residents" = "residents")),
        tags$hr(class = "my-1"),
        sliderInput("max_amr", "Max median rent (AMR, S$)", min = AMR_RANGE[1], max = AMR_RANGE[2],
                    value = AMR_RANGE[2], step = 50, pre = "S$"),
        sliderInput("max_mrt", "Max distance to MRT exit (m)", min = 0, max = MRT_MAX,
                    value = MRT_MAX, step = 100),
        sliderInput("min_vacant", "Min. vacant stalls (Hawker Guide)", min = 0, max = VAC_MAX,
                    value = 0, step = 1),
        tags$hr(class = "my-1"),
        checkboxInput("only_tenders", "Only centres with tender listings", FALSE),
        checkboxInput("map_exits", "Show MRT/LRT exits", FALSE),
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
          # Map with its legend in one row right below it
          div(leafletOutput("map", height = 580),
              div(class = "map-legend-below", uiOutput("map_legend"))),
          # Panel beside the map: the centre card in its own bordered box
          div(class = "map-info",
              div(class = "centre-card-box",
                  div(class = "info-title mt-0", "Centre card"),
                  uiOutput("centre_card"),
                  plotlyOutput("centre_cuisine", height = 200)),
              tag_note(class = "mt-2", "Hover a circle: name, planning area, stalls, tender listings, top cuisines, vacant stalls and rent. Click a circle or a table row to fill the centre card. Shading, MRT exits, budget (max rent) and vacant-stall filters are in the sidebar. Data: NEA hawker centres via team SQL database."))
        )
      ),
      card(
        card_header(class = "d-flex align-items-center",
                    tags$strong("Centre table"),
                    div(class = "ms-auto",
                        downloadButton("centre_csv", "CSV", icon = icon("file-csv"),
                                       class = "btn-sm btn-outline-success"))),
        DTOutput("centre_table")
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
  )
)
