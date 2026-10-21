# =============================================================================
# Hawker Site Finder — ui.R
# One scrolling page in the style of McKinsey Global Institute's "2025 in
# charts": themed sections with a serif heading, an insight sentence, and
# exhibits (takeaway headline, "Metric, unit" subtitle, chart, source line).
# One sidebar holds every filter; jump links in the header replace the tabs.
# =============================================================================

# Helpers tag_note(), rq_header(), hdr_input(), reset_button() are defined in global.R

# Styles: brand colours (FMI template, Figma) + McKinsey-style typography,
# white cards with a soft shadow (no borders), IBM Plex Sans / Serif / Mono
app_css <- "
html { scroll-behavior: smooth; }
body { background: linear-gradient(180deg, #E9F3E0 0%, #F4F9EF 40%, #FFFFFF 100%); background-attachment: fixed; }

/* Header bar with jump links (replaces the tabs) */
.app-header { position: sticky; top: 0; z-index: 1000; display: flex; align-items: center; justify-content: space-between;
  gap: 16px; background: #E8F2DE; border-bottom: 2px solid #0D743B; padding: 10px 24px; margin: 0 -12px 18px; }
.app-title { font-weight: 600; font-size: 1.45rem; line-height: 1.2; color: #1E2B22; }
.app-question { font-size: 0.8rem; color: #404041; }
.jump-links a { color: #404041; text-decoration: none; border-radius: 8px; padding: 0.45rem 0.9rem; margin-left: 4px; font-size: 0.95rem; }
.jump-links a:hover { background: #DDECC1; }
.jump-links a.active { background: #0D743B; color: #FFFFFF; }

/* Boxes: no borders, soft outer shadow */
.card, .bslib-value-box { border: none !important; border-radius: 8px; background: #FFFFFF;
  box-shadow: 0 6px 16px rgba(13, 116, 59, 0.10), 0 2px 4px rgba(64, 64, 65, 0.08) !important; }
.card-header, .card-footer { background: #FFFFFF; border: none; }
.bslib-value-box .value-box-title { font-size: 0.85rem; color: #404041; }
.bslib-value-box .value-box-value { font-family: 'IBM Plex Serif', Georgia, serif; font-weight: 600; font-size: 2rem; color: #1E2B22; }
.vb-sub { font-size: 0.8rem; color: #404041; margin-bottom: 0; }
.mono { font-family: 'IBM Plex Mono', monospace; }

/* McKinsey-style sections */
.mck-section { scroll-margin-top: 90px; margin-bottom: 2.75rem; }
.sec-title { font-family: 'IBM Plex Serif', Georgia, serif; font-size: 2.25rem; font-weight: 500; color: #1E2B22; margin: 0.25rem 0 0.2rem; }
.sec-kicker { font-size: 1rem; color: #58595B; margin-bottom: 0.9rem; }
.sec-insight { font-size: 1.15rem; line-height: 1.6; color: #333333; max-width: 1000px; margin-bottom: 1.25rem; }

/* McKinsey-style exhibits: label, takeaway headline, subtitle, chart, source */
.exhibit > .card-body { padding: 22px 26px; }
.ex-label { font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase; color: #0D743B; font-weight: 600; margin-bottom: 6px; }
.ex-headline { font-size: 1.35rem; font-weight: 600; color: #1E2B22; line-height: 1.3; margin-bottom: 4px; }
.ex-subtitle { font-size: 0.95rem; color: #404041; margin-bottom: 14px; }
.ex-subtitle b { color: #1E2B22; }
.ex-source { font-size: 0.75rem; font-style: italic; color: #808285; margin-top: 12px; }
.ex-placeholder .ex-headline { color: #808285; }
.placeholder-body { height: 300px; background: #F6F8F9; border-radius: 6px; }

/* Map */
.leaflet-container { background: #EDF0F2; border-radius: 6px; }
.bslib-card[data-full-screen='true'] #map { height: calc(100vh - 360px) !important; }
.map-info .info-title, .map-legend-below .info-title { font-weight: 600; font-size: 0.85rem; }
.legend-row { display: flex; align-items: center; gap: 8px; font-size: 0.82rem; margin-bottom: 6px; }
.legend-swatch { width: 18px; height: 12px; border-radius: 3px; border: 1px solid #C8CDD2; flex: none; }
.legend-dot { width: 12px; height: 12px; border-radius: 50%; background: #FAA619; flex: none; }
.legend-exit { width: 7px; height: 7px; border-radius: 50%; background: #4C83C2; margin: 0 2.5px; flex: none; }
.map-legend-below > div { display: flex; flex-wrap: wrap; align-items: center; gap: 4px 18px; margin-top: 10px; }
.map-legend-below .info-title { width: 100%; margin: 0; }
.map-legend-below .legend-row { margin-bottom: 0; }
.centre-card-box { border-radius: 8px; padding: 12px 14px; background: #FFFFFF;
  box-shadow: 0 4px 14px rgba(13, 116, 59, 0.16), 0 1px 3px rgba(64, 64, 65, 0.12); }
.centre-card-box .info-title { margin: 0 0 0.5rem; }

/* McKinsey-style tooltips and callout on the map */
.leaflet-tooltip.mck-tt { background: #FFFFFF; border: none; border-radius: 6px; padding: 10px 12px;
  box-shadow: 0 6px 18px rgba(30, 43, 34, 0.18); font-family: 'IBM Plex Sans', sans-serif; color: #404041; }
.leaflet-tooltip.mck-tt::before { display: none; }
.tt-title { font-weight: 600; color: #1E2B22; font-size: 0.9rem; }
.tt-big { font-family: 'IBM Plex Serif', Georgia, serif; font-size: 1.7rem; color: #0D743B; line-height: 1.1; margin-top: 2px; }
.tt-sub { font-size: 0.78rem; color: #58595B; }
.tt-meta { font-size: 0.78rem; color: #404041; margin-top: 6px; padding-top: 6px; border-top: 1px solid #EDF0F2; }
.leaflet-tooltip.mck-annot { background: transparent; border: none; box-shadow: none; text-align: center; }
.annot-big { font-family: 'IBM Plex Serif', Georgia, serif; font-size: 1.9rem; color: #1E2B22; line-height: 1;
  text-shadow: 0 0 3px #FFFFFF, 0 0 6px #FFFFFF; }
.annot-sub { font-size: 0.8rem; font-weight: 600; color: #1E2B22; text-shadow: 0 0 3px #FFFFFF, 0 0 6px #FFFFFF; }

/* Centre table: header in the app's light green with a dark green rule */
table.dataTable thead th { background: #E8F2DE !important; color: #0D743B; font-weight: 600; border-bottom: 2px solid #0D743B !important; }

/* Sections fade up as they scroll into view. Only armed once the script has
   checked the browser supports it (html.js-reveal), so content is never hidden
   if the script does not run. */
.js-reveal .reveal { opacity: 0; transform: translateY(24px); transition: opacity 0.7s ease, transform 0.7s ease; }
.js-reveal .reveal.is-visible { opacity: 1; transform: none; }
@media (prefers-reduced-motion: reduce) {
  html { scroll-behavior: auto; }
  .js-reveal .reveal { opacity: 1; transform: none; transition: none; }
}
"

app_js <- "
$(function () {
  // Expanding or collapsing the map card: resize the map and fit Singapore
  var fit = function () {
    var w = HTMLWidgets.find('#map');
    var m = w && w.getMap();
    if (!m) return;
    m.invalidateSize();
    m.fitBounds([[1.205, 103.60], [1.475, 104.05]]);
  };
  new MutationObserver(function () { setTimeout(fit, 300); })
    .observe(document.body, { attributes: true, subtree: true, attributeFilter: ['data-full-screen'] });

  if (!('IntersectionObserver' in window)) return;  // old browser: no effects, all visible

  // Sections fade up as they scroll into view
  document.documentElement.classList.add('js-reveal');
  var reveal = new IntersectionObserver(function (entries) {
    entries.forEach(function (e) {
      if (e.isIntersecting) { e.target.classList.add('is-visible'); reveal.unobserve(e.target); }
    });
  }, { threshold: 0.08 });
  document.querySelectorAll('.reveal').forEach(function (el) { reveal.observe(el); });

  // Jump links: highlight the section in view
  var links = document.querySelectorAll('.jump-links a');
  var spy = new IntersectionObserver(function (entries) {
    entries.forEach(function (e) {
      if (!e.isIntersecting) return;
      links.forEach(function (a) { a.classList.toggle('active', a.getAttribute('href') === '#' + e.target.id); });
    });
  }, { rootMargin: '-45% 0px -50% 0px' });
  document.querySelectorAll('.mck-section').forEach(function (s) { spy.observe(s); });
});
"

# McKinsey-style exhibit frame. With no chart given it is an empty, named space
# for a teammate's visual (V3-V10); put their output in `...` when it is ready.
exhibit <- function(code, headline, subtitle, source, ...) {
  content <- list(...)
  card(fill = FALSE, class = paste("exhibit", if (!length(content)) "ex-placeholder"),
       div(class = "ex-label", paste("Exhibit", code)),
       div(class = "ex-headline", headline),
       div(class = "ex-subtitle", HTML(subtitle)),
       if (length(content)) content else div(class = "placeholder-body"),
       div(class = "ex-source", source))
}

# Section with a serif heading and a one-line kicker
mck_section <- function(id, title, kicker, ..., reveal = TRUE) {
  tags$section(id = id, class = paste("mck-section", if (reveal) "reveal"),
               h2(class = "sec-title", title),
               p(class = "sec-kicker", kicker),
               ...)
}

# Month label that does not depend on the computer's language, e.g. "Oct 25"
month_label <- function(d) paste(month.abb[as.integer(format(d, "%m"))], format(d, "%y"))

TENDER_SOURCE <- paste0("Source: NEA tender notices, ", month_label(MONTHS[1]), "–", month_label(MONTHS[2]))

ui <- page_fluid(
  title = "Hawker Site Finder",
  theme = bs_theme(
    version = 5,
    # Colour template: FMI / Oliver Wyman "Imperative Issues 2022" report
    primary = BRAND$green_dark,
    secondary = BRAND$orange,
    success = BRAND$green,
    info = BRAND$blue,
    fg = BRAND$text,
    bg = "#FFFFFF",
    base_font = font_google("IBM Plex Sans", local = FALSE),
    heading_font = font_google("IBM Plex Serif", local = FALSE),
    code_font = font_google("IBM Plex Mono", local = FALSE)
  ),
  tags$head(tags$style(HTML(app_css)), tags$script(HTML(app_js))),

  # ---------------------------------------------------------------------------
  # Header: title, question and jump links to the sections
  # ---------------------------------------------------------------------------
  div(class = "app-header",
      div(div(class = "app-title", "Hawker Site Finder"),
          div(class = "app-question", "Where should a new hawker open, and what should they sell?")),
      tags$nav(class = "jump-links",
               tags$a(href = "#sec-map", class = "active", "Map"),
               tags$a(href = "#sec-cuisine", "Cuisine supply & demand"),
               tags$a(href = "#sec-access", "Access & residents"),
               tags$a(href = "#sec-tenancy", "Tenancy risk"))),

  layout_sidebar(
    fillable = FALSE, border = FALSE,
    # One sidebar for the whole page, filters grouped by type
    sidebar = sidebar(
      title = "Filters", width = 340,
      selectInput("region", "Region", choices = c("All regions" = "All", named_choices(REGIONS))),
      selectInput("pa", "Planning area", choices = c("All planning areas" = "All", named_choices(PAS))),
      selectInput("cuisine", "Centres that sell", choices = c("Any cuisine" = "All", CUISINE_GROUPS)),
      selectInput("map_metric", "Shade planning areas by",
                  choices = c("Number of centres" = "n",
                              "Centres per 10 km²" = "density",
                              "Cooked-food stalls" = "stalls",
                              "Residents" = "residents")),
      selectInput("cg_cuisine", "Cuisine to analyse (charts)", choices = CUISINE_GROUPS, selected = "Western"),
      selectInput("cg_centre", "Hawker centre (charts)", choices = NULL),
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
      tag_note("A region limits the planning areas; a planning area sets its region. Cuisines and slider ranges follow the chosen area. Clicking a centre on the map also picks it for the charts.")
    ),

    # -------------------------------------------------------------------------
    # Section 1: the map (V1 + V2)
    # -------------------------------------------------------------------------
    mck_section(
      "sec-map", "Where hawker centres are",
      "Location, density and residents, filtered by your budget and the vacant stalls you need",
      reveal = FALSE,
      p(class = "sec-insight", textOutput("page_insight", inline = TRUE)),
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
        full_screen = TRUE, class = "exhibit",
        div(class = "ex-label", "Exhibit V1 + V2"),
        div(class = "ex-headline", textOutput("map_headline", inline = TRUE)),
        div(class = "ex-subtitle", uiOutput("map_subtitle", inline = TRUE)),
        layout_columns(
          col_widths = c(9, 3), fill = FALSE,
          # Map with its legend in one row right below it
          div(leafletOutput("map", height = 580),
              div(class = "map-legend-below", uiOutput("map_legend"))),
          # Beside the map: the centre card (shadow, no border)
          div(class = "map-info",
              div(class = "centre-card-box",
                  div(class = "info-title", "Centre card"),
                  uiOutput("centre_card"),
                  plotlyOutput("centre_cuisine", height = 200)),
              tag_note(class = "mt-2", "Hover a planning area or a circle for details; click a circle or a table row to fill the centre card. The big number marks the top planning area for the chosen shading."))
        ),
        div(class = "ex-source",
            "Source: NEA hawker centres and tender notices; Hawker Guide; URA Master Plan 2019; SingStat, June 2026; LTA MRT/LRT exits")
      ),
      # fill = FALSE: the card grows with the table instead of scrolling
      card(
        fill = FALSE,
        card_header(class = "d-flex align-items-center",
                    tags$strong("Centre table"),
                    div(class = "ms-auto",
                        downloadButton("centre_csv", "CSV", icon = icon("file-csv"),
                                       class = "btn-sm btn-outline-success"))),
        DTOutput("centre_table", fill = FALSE)  # size to its rows, no 400 px fill box
      )
    ),

    # -------------------------------------------------------------------------
    # Sections 2-4: exhibits V3-V10 (empty, named spaces for the teammates)
    # -------------------------------------------------------------------------
    mck_section(
      "sec-cuisine", "Cuisine supply and demand",
      "What existing stalls sell, what residents may want, and where the gaps are",
      layout_column_wrap(
        width = 1/2, fill = FALSE,
        exhibit("V3", "Cuisine supply comparison", "<b>Share of stalls by cuisine</b>, %",
                "Source: Hawker Guide"),
        exhibit("V4", "Cuisine demand heatmap", "<b>Estimated demand share by planning area</b>, %",
                "Source: synthetic cuisine survey; URA Master Plan 2019"),
        exhibit("V5", "Age-grouped cuisine preference", "<b>Estimated preference by age group</b>, %",
                "Source: synthetic cuisine survey; SingStat, June 2026"),
        exhibit("V6", "Demand vs supply share", "<b>Demand share minus supply share</b>, percentage points",
                "Source: synthetic cuisine survey; Hawker Guide"),
        exhibit("V7", "Demand–supply gap vs rent", "<b>Demand–supply gap vs median assessed market rent</b>, percentage points and S$",
                "Source: synthetic cuisine survey; Hawker Guide; NEA tender notices")
      )
    ),
    mck_section(
      "sec-access", "Access and residents",
      "The customer base and transit access around each centre",
      exhibit("V8", "MRT access vs residents (quadrant)",
              "<b>Distance to nearest MRT/LRT exit vs subzone residents</b>, metres and residents",
              "Source: LTA MRT/LRT exits; SingStat, June 2026")
    ),
    mck_section(
      "sec-tenancy", "Tenancy risk",
      "Which centres struggle to let their stalls, and which are tightly held",
      layout_column_wrap(
        width = 1/2, fill = FALSE,
        exhibit("V9", "Relisting frequency", "<b>Listings per distinct stall, by release category</b>, ratio",
                TENDER_SOURCE),
        exhibit("V10", "Tender timeline heatmap", "<b>Listings per centre per month</b>, number",
                TENDER_SOURCE)
      )
    )
  )
)
