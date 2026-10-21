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
  gap: 16px; background: #E8F2DE; border-bottom: 3px solid #CFE3BE; padding: 8px 24px; margin: 0 -12px 4px; }
/* Scroll progress: the line under the header fills as you scroll down */
.scroll-progress { position: absolute; left: 0; bottom: -3px; height: 3px; width: 0; background: #0D743B; }
.app-title { font-weight: 600; font-size: 1.45rem; line-height: 1.2; color: #1E2B22; }
.app-question { font-size: 0.8rem; color: #404041; }
.jump-links a { color: #404041; text-decoration: none; border-radius: 8px; padding: 0.45rem 0.9rem; margin-left: 4px; font-size: 0.95rem; }
.jump-links a:hover { background: #DDECC1; }
.jump-links a.active { background: #0D743B; color: #FFFFFF; }

/* Sidebar compacting: 50% reduced vertical spacing between categories & inputs */
.sidebar > .sidebar-content { gap: 6px !important; padding: 8px 12px !important; }
.sidebar .sidebar-title { font-size: 1.05rem !important; margin: 0 0 6px !important; padding-bottom: 6px !important; }
.sidebar .shiny-input-container { margin-bottom: 5px !important; }
.sidebar .form-group { margin-bottom: 5px !important; }
.sidebar .control-label { margin-bottom: 2px !important; font-size: 0.82rem !important; font-weight: 500; color: #1E2B22; }
.sidebar hr { margin: 6px 0 !important; border-top: 1px solid #CFE3BE !important; opacity: 1; }
/* Sliders: expanded spacing (+50%) so numbers, grid ticks and values are completely clear and sharp */
.sidebar .shiny-input-container:has(.irs--shiny) { margin-bottom: 12px !important; }
.sidebar .irs--shiny { margin-bottom: 4px !important; }
/* Narrower track: the value labels over the two handles are wider than
   the handle (e.g. S$1,400), so they need room on both sides instead of running into the sidebar edge */
.sidebar .irs--shiny { margin-left: 22px !important; margin-right: 22px !important; }
.sidebar .irs--shiny.irs-with-grid { height: 52px !important; }
.sidebar .irs--shiny .irs-line { top: 22px !important; height: 7px !important; border-radius: 4px !important; background: #E2E8F0 !important; }
.sidebar .irs--shiny .irs-bar { top: 22px !important; height: 7px !important; background: #0D743B !important; }
.sidebar .irs--shiny .irs-handle, .sidebar .irs--shiny .irs-slider { top: 16px !important; width: 18px !important; height: 18px !important; border-radius: 50% !important; background: #2E6930 !important; box-shadow: 0 2px 5px rgba(0,0,0,0.2) !important; cursor: pointer !important; }
.sidebar .irs--shiny .irs-min, .sidebar .irs--shiny .irs-max { top: 0 !important; font-size: 0.70rem !important; color: #808285 !important; line-height: 1.2 !important; }
.sidebar .irs--shiny .irs-single, .sidebar .irs--shiny .irs-from, .sidebar .irs--shiny .irs-to { top: -2px !important; font-size: 0.72rem !important; font-weight: 600 !important; background-color: #0D743B !important; color: #FFFFFF !important; padding: 1px 5px !important; border-radius: 4px !important; line-height: 1.25 !important; }
.sidebar .irs--shiny .irs-grid { top: 28px !important; height: 22px !important; }
.sidebar .irs--shiny .irs-grid-pol { background-color: #94A3B8 !important; height: 5px !important; }
.sidebar .irs--shiny .irs-grid-pol.small { height: 3px !important; }
.sidebar .irs--shiny .irs-grid-text { bottom: 0 !important; font-size: 0.68rem !important; color: #475569 !important; font-weight: 500 !important; }
.sidebar .checkbox { margin-top: 2px !important; margin-bottom: 3px !important; }
.sidebar .checkbox label { font-size: 0.80rem; }
.sidebar #t1_count { margin-top: 2px !important; margin-bottom: 4px !important; font-size: 0.74rem !important; }
.sidebar .tag-note { margin-top: 4px !important; font-size: 0.70rem !important; line-height: 1.25; }
.sidebar .btn { padding: 4px 8px !important; font-size: 0.78rem !important; }

/* Sticky floating map: stays fixed & elevated until scrolled past reset filter */
.map-sticky-wrap { position: relative; margin-bottom: 14px; }
.map-floating-card { position: sticky; top: 54px; z-index: 95; border-radius: 8px;
  box-shadow: 0 12px 30px rgba(13, 116, 59, 0.16), 0 4px 12px rgba(64, 64, 65, 0.10) !important;
  transition: box-shadow 0.25s ease; }

/* Filters toggle: a three-bar (hamburger) button in the sticky header, always
   visible. bslib's own small toggle (which scrolls away) is made invisible but
   NOT display:none — bslib only shows the filters again when that toggle's icon
   finishes its rotation (transitionend), so it must stay rendered. */
.bslib-sidebar-layout > .collapse-toggle { opacity: 0 !important; pointer-events: none !important; }
.bslib-sidebar-layout > .main { padding: 8px 16px 20px !important; }
#sec-map.mck-section { margin-top: 0 !important; padding-top: 0 !important; }
#sec-map .sec-title { margin-top: 0 !important; }
.brand { display: flex; align-items: center; gap: 14px; }
.hamburger-btn { width: 40px; height: 40px; border: none; border-radius: 8px; background: transparent; flex: none;
  display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 4px; cursor: pointer; }
.hamburger-btn span { display: block; width: 20px; height: 2px; border-radius: 2px; background: #1E2B22; }
.hamburger-btn:hover { background: #DDECC1; }
.hamburger-btn:focus-visible { outline: 2px solid #0D743B; outline-offset: 2px; }

/* Boxes: no borders, soft outer shadow, compact & balanced spacing */
.card, .bslib-value-box { border: none !important; border-radius: 7px; background: #FFFFFF;
  box-shadow: 0 4px 12px rgba(13, 116, 59, 0.08), 0 1px 3px rgba(64, 64, 65, 0.06) !important; }
.card-header, .card-footer { background: #FFFFFF; border: none; }
.bslib-value-box { min-height: unset !important; height: auto !important; margin-bottom: 0 !important; }
.bslib-value-box > .card-body { padding: 0 !important; margin: 0 !important; }
.bslib-value-box .value-box-grid { grid-template-columns: 1fr !important; gap: 0 !important; }
.bslib-value-box .value-box-area { padding: 8px 12px !important; display: flex !important; flex-direction: column !important; justify-content: center !important; gap: 2px !important; }
.bslib-value-box .value-box-title { font-size: 0.72rem !important; font-weight: 500 !important; color: #64748B !important; line-height: 1.2 !important; min-height: 2.1em !important; margin: 0 0 1px !important; display: flex !important; align-items: flex-start !important; }
.bslib-value-box .value-box-value { font-family: 'IBM Plex Serif', Georgia, serif !important; font-weight: 600 !important; font-size: 1.35rem !important; color: #1E2B22 !important; line-height: 1.15 !important; margin: 1px 0 !important; white-space: nowrap !important; }
.vb-sub { font-size: 0.70rem !important; color: #64748B !important; margin: 0 !important; line-height: 1.2 !important; min-height: 1.2em !important; white-space: nowrap !important; overflow: hidden !important; text-overflow: ellipsis !important; }
.mono { font-family: 'IBM Plex Mono', monospace !important; }
.card.exhibit[data-code='V1+V2'] > .card-body { padding: 12px 14px; gap: 4px; }
#sec-map .bslib-grid { gap: 10px !important; margin-bottom: 10px !important; }
#sec-map .bslib-grid .bslib-grid-item { min-height: unset !important; }

/* McKinsey-style sections */
.mck-section { scroll-margin-top: 70px; margin-bottom: 1.75rem; }
#sec-map.mck-section { margin-bottom: 0.85rem; }
.sec-title { font-family: 'IBM Plex Serif', Georgia, serif; font-size: 1.35rem; font-weight: 600; color: #1E2B22; margin: 0 0 2px; line-height: 1.2; }
.sec-kicker { font-size: 0.82rem; color: #64748B; margin-bottom: 8px; line-height: 1.25; }
.sec-insight { font-size: 0.85rem; line-height: 1.35; color: #475569; max-width: 950px; margin-bottom: 8px; }

/* McKinsey-style exhibits: label, takeaway headline, subtitle, chart, source */
.exhibit > .card-body { padding: 22px 26px; gap: 8px; }
.ex-headline { font-size: 1.35rem; font-weight: 600; color: #1E2B22; line-height: 1.3; margin-bottom: 4px; }
.ex-subtitle { font-size: 0.95rem; color: #404041; margin-bottom: 14px; }
.ex-subtitle b { color: #1E2B22; }
.ex-source { font-size: 0.75rem; font-style: italic; color: #808285; margin-top: 0; line-height: 1.35; }
.ex-placeholder .ex-headline { color: #808285; }
.placeholder-body { height: 300px; background: #F6F8F9; border-radius: 6px; }

/* Map */
.leaflet-container { background: #EDF0F2; border-radius: 6px; }
.bslib-card[data-full-screen='true'] #map { height: calc(100vh - 400px) !important; }
.map-info .info-title, .map-legend-below .info-title { font-weight: 600; font-size: 0.85rem; }
.legend-row { display: flex; align-items: center; gap: 6px; font-size: 0.8rem; line-height: 1.3; margin-bottom: 4px; }
.legend-swatch { width: 18px; height: 12px; border-radius: 3px; border: 1px solid #C8CDD2; flex: none; }
.legend-dot { width: 12px; height: 12px; border-radius: 50%; background: #FAA619; flex: none; }
.mrt-icon { width: 16px; height: 16px; flex: none; vertical-align: -3px; margin-right: 2px; }
.leaflet-marker-icon.leaflet-interactive { filter: drop-shadow(0 1px 2px rgba(30, 43, 34, 0.35)); }
.map-legend-below > div { display: flex; flex-wrap: wrap; align-items: center; gap: 0 14px; margin-top: 0; }
.map-legend-below .info-title { width: 100%; margin: 0; line-height: 1.25; }
.map-legend-below .legend-row { margin-bottom: 0; line-height: 1.25; }
/* Note and source under the legend: tight lines, small gaps */
.map-note { margin: 0 !important; line-height: 1.25; }
.map-floating-card .ex-source { line-height: 1.25; }
.centre-card-box { border-radius: 6px; padding: 8px 10px; background: #FFFFFF;
  box-shadow: 0 4px 14px rgba(13, 116, 59, 0.16), 0 1px 3px rgba(64, 64, 65, 0.12); }
/* Map spans the card; the centre card floats over its top-right corner.
   z-index 0 on the wrapper keeps the map and card below the sticky header. */
.map-wrap { position: relative; z-index: 0; }
.map-overlay { position: absolute; top: 12px; right: 12px; width: 245px; z-index: 950; }
.centre-card-box .info-title { font-size: 0.70rem; margin: 0 0 0.35rem; }
/* Centre search box over the map's top-left corner; zoom buttons move below it */
.map-search { position: absolute; top: 12px; left: 12px; width: 320px; z-index: 960; }
.map-search-box { position: relative; display: flex; align-items: center; background: #FFFFFF; border-radius: 6px;
  box-shadow: 0 4px 14px rgba(13, 116, 59, 0.16), 0 1px 3px rgba(64, 64, 65, 0.12); }
.map-search-box:focus-within { box-shadow: 0 0 0 2px #0D743B, 0 4px 14px rgba(13, 116, 59, 0.16); }
.map-search-icon { position: absolute; left: 13px; top: 50%; transform: translateY(-50%); z-index: 2;
  color: #808285; pointer-events: none; }
.map-search-input { width: 100%; border: none; outline: none; padding: 9px 32px 9px 36px; border-radius: 6px;
  font-size: 0.9rem; font-family: inherit; color: #1E2B22; background: transparent; }
.map-search-clear { position: absolute; right: 8px; top: 50%; transform: translateY(-50%); border: none;
  background: transparent; color: #808285; cursor: pointer; padding: 2px 6px; font-size: 0.85rem; display: none; }
.map-search-clear:hover { color: #1E2B22; }
.map-search-dropdown { position: absolute; top: calc(100% + 4px); left: 0; right: 0; background: #FFFFFF;
  border-radius: 6px; box-shadow: 0 6px 18px rgba(30, 43, 34, 0.18); max-height: 280px; overflow-y: auto;
  z-index: 1003; display: none; }
.map-search-item { padding: 8px 12px; cursor: pointer; border-bottom: 1px solid #F6F8F9; }
.map-search-item:last-child { border-bottom: none; }
.map-search-item:hover, .map-search-item.active { background: #E8F2DE; color: #0D743B; }
.map-search-item-name { font-weight: 600; font-size: 0.86rem; color: #1E2B22; line-height: 1.25; }
.map-search-item:hover .map-search-item-name { color: #0D743B; }
.map-search-item-pa { font-size: 0.74rem; color: #58595B; margin-top: 1px; }
.map-search-empty { padding: 12px; font-size: 0.82rem; color: #808285; text-align: center; }
.map-wrap .leaflet-top.leaflet-left { top: 56px; }

/* McKinsey-style tooltips on the map */
.leaflet-map-pane { z-index: auto !important; }
.leaflet-pane.leaflet-tooltip-pane { z-index: 1005 !important; }
.leaflet-tooltip.mck-tt { background: #FFFFFF; border: none; border-radius: 6px; padding: 10px 12px;
  box-shadow: 0 6px 18px rgba(30, 43, 34, 0.18); font-family: 'IBM Plex Sans', sans-serif; color: #404041;
  z-index: 1005 !important; pointer-events: none; }
.leaflet-tooltip.mck-tt::before { display: none; }
/* Floating copy of the map tooltip, above everything (incl. the centre card) */
.float-tip { position: fixed; z-index: 2000; display: none; pointer-events: none; max-width: 320px;
  background: #FFFFFF; border-radius: 6px; padding: 10px 12px; box-shadow: 0 6px 18px rgba(30, 43, 34, 0.18);
  font-family: 'IBM Plex Sans', sans-serif; font-size: 0.85rem; color: #404041; }
.tt-title { font-weight: 600; color: #1E2B22; font-size: 0.9rem; }
.tt-big { font-family: 'IBM Plex Serif', Georgia, serif; font-size: 1.7rem; color: #0D743B; line-height: 1.1; margin-top: 2px; }
.tt-sub { font-size: 0.78rem; color: #58595B; }
.tt-meta { font-size: 0.78rem; color: #404041; margin-top: 6px; padding-top: 6px; border-top: 1px solid #EDF0F2; }

/* Centre table: header in the app's light green with a dark green rule */
table.dataTable thead th { background: #E8F2DE !important; color: #0D743B; font-weight: 600; border-bottom: 2px solid #0D743B !important; }
/* Centre table: compact rows, header on one line, tight search / info / pager */
.table-card > .card-body { padding-top: 6px; }
#centre_table table.dataTable > thead > tr > th,
#centre_table table.dataTable > tbody > tr > td { padding: 5px 10px !important; line-height: 1.3; }
#centre_table table.dataTable > thead > tr > th { white-space: nowrap; }
#centre_table .dataTables_filter { margin-bottom: 6px; }
#centre_table .dataTables_filter input { padding: 2px 8px; margin-left: 6px; }
#centre_table .dataTables_info { padding-top: 6px !important; font-size: 0.82rem; }
#centre_table .dataTables_paginate { padding-top: 4px !important; }
#centre_table .pagination { margin: 0; }
#centre_table .page-link { padding: 2px 10px; font-size: 0.85rem; }

/* Comparison table: drag handle, sortable headers, remove button, empty state */
.compare-table { margin-bottom: 0; }
.compare-table thead th { background: #E8F2DE; color: #0D743B; font-weight: 600; border-bottom: 2px solid #0D743B; white-space: nowrap; }
.compare-table th.sortable-col { cursor: pointer; user-select: none; }
.compare-table th.sortable-col:hover { text-decoration: underline; }
.compare-table td { vertical-align: middle; user-select: none; }
.compare-table > :not(caption) > tr > th, .compare-table > :not(caption) > tr > td { padding: 5px 10px; line-height: 1.3; }
.compare-table tbody tr { cursor: pointer; }
.compare-table tbody tr:hover td { background: #F2F8EC; }
.compare-table tbody tr.is-selected td { background: #E8F2DE; }
.compare-table tbody tr.is-selected td:first-child { box-shadow: inset 4px 0 0 #0D743B; }
/* Selected centre on the map: a pulsing ring */
.sel-ring { animation: sel-pulse 1.6s ease-in-out infinite; }
@keyframes sel-pulse { 0%, 100% { stroke-opacity: 1; } 50% { stroke-opacity: 0.25; } }
@media (prefers-reduced-motion: reduce) { .sel-ring { animation: none; } }
.compare-table td.num { text-align: right; }
.drag-handle { cursor: grab; color: #808285; width: 28px; text-align: center; font-weight: 700; letter-spacing: -2px; }
.sortable-ghost { opacity: 0.4; background: #E8F2DE; }
.btn-remove { border: none; background: transparent; color: #808285; font-size: 1rem; line-height: 1; padding: 2px 6px; }
.btn-remove:hover { color: #B03A2E; }
.compare-empty { text-align: center; padding: 28px 16px; }
.compare-empty-icon { font-size: 2rem; color: #0D743B; margin-bottom: 6px; }
.compare-empty-title { font-family: 'IBM Plex Serif', Georgia, serif; font-size: 1.35rem; color: #1E2B22; }
.compare-empty-text { color: #58595B; margin: 6px auto 14px; max-width: 560px; }
.compare-empty-quick { display: flex; flex-wrap: wrap; gap: 8px; justify-content: center; align-items: center; }

/* Back to top: round up-arrow button, bottom right, shown after scrolling down */
.back-to-top { position: fixed; right: 24px; bottom: 24px; z-index: 1050; width: 46px; height: 46px; border-radius: 50%;
  border: none; background: #0D743B; color: #FFFFFF; font-size: 1.1rem; display: flex; align-items: center; justify-content: center;
  box-shadow: 0 6px 16px rgba(13, 116, 59, 0.30); opacity: 0; pointer-events: none; transform: translateY(10px);
  transition: opacity 0.25s ease, transform 0.25s ease, background 0.2s ease; }
.back-to-top.show { opacity: 1; pointer-events: auto; transform: none; }
.back-to-top:hover { background: #40AD49; }
/* Notifications sit above the back-to-top button */
#shiny-notification-panel { bottom: 80px !important; }

/* Centre card: header, stat tiles, cuisine mix (compacted 30%) */
.cc-name { font-family: 'IBM Plex Serif', Georgia, serif; font-size: 0.92rem; line-height: 1.2; color: #1E2B22; }
.cc-meta { font-size: 0.65rem; color: #58595B; margin-top: 2px; display: flex; flex-wrap: wrap; gap: 4px; align-items: center; }
.cc-chip { background: #DDECC1; color: #0D743B; font-weight: 600; font-size: 0.58rem; padding: 1px 5px; border-radius: 999px; }
.cc-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 5px; margin: 8px 0 6px; }
.cc-stat { background: #F6F8F9; border-radius: 4px; padding: 5px 6px; }
.cc-label { font-size: 0.52rem; text-transform: uppercase; letter-spacing: 0.04em; color: #58595B; font-weight: 600; }
.cc-value { font-family: 'IBM Plex Serif', Georgia, serif; font-size: 0.88rem; color: #1E2B22; line-height: 1.15; }
.cc-note { font-size: 0.55rem; color: #808285; line-height: 1.15; }
.cc-section-title { font-size: 0.65rem; font-weight: 600; color: #1E2B22; margin: 3px 0 0; }
.cc-compare-btn { font-size: 0.72rem !important; padding: 3px 6px !important; }
.cc-empty { text-align: center; color: #58595B; padding: 18px 6px; font-size: 0.72rem; }
.cc-empty .fa, .cc-empty svg { font-size: 1.2rem; color: #0D743B; display: block; margin: 0 auto 6px; }

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
// Header hamburger: hide or show the filters sidebar, then let the map resize
function toggleFilters() {
  var t = document.querySelector('.bslib-sidebar-layout > .collapse-toggle');
  if (!t) return;
  t.click();
  // Safety net: if bslib is still mid-switch, tell it the animation has ended
  setTimeout(function () {
    var lay = t.closest('.bslib-sidebar-layout');
    var icon = t.querySelector('.collapse-icon');
    if (lay && icon && lay.classList.contains('transitioning')) icon.dispatchEvent(new Event('transitionend'));
  }, 800);
  [350, 700, 900].forEach(function (ms) {
    setTimeout(function () {
      window.dispatchEvent(new Event('resize'));
      var w = window.HTMLWidgets && HTMLWidgets.find('#map');
      var m = w && w.getMap && w.getMap();
      if (m) m.invalidateSize();
    }, ms);
  });
}

$(function () {
  // bslib's own sidebar toggle is invisible (the header button replaces it): keep it out of the Tab order
  document.querySelectorAll('.bslib-sidebar-layout > .collapse-toggle').forEach(function (b) {
    b.setAttribute('tabindex', '-1'); b.setAttribute('aria-hidden', 'true');
  });

  // Expanding or collapsing the map card: resize the map and fit Singapore
  var fit = function () {
    var w = HTMLWidgets.find('#map');
    var m = w && w.getMap();
    if (!m) return;
    m.invalidateSize();
    // leave room on the right for the floating centre card
    m.fitBounds([[1.205, 103.60], [1.475, 104.05]], { paddingTopLeft: [10, 10], paddingBottomRight: [265, 10] });
  };
  new MutationObserver(function () { setTimeout(fit, 300); })
    .observe(document.body, { attributes: true, subtree: true, attributeFilter: ['data-full-screen'] });

  // Map search: pure autocomplete, never opens dropdown on click
  (function () {
    var centres = window.CENTRES_DATA || [];
    var $input = $('#map_search_input');
    var $drop = $('#map_search_dropdown');
    var $clear = $('#map_search_clear');

    function renderResults(q) {
      if (!q) {
        $drop.hide().empty();
        $clear.hide();
        return;
      }
      $clear.show();
      var qLower = q.toLowerCase();
      var matches = centres.filter(function (c) {
        return (c.name && c.name.toLowerCase().indexOf(qLower) !== -1) ||
               (c.pa && c.pa.toLowerCase().indexOf(qLower) !== -1);
      }).slice(0, 10);

      if (matches.length === 0) {
        $drop.html('<div class=\"map-search-empty\">No hawker centre found</div>').show();
        return;
      }

      var html = matches.map(function (c) {
        return '<div class=\"map-search-item\" data-id=\"' + c.id + '\">' +
                 '<div class=\"map-search-item-name\">' + c.name + '</div>' +
                 '<div class=\"map-search-item-pa\">' + c.pa + '</div>' +
               '</div>';
      }).join('');
      $drop.html(html).show();
    }

    // Focusing/clicking the input selects all text; NO DROPDOWN OPENS
    $input.on('focus', function () {
      this.select();
    });

    // Dropdown only appears when typing
    $input.on('input', function () {
      renderResults(this.value.trim());
    });

    // Click a matching centre
    $drop.on('click', '.map-search-item', function () {
      var id = $(this).data('id');
      var name = $(this).find('.map-search-item-name').text();
      $input.val(name);
      $drop.hide().empty();
      $clear.show();
      Shiny.setInputValue('cg_centre', id, { priority: 'event' });
    });

    // Clear button (x)
    $clear.on('click', function () {
      $input.val('').focus();
      $drop.hide().empty();
      $clear.hide();
    });

    // Close on click outside or Escape
    $(document).on('click', function (e) {
      if (!$(e.target).closest('.map-search').length) {
        $drop.hide();
      }
    });
    $input.on('keydown', function (e) {
      if (e.key === 'Escape') {
        $drop.hide();
      } else if (e.key === 'Enter') {
        var $first = $drop.find('.map-search-item').first();
        if ($first.length) {
          $first.trigger('click');
          $input.blur();
        }
      }
    });

    // Set search text when centre is selected elsewhere (map marker, table row)
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('set_search_centre', function (data) {
        if (data && data.name) {
          $input.val(data.name);
          $clear.show();
          $drop.hide();
        }
      });
    }
  })();

  // Map: a centre circle (orange) turns dark green while the mouse is over it
  var hookMap = setInterval(function () {
    var w = window.HTMLWidgets && HTMLWidgets.find('#map');
    var m = w && w.getMap && w.getMap();
    if (!m) return;
    clearInterval(hookMap);
    var hover = function (l) {
      if (!(l instanceof L.CircleMarker) || l.options.fillColor !== '#FAA619' || l._hoverHooked) return;
      l._hoverHooked = true;
      var r = l.getRadius();
      l.on('mouseover', function () { l.setStyle({ fillColor: '#0D743B', fillOpacity: 1 }); l.setRadius(r + 2); l.bringToFront(); });
      l.on('mouseout', function () { l.setStyle({ fillColor: '#FAA619', fillOpacity: 0.9 }); l.setRadius(r); });
    };
    m.eachLayer(hover);
    m.on('layeradd', function (e) { hover(e.layer); });

    // Map tooltips are shown in a floating layer above the whole page, so the
    // centre card can never cover them (Leaflet's own tooltips live inside the
    // map's moving layer, which always sits below anything floating over the map).
    var tip = document.createElement('div');
    tip.className = 'float-tip';
    document.body.appendChild(tip);
    var active = null;
    var place = function (latlng) {
      var pt = m.latLngToContainerPoint(latlng), box = m.getContainer().getBoundingClientRect();
      var x = box.left + pt.x, y = box.top + pt.y, w = tip.offsetWidth, h = tip.offsetHeight;
      var left = x + 16;
      if (left + w > window.innerWidth - 8) left = x - w - 16;  // flip to the left near the edge
      var top = Math.max(8, Math.min(y - h / 2, window.innerHeight - h - 8));
      tip.style.left = left + 'px';
      tip.style.top = top + 'px';
    };
    m.on('tooltipopen', function (e) {
      var t = e.tooltip, cls = t.options.className || '';
      if (cls.indexOf('mck-tt') < 0) return;  // only the McKinsey-style tooltips
      active = t;
      var el = t.getElement();
      if (el) el.style.visibility = 'hidden';
      var c = t.getContent();
      tip.innerHTML = typeof c === 'string' ? c : (c && c.outerHTML) || '';
      tip.style.display = 'block';
      place(t.getLatLng());
    });
    m.on('tooltipclose', function (e) {
      if (e.tooltip === active) { active = null; tip.style.display = 'none'; }
    });
    m.on('mousemove', function (e) { if (active) place(e.latlng); });  // follow the mouse
    m.on('movestart zoomstart', function () { active = null; tip.style.display = 'none'; });
  }, 300);

  // Scroll progress: the line under the header fills as the page scrolls
  var bar = document.querySelector('.scroll-progress');
  var progress = function () {
    var h = document.documentElement, max = h.scrollHeight - h.clientHeight;
    bar.style.width = (max > 0 ? Math.min(100, h.scrollTop / max * 100) : 0) + '%';
    // Back-to-top button appears after scrolling down a little
    var up = document.querySelector('.back-to-top');
    if (up) up.classList.toggle('show', h.scrollTop > 400);
  };
  window.addEventListener('scroll', progress, { passive: true });
  window.addEventListener('resize', progress);
  progress();

  // Sticky floating map track height sync with reset filter button
  var syncMapStickyTrack = function () {
    var wrap = document.querySelector('.map-sticky-wrap');
    var card = document.querySelector('.map-floating-card');
    var resetBtn = document.getElementById('reset_t1');
    if (!wrap || !card || !resetBtn) return;
    var resetRect = resetBtn.getBoundingClientRect();
    var wrapRect = wrap.getBoundingClientRect();
    var cardH = card.offsetHeight;
    var targetBottom = (resetRect.bottom + window.scrollY) - (wrapRect.top + window.scrollY);
    if (targetBottom > cardH) {
      wrap.style.minHeight = (targetBottom + 20) + 'px';
    } else {
      wrap.style.minHeight = 'auto';
    }
  };
  window.addEventListener('resize', syncMapStickyTrack);
  window.addEventListener('scroll', syncMapStickyTrack, { passive: true });
  setTimeout(syncMapStickyTrack, 500);
  setTimeout(syncMapStickyTrack, 1500);

  if (!('IntersectionObserver' in window)) return;  // old browser: no effects, all visible

  // Sections fade up as they scroll into view
  document.documentElement.classList.add('js-reveal');
  var reveal = new IntersectionObserver(function (entries) {
    entries.forEach(function (e) {
      if (e.isIntersecting) { e.target.classList.add('is-visible'); reveal.unobserve(e.target); }
    });
  }, { threshold: 0.08 });
  document.querySelectorAll('.reveal').forEach(function (el) { reveal.observe(el); });

  // Sliders: tick marks that match the real number of steps. A 0-1 range (a small
  // planning area) shows two ticks, 0 and 1, instead of ten labelled 0 0 0 1 1 1.
  // Few steps: one tick per step. Many steps: the number of ticks (2-6) that divides
  // the steps evenly, so every label is a round value on the slider's step. Re-checked
  // after every redraw (the filters change the slider ranges from the server).
  var fitSliderGrid = function (id) {
    var s = window.jQuery && jQuery('#' + id).data('ionRangeSlider');
    if (!s) return false;
    var o = s.options;
    if (o.origGrid === undefined) o.origGrid = o.grid_num;
    var steps = Math.round((o.max - o.min) / (o.step || 1)), want = null, k;
    if (steps <= 8) want = Math.max(steps, 1);
    else for (k = 6; k >= 2 && want === null; k--) if (steps % k === 0) want = k;
    if (want === null) want = o.origGrid;
    if (o.grid_num !== want || o.grid_snap !== false) s.update({ grid_num: want, grid_snap: false });
    return true;
  };
  var sliderIds = ['max_amr', 'max_mrt', 'min_vacant'], armed = {};
  var armSliders = setInterval(function () {
    sliderIds.forEach(function (id) {
      if (armed[id] || !fitSliderGrid(id)) return;
      armed[id] = true;
      new MutationObserver(function () { fitSliderGrid(id); })
        .observe(document.getElementById(id).parentNode, { childList: true, subtree: true });
    });
    if (sliderIds.every(function (id) { return armed[id]; })) clearInterval(armSliders);
  }, 300);

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
       `data-code` = code,
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
  tags$head(tags$style(HTML(app_css)), tags$script(HTML(paste0("window.CENTRES_DATA = ", CENTRES_SEARCH_JSON, ";"))), tags$script(HTML(app_js))),

  # ---------------------------------------------------------------------------
  # Header: title, question and jump links to the sections
  # ---------------------------------------------------------------------------
  div(class = "app-header", id = "top",
      div(class = "brand",
          # Hide / show the filters (clicks bslib's hidden sidebar toggle)
          tags$button(type = "button", class = "hamburger-btn", title = "Hide or show the filters",
                      `aria-label` = "Hide or show the filters", onclick = "toggleFilters()",
                      tags$span(), tags$span(), tags$span()),
          div(div(class = "app-title", "Hawker Site Finder"),
              div(class = "app-question", "Where should a new hawker open, and what should they sell?"))),
      tags$nav(class = "jump-links",
               tags$a(href = "#sec-map", class = "active", "Map"),
               tags$a(href = "#sec-compare", "Compare"),
               tags$a(href = "#sec-viz", "Data visualization")),
      div(class = "scroll-progress")),

  layout_sidebar(
    fillable = FALSE, border = FALSE,
    # One sidebar for the whole page, filters grouped by type
    sidebar = sidebar(
      title = "Filters", width = 270,
      selectInput("region", "Region", choices = c("All regions" = "All", named_choices(REGIONS))),
      selectInput("pa", "Planning area", choices = c("All planning areas" = "All", named_choices(PAS))),
      selectInput("cuisine", "Centres that sell", choices = c("Any cuisine" = "All", CUISINE_GROUPS)),
      selectInput("map_metric", "Shade planning areas by",
                  choices = c("Number of centres" = "n",
                              "Centres per 10 km²" = "density",
                              "Cooked-food stalls" = "stalls",
                              "Residents" = "residents")),
      # (The Hawker centre picker is now the search box on the map, see Section 1)
      tags$hr(class = "my-1"),
      sliderInput("max_amr", "Median rent (AMR, S$)", min = AMR_RANGE[1], max = AMR_RANGE[2],
                  value = AMR_RANGE, step = 50, pre = "S$"),
      sliderInput("max_mrt", "Distance to MRT exit (m)", min = 0, max = MRT_MAX,
                  value = c(0, MRT_MAX), step = 100),
      sliderInput("min_vacant", "Min. vacant stalls (Hawker Guide)", min = 0, max = VAC_MAX,
                  value = 0, step = 1),
      tags$hr(class = "my-1"),
      checkboxInput("only_tenders", "Only centres with tender listings", FALSE),
      checkboxInput("map_exits", tagList(mrt_icon_img(), "Show MRT/LRT exits"), FALSE),
      uiOutput("t1_count"),
      reset_button("reset_t1"),
      tag_note("A region limits the planning areas; a planning area sets its region. Cuisines and slider ranges follow the chosen area. To find one hawker centre, use the search box at the top left of the map.")
    ),

    # -------------------------------------------------------------------------
    # Section 1: the map (V1 + V2)
    # -------------------------------------------------------------------------
    mck_section(
      "sec-map", "Where hawker centres are",
      "Location, density and residents, filtered by your budget and the vacant stalls you need",
      reveal = FALSE,
      layout_columns(
        fill = FALSE, gap = "10px",
        value_box("Hawker centres", textOutput("vb_centres"),
                  tags$p(class = "vb-sub mono", "hawker_centres")),
        value_box(paste0("Tender listings (", month_label(MONTHS[1]), " – ", month_label(MONTHS[2]), ")"),
                  textOutput("vb_listings"),
                  tags$p(class = "vb-sub", textOutput("vb_listings_sub", inline = TRUE))),
        value_box("Median centre rent", textOutput("vb_rent"), tags$p(class = "vb-sub", "per month (AMR)")),
        value_box("Avg distance to MRT exit", textOutput("vb_mrt"), tags$p(class = "vb-sub", "straight line"))
      ),
      div(class = "map-sticky-wrap",
        card(
          full_screen = TRUE, class = "exhibit map-floating-card", `data-code` = "V1+V2",
        # Full-width map; the centre card floats over its top-right corner, so
        # clicking a centre never changes the size of the map or the page layout
        div(class = "map-wrap",
            leafletOutput("map", height = 620),
            # Search box over the map's top-left corner: pick a centre to zoom to it
            # and open its centre card (replaces the old sidebar dropdown)
            div(class = "map-search",
                div(class = "map-search-box",
                    icon("magnifying-glass", class = "map-search-icon"),
                    tags$input(type = "text", id = "map_search_input", class = "map-search-input",
                               placeholder = "Search hawker centre", autocomplete = "off", spellcheck = "false"),
                    tags$button(type = "button", id = "map_search_clear", class = "map-search-clear",
                                `aria-label` = "Clear search", tags$i(class = "fa fa-xmark"))),
                div(id = "map_search_dropdown", class = "map-search-dropdown")),
            div(class = "centre-card-box map-overlay",
                div(class = "info-title", "Centre card"),
                uiOutput("centre_card"),
                # Shown once a centre is chosen: its cuisine mix, then the Compare button
                conditionalPanel("output.centre_selected",
                  div(class = "cc-section-title", "Cuisine mix (stalls)"),
                  plotlyOutput("centre_cuisine", height = 120),
                  actionButton("add_compare", "Compare", icon = icon("plus"),
                               class = "btn-success btn-sm mt-1 w-100 cc-compare-btn")))),
        # Legend in one row right below the map
        div(class = "map-legend-below", uiOutput("map_legend")),
        tag_note(class = "map-note", "Search a centre (top left of the map) to zoom to it. Hover a planning area or a circle for details; click a circle or a table row to open its centre card (top right of the map). Double-click a circle, or press Compare, to add it to the comparison table."),
        div(class = "ex-source",
            "Source: NEA hawker centres and tender notices; Hawker Guide; URA Master Plan 2019; SingStat, June 2026; LTA MRT/LRT exits")
        )
      ),
      # fill = FALSE: the card grows with the table instead of scrolling
      card(
        fill = FALSE, class = "table-card",
        card_header(class = "d-flex align-items-center",
                    tags$strong("Centre table"),
                    div(class = "ms-auto",
                        downloadButton("centre_csv", "CSV", icon = icon("file-csv"),
                                       class = "btn-sm btn-outline-success"))),
        DTOutput("centre_table", fill = FALSE)  # size to its rows, no 400 px fill box
      )
    ),

    # -------------------------------------------------------------------------
    # Section 2: comparison table (V11) — the investor's shortlist
    # -------------------------------------------------------------------------
    mck_section(
      "sec-compare", "Compare your shortlist",
      "Weigh shortlisted centres side by side before you bid",
      card(
        fill = FALSE,
        card_header(class = "d-flex align-items-center gap-2",
                    tags$strong("Comparison table"),
                    span(class = "badge rounded-pill text-bg-light", textOutput("compare_count", inline = TRUE)),
                    div(class = "ms-auto d-flex gap-2",
                        actionButton("compare_clear", "Clear all", icon = icon("trash-can"),
                                     class = "btn-sm btn-outline-secondary"),
                        downloadButton("compare_csv", "CSV", icon = icon("file-csv"),
                                       class = "btn-sm btn-outline-success"))),
        uiOutput("compare_ui"),
        card_footer(tag_note("Double-click a row to show the centre on the map · drag ⋮⋮ to reorder · click a column header to sort · ✕ removes a centre · the cuisine column follows \"Centres that sell\" in the sidebar (stalls of that cuisine, or the main cuisine when it is \"Any cuisine\")."))
      )
    ),

    # -------------------------------------------------------------------------
    # Section 3: data visualization — exhibits V3-V10 (empty, named spaces for
    # the teammates): cuisine supply and demand, access and residents, tenancy risk
    # -------------------------------------------------------------------------
    mck_section(
      "sec-viz", "Data visualization",
      "Cuisine supply and demand, access and residents, and tenancy risk",
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
                "Source: synthetic cuisine survey; Hawker Guide; NEA tender notices"),
        exhibit("V8", "MRT access vs residents (quadrant)",
                "<b>Distance to nearest MRT/LRT exit vs subzone residents</b>, metres and residents",
                "Source: LTA MRT/LRT exits; SingStat, June 2026"),
        exhibit("V9", "Relisting frequency", "<b>Listings per distinct stall, by release category</b>, ratio",
                TENDER_SOURCE),
        exhibit("V10", "Tender timeline heatmap", "<b>Listings per centre per month</b>, number",
                TENDER_SOURCE)
      )
    ),

    # Back to top: round up-arrow button fixed at the bottom right
    tags$button(type = "button", class = "back-to-top", title = "Back to top", `aria-label` = "Back to top",
                onclick = "window.scrollTo({ top: 0, behavior: 'smooth' });",
                icon("arrow-up"))
  )
)
