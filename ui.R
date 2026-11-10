# =============================================================================
# HawkerRadar — ui.R
# One scrolling page in the style of McKinsey Global Institute's "2025 in
# charts": a full-screen opening, then sections with a centred heading under a
# rule, one screen per chart (chart left, story right), the map with its own
# filters, the comparison table and About us. The header's menu button lists them.
# =============================================================================

# Helpers tag_note() and reset_button() are defined in global.R

# Styles: brand colours (FMI template, Figma) + McKinsey-style typography,
# white cards with a soft shadow (no borders), IBM Plex Sans (sans-serif everywhere) / Mono
app_css <- "
/* Relative sizing. The design size is a 16:9 screen of 1920 x 1080. Type: 16 px there, never smaller on a small
   screen (so it stays easy to read) and growing with the window on a bigger one. Layout (widths, heights, padding,
   gaps, radii): every length is a number of --u, which is 1 px at 1920 px and follows the window width, so the page
   scales in proportion; it stops shrinking at 0.6 and growing at 1.5 times. */
:root { --u: clamp(0.6px, 0.0520833vw, 1.5px); }
html { font-size: clamp(16px, 0.8333vw, 28px); }
html { scroll-behavior: smooth; scroll-snap-type: y proximity; scrollbar-color: #80C683 #EEF5E6; }
/* Scrollbar in the page's greens (older Chromium / Safari use these rules) */
::-webkit-scrollbar { width: calc(12 * var(--u)); }
::-webkit-scrollbar-track { background: #EEF5E6; }
::-webkit-scrollbar-thumb { background: #80C683; border-radius: calc(8 * var(--u)); border: calc(3 * var(--u)) solid #EEF5E6; }
::-webkit-scrollbar-thumb:hover { background: #40AD49; }
/* The page itself shows no scrollbar (the dot navigation shows where you are); inner boxes keep theirs */
html { scrollbar-width: none; }
html::-webkit-scrollbar { width: 0; height: 0; }
.hero-rest { scroll-snap-align: start; }
body { background: linear-gradient(180deg, #E9F3E0 0%, #F4F9EF 40%, #FFFFFF 100%); background-attachment: fixed; }

/* Header (McKinsey style): the name on the left.
   Transparent with white text over the opening photos; white once scrolled past them. */
.app-header { position: fixed; top: 0; left: 0; right: 0; z-index: 1200; height: calc(64 * var(--u)); display: flex; align-items: center;
  background: transparent; color: #FFFFFF; transition: background 0.3s ease, color 0.3s ease, box-shadow 0.3s ease; }
.app-header.solid { background: #FFFFFF; color: #1E2B22; box-shadow: 0 2px calc(12 * var(--u)) rgba(30, 43, 34, 0.08); }
/* Scroll progress: the line under the header fills as you scroll down */
.scroll-progress { position: absolute; left: 0; bottom: 0; height: calc(3 * var(--u)); width: 0; background: #FAA619; transition: opacity 0.2s ease; }
.brand { display: flex; align-items: center; gap: calc(10 * var(--u)); padding: 0 calc(18 * var(--u)); color: inherit; text-decoration: none; }
.brand:hover { color: inherit; }
.app-title { font-family: 'Libre Baskerville', Georgia, serif; font-weight: 700; font-size: 1.12rem; line-height: 1; }

/* Opening section: photos sliding behind the title. The section is taller than the screen and its
   stage stays put (sticky) while you scroll: the title fades away (script in app_js), then for a
   stretch only the photos show, then the page moves on. */
.hero { position: relative; height: 175vh; margin: 0 calc(-12 * var(--u)); background: #10241A; color: #FFFFFF; scroll-snap-align: start; }
.hero-stage { position: sticky; top: 0; height: 100vh; min-height: calc(560 * var(--u)); overflow: hidden; }
.hero-rest { position: absolute; top: 62vh; left: 0; width: 1px; height: 1px; }   /* resting point: photos only */
.hero .ov-slider { position: absolute; inset: 0; aspect-ratio: auto; border-radius: 0; }
.hero-shade { position: absolute; inset: 0; pointer-events: none;
  background: linear-gradient(180deg, rgba(8, 20, 14, 0.55) 0%, rgba(8, 20, 14, 0.10) 30%, rgba(8, 20, 14, 0.30) 58%, rgba(8, 20, 14, 0.88) 100%); }
.hero-content { position: absolute; left: 50%; bottom: 17vh; transform: translateX(-50%); width: min(calc(1100 * var(--u)), 90vw); text-align: center;
  will-change: opacity, transform; }
.hero-kicker { font-size: 1rem; opacity: 0.9; margin-bottom: calc(12 * var(--u)); }
.hero-title { font-family: 'Libre Baskerville', Georgia, serif; font-weight: 700; font-size: clamp(2.4rem, 4.6vw, 4.4rem); line-height: 1.15;
  margin: 0; text-shadow: 0 2px calc(24 * var(--u)) rgba(0, 0, 0, 0.35); }
.hero-shade { will-change: opacity; }
.hero-sub { font-size: clamp(1.05rem, 1.5vw, 1.35rem); white-space: nowrap; margin: calc(16 * var(--u)) auto 0; opacity: 0.95; }
.hero-meta { font-size: 0.95rem; margin-top: calc(22 * var(--u)); opacity: 0.85; }
.hero-down { position: absolute; left: 50%; bottom: 4vh; transform: translateX(-50%); color: #FFFFFF; opacity: 0.9; }
.hero-down:hover { color: #FFFFFF; opacity: 1; }
.hero-down svg { display: block; width: calc(46 * var(--u)); height: calc(24 * var(--u)); }
.hero-credit { position: absolute; right: calc(18 * var(--u)); bottom: calc(12 * var(--u)); font-size: 0.72rem; opacity: 0.7; }
@media (prefers-reduced-motion: no-preference) { .hero-down { animation: hero-bob 2.4s ease-in-out infinite; } }
@keyframes hero-bob { 0%, 100% { transform: translate(-50%, 0); } 50% { transform: translate(-50%, calc(6 * var(--u))); } }

/* Sidebar compacting: 50% reduced vertical spacing between categories & inputs */
.sidebar > .sidebar-content { gap: calc(6 * var(--u)) !important; padding: calc(8 * var(--u)) calc(12 * var(--u)) !important; }
.sidebar .sidebar-title { font-size: 1.05rem !important; margin: 0 0 calc(6 * var(--u)) !important; padding-bottom: calc(6 * var(--u)) !important; }
.sidebar .shiny-input-container { margin-bottom: calc(5 * var(--u)) !important; }
.sidebar .form-group { margin-bottom: calc(5 * var(--u)) !important; }
.sidebar .control-label { margin-bottom: 2px !important; font-size: 0.82rem !important; font-weight: 500; color: #1E2B22; }
.sidebar hr { margin: calc(6 * var(--u)) 0 !important; border-top: 1px solid #CFE3BE !important; opacity: 1; }
/* Sliders: expanded spacing (+50%) so numbers, grid ticks and values are completely clear and sharp */
.sidebar .shiny-input-container:has(.irs--shiny) { margin-bottom: calc(12 * var(--u)) !important; }
.sidebar .irs--shiny { margin-bottom: calc(4 * var(--u)) !important; }
/* Narrower track: the value labels over the two handles are wider than
   the handle (e.g. S$1,400), so they need room on both sides instead of running into the sidebar edge */
.sidebar .irs--shiny { margin-left: calc(22 * var(--u)) !important; margin-right: calc(22 * var(--u)) !important; }
.sidebar .irs--shiny.irs-with-grid { height: calc(52 * var(--u)) !important; }
.sidebar .irs--shiny .irs-line { top: calc(22 * var(--u)) !important; height: calc(7 * var(--u)) !important; border-radius: calc(4 * var(--u)) !important; background: #E2E8F0 !important; }
.sidebar .irs--shiny .irs-bar { top: calc(22 * var(--u)) !important; height: calc(7 * var(--u)) !important; background: #0D743B !important; }
.sidebar .irs--shiny .irs-handle, .sidebar .irs--shiny .irs-slider { top: calc(16 * var(--u)) !important; width: calc(18 * var(--u)) !important; height: calc(18 * var(--u)) !important; border-radius: 50% !important; background: #2E6930 !important; box-shadow: 0 2px calc(5 * var(--u)) rgba(0,0,0,0.2) !important; cursor: pointer !important; }
.sidebar .irs--shiny .irs-min, .sidebar .irs--shiny .irs-max { top: 0 !important; font-size: 0.70rem !important; color: #808285 !important; line-height: 1.2 !important; }
.sidebar .irs--shiny .irs-single, .sidebar .irs--shiny .irs-from, .sidebar .irs--shiny .irs-to { top: -2px !important; font-size: 0.72rem !important; font-weight: 600 !important; background-color: #0D743B !important; color: #FFFFFF !important; padding: 1px calc(5 * var(--u)) !important; border-radius: calc(4 * var(--u)) !important; line-height: 1.25 !important; }
.sidebar .irs--shiny .irs-grid { top: calc(28 * var(--u)) !important; height: calc(22 * var(--u)) !important; }
.sidebar .irs--shiny .irs-grid-pol { background-color: #94A3B8 !important; height: calc(5 * var(--u)) !important; }
.sidebar .irs--shiny .irs-grid-pol.small { height: calc(3 * var(--u)) !important; }
.sidebar .irs--shiny .irs-grid-text { bottom: 0 !important; font-size: 0.68rem !important; color: #475569 !important; font-weight: 500 !important; }
.sidebar .checkbox { margin-top: 2px !important; margin-bottom: calc(3 * var(--u)) !important; }
.sidebar .checkbox label { font-size: 0.80rem; }
.sidebar #t1_count { margin-top: 2px !important; margin-bottom: calc(4 * var(--u)) !important; font-size: 0.74rem !important; }
.sidebar .btn { padding: var(--phi-1) var(--phi-2) !important; font-size: 0.8rem !important; }

/* Sticky floating map: stays fixed & elevated until scrolled past reset filter */
.map-sticky-wrap { position: relative; margin-bottom: calc(14 * var(--u)); }
.map-floating-card { position: sticky; top: calc(72 * var(--u)); z-index: 95; border-radius: calc(8 * var(--u));
  box-shadow: 0 calc(12 * var(--u)) calc(30 * var(--u)) rgba(13, 116, 59, 0.16), 0 calc(4 * var(--u)) calc(12 * var(--u)) rgba(64, 64, 65, 0.10) !important;
  transition: box-shadow 0.25s ease; }

/* Map filters: bslib's own collapse toggle hides or shows them. Open: a round white button with a green
   left arrow inside the sidebar at the end of the Filters title (kept off the edge, where bslib's drag-to-resize
   handle sits, so the two never compete for the pointer). Closed: a green tab on the left edge
   reading Filters with a right arrow. Both fill green on hover. (Phones keep bslib's own toggle.) */
@media (min-width: 576px) {
  #sec-map .bslib-sidebar-layout { position: relative; }
  #sec-map .bslib-sidebar-layout > .collapse-toggle {
    position: absolute; top: calc(10 * var(--u)); left: calc(var(--_sidebar-width) - 44 * var(--u)); right: auto; bottom: auto;
    display: inline-flex; flex-direction: column; align-items: center; justify-content: center; gap: calc(8 * var(--u));
    width: calc(30 * var(--u)); height: calc(30 * var(--u)); min-height: 0; padding: 0; margin: 0; border-radius: calc(999 * var(--u));
    border: 1px solid #0D743B; background: #FFFFFF; color: #0D743B; opacity: 1; z-index: 1100; cursor: pointer;
    box-shadow: 0 calc(3 * var(--u)) calc(10 * var(--u)) rgba(13, 116, 59, 0.18), 0 1px calc(2 * var(--u)) rgba(64, 64, 65, 0.12);
    transition: left var(--_transition-duration, 0.3s) var(--_transition-easing, ease), background 0.15s, color 0.15s, box-shadow 0.15s; }
  #sec-map .bslib-sidebar-layout > .collapse-toggle > .collapse-icon { opacity: 1; width: calc(15 * var(--u)); height: calc(15 * var(--u)); }
  #sec-map .bslib-sidebar-layout > .collapse-toggle:hover, #sec-map .bslib-sidebar-layout > .collapse-toggle:focus-visible {
    background: #0D743B; color: #FFFFFF; outline: none; box-shadow: 0 0 0 calc(3 * var(--u)) rgba(13, 116, 59, 0.2); }
  /* Closed: bslib gives the toggle a fixed height, so the tab sets its own (label + arrow, never clipped) */
  #sec-map .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle {
    left: calc(10 * var(--u)); height: auto !important; max-height: none !important; overflow: visible;
    width: calc(34 * var(--u)); padding: calc(14 * var(--u)) 0 calc(12 * var(--u)); gap: calc(10 * var(--u));
    border-radius: calc(17 * var(--u)); background: #0D743B; color: #FFFFFF; }
  #sec-map .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle:hover { background: #0A5C2F; }
  #sec-map .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle::before {
    content: 'Filters'; display: block; flex: none; writing-mode: vertical-rl; transform: rotate(180deg);
    font-size: 0.8rem; font-weight: 600; letter-spacing: 0.04em; line-height: 1; white-space: nowrap; }
  #sec-map .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle > .collapse-icon { flex: none; }
  /* Room for the closed tab beside the headline numbers and the map */
  #sec-map .bslib-sidebar-layout.sidebar-collapsed > .main { padding-left: calc(58 * var(--u)) !important; }
}
.bslib-sidebar-layout > .main { padding: calc(8 * var(--u)) calc(16 * var(--u)) calc(20 * var(--u)) !important; }

/* Boxes: no borders, soft outer shadow, compact & balanced spacing */
.card, .bslib-value-box { border: none !important; border-radius: calc(7 * var(--u)); background: #FFFFFF;
  box-shadow: 0 calc(4 * var(--u)) calc(12 * var(--u)) rgba(13, 116, 59, 0.08), 0 1px calc(3 * var(--u)) rgba(64, 64, 65, 0.06) !important; }
/* Spacing scale in golden-ratio steps (each is 1.618 times the one before): 8, 13, 21 px. Buttons: 8 px above and
   below the label, 13 px at the sides (13 / 8 is 1.625), 8 px between icon and label; 13 px between buttons; 21 px from
   the card edge to the nearest button and text, the same on every side. */
:root { --phi-1: calc(8 * var(--u)); --phi-2: calc(13 * var(--u)); --phi-3: calc(21 * var(--u)); }
.card > .card-header, .card > .card-footer { background: #FFFFFF; border: none; padding: var(--phi-3) !important; gap: var(--phi-2); }
/* Action buttons: one shape (pill), one size and one rhythm everywhere (header, sidebar, centre card, quick starts) */
.btn.btn-sm { border-radius: calc(999 * var(--u)); padding: var(--phi-1) var(--phi-2); font-size: 0.8rem; font-weight: 600; line-height: 1.3;
  display: inline-flex; align-items: center; justify-content: center; gap: var(--phi-1); }
.btn.btn-sm i, .btn.btn-sm .fa, .btn.btn-sm svg { margin: 0 !important; }
.card-header .d-flex { gap: var(--phi-2) !important; }
.bslib-value-box { min-height: unset !important; height: auto !important; margin-bottom: 0 !important; }
.bslib-value-box > .card-body { padding: 0 !important; margin: 0 !important; }
.bslib-value-box .value-box-grid { grid-template-columns: 1fr !important; gap: 0 !important; }
.bslib-value-box .value-box-area { padding: calc(8 * var(--u)) calc(12 * var(--u)) !important; display: flex !important; flex-direction: column !important; justify-content: center !important; gap: 2px !important; }
.bslib-value-box .value-box-title { font-size: 0.72rem !important; font-weight: 500 !important; color: #64748B !important; line-height: 1.2 !important; min-height: 2.1em !important; margin: 0 0 1px !important; display: flex !important; align-items: flex-start !important; }
.bslib-value-box .value-box-value { font-family: 'IBM Plex Sans', sans-serif !important; font-weight: 600 !important; font-size: 1.35rem !important; color: #1E2B22 !important; line-height: 1.15 !important; margin: 1px 0 !important; white-space: nowrap !important; }
.vb-sub { font-size: 0.70rem !important; color: #64748B !important; margin: 0 !important; line-height: 1.2 !important; min-height: 1.2em !important; white-space: nowrap !important; overflow: hidden !important; text-overflow: ellipsis !important; }
.mono { font-family: 'IBM Plex Mono', monospace !important; }
.card.exhibit[data-code='V1+V2'] > .card-body { padding: calc(12 * var(--u)) calc(14 * var(--u)); gap: calc(4 * var(--u)); }
#sec-map .bslib-grid { gap: calc(10 * var(--u)) !important; margin-bottom: calc(10 * var(--u)) !important; }
#sec-map .bslib-grid .bslib-grid-item { min-height: unset !important; }

/* Photo slider: the track holds the photos side by side and slides left */
.ov-slider { position: relative; overflow: hidden; background: #1E2B22; }
.ov-track { display: flex; height: 100%; transition: transform 1.1s cubic-bezier(0.65, 0, 0.35, 1); will-change: transform; }
.ov-slide { flex: 0 0 100%; width: 100%; height: 100%; object-fit: cover; object-position: center bottom; display: block; user-select: none; }

/* Page body below the opening photos (the filters sit beside the map only) */
.page-main { padding: calc(28 * var(--u)) calc(20 * var(--u)) calc(24 * var(--u)); }

/* Data visualization: one row per chart, chart left and story right (mirrored McKinsey layout).
   The story fills its column and both sides of the row get the same relative margin, so chart and text sit centred */
/* Each row fills one screen below the header, so a chart and its story are seen on their own;
   scrolling settles on a row (proximity snap: free scrolling elsewhere, e.g. in the map) */
.viz-rows { margin-top: calc(4 * var(--u)); }
.viz-row { display: grid; grid-template-columns: minmax(0, 1.65fr) minmax(0, 1fr); gap: calc(56 * var(--u)); align-items: center;
  min-height: calc(100vh - calc(64 * var(--u))); box-sizing: border-box; padding: calc(40 * var(--u)) calc(64 * var(--u)); border-top: 1px solid #E3E8EB;
  scroll-snap-align: start; scroll-margin-top: calc(64 * var(--u)); }
.viz-rows > .viz-row:first-child { border-top: none; }
/* The first chart of a section starts right under the section heading, not in the middle of a full screen */
.viz-rows > .viz-row:first-child { align-content: start; padding-top: calc(8 * var(--u)); }
.viz-chart .exhibit { margin: 0; }
/* Exhibits in the Data visualization rows: heading on the left, then one labelled toolbar (search box or
   dropdown, map-selection chip, and the Add to comparison button at the right edge) above the chart, then the chart */
.viz-toolbar { display: flex; align-items: center; justify-content: flex-start; gap: calc(10 * var(--u)); flex-wrap: nowrap;
  margin: 0 0 calc(16 * var(--u)); padding: calc(4 * var(--u)) 0 calc(16 * var(--u)); border-bottom: 1px solid #EDF0F2; }
.viz-toolbar > * { flex: none; }
/* On a narrow screen the toolbar may wrap */
@media (max-width: 1100px) { .viz-toolbar { flex-wrap: wrap; } }
.viz-toolbar-label { font-family: 'IBM Plex Mono', monospace; font-size: 0.72rem; letter-spacing: 0.08em;
  text-transform: uppercase; color: #0D743B; }
/* The Centre search box and the Cuisine dropdown of every chart: same width, height, type size and icon position */
.viz-toolbar .map-search, .viz-toolbar .viz-tool { position: relative; top: auto; left: auto; right: auto; width: calc(280 * var(--u)); max-width: 100%; }
.viz-toolbar .map-search-box { box-shadow: none; border: 1px solid #D5DBDF; }
.viz-toolbar .map-search-box:focus-within { border-color: #0D743B; box-shadow: 0 0 0 calc(3 * var(--u)) rgba(13, 116, 59, 0.15); }
/* Dropdown inside the same box as the search */
.viz-select-box .form-group { margin: 0; width: 100%; }
.viz-select-box select { width: 100%; border: none; outline: none; box-shadow: none; background-color: transparent;
  padding: calc(6 * var(--u)) calc(28 * var(--u)) calc(6 * var(--u)) calc(32 * var(--u)); border-radius: calc(6 * var(--u)); font-size: 0.8rem; font-family: inherit; color: #1E2B22; cursor: pointer; }
.viz-toolbar .map-search-icon { left: calc(11 * var(--u)); font-size: 0.78rem; }
.viz-toolbar .map-search-input { font-size: 0.8rem; padding: calc(6 * var(--u)) calc(28 * var(--u)) calc(6 * var(--u)) calc(32 * var(--u)); text-overflow: ellipsis; }
.viz-select-box select:focus { box-shadow: none; }
/* Chip naming the centre picked on the map (V4, V7, V8) */
.viz-chip { display: inline-flex; align-items: center; gap: calc(6 * var(--u)); font-size: 0.82rem; color: #404041;
  background: #F1F4F2; border-radius: calc(999 * var(--u)); padding: calc(6 * var(--u)) calc(12 * var(--u)); }
.viz-chip b { color: #1E2B22; font-weight: 600; }
.viz-chip-dot { flex: none; width: calc(9 * var(--u)); height: calc(9 * var(--u)); border-radius: 50%; background: #FAA619; box-shadow: 0 0 0 2px #FFFFFF, 0 0 0 calc(3.5 * var(--u)) #1E2B22; }
.viz-chip-off, .viz-chip-off b { color: #808285; font-weight: 400; }
/* The chip takes what is left of the row and shortens long names with an ellipsis, so every toolbar fits on one line */
.viz-toolbar .shiny-html-output:has(.viz-chip) { flex: 0 1 auto; min-width: 0; }
.viz-toolbar .viz-chip { max-width: 100%; min-width: 0; white-space: nowrap; }
.viz-toolbar .viz-chip b { min-width: 0; overflow: hidden; text-overflow: ellipsis; }
/* Facts about the centre picked on the map, under a chart's story (V8) */
/* Facts about the centre picked on the map (V8): a title with its zone tag, then a table in the same style as
   the other tables (light green header with a dark green rule), the centre next to the median centre */
.viz-detail { margin-top: var(--phi-3); }
.viz-detail-title { display: flex; align-items: center; flex-wrap: wrap; gap: var(--phi-1); font-size: 1.05rem; font-weight: 600; color: #1E2B22; margin-bottom: var(--phi-2); }
.viz-tag { font-size: 0.72rem; font-weight: 600; padding: calc(3 * var(--u)) calc(10 * var(--u)); border-radius: calc(999 * var(--u)); background: #E8F2DE; color: #0D743B; }
.viz-tag.viz-tag-off { background: #EDF0F2; color: #58595B; }
.viz-table { width: 100%; border-collapse: collapse; font-size: 0.86rem; }
.viz-table thead th { background: #E8F2DE; color: #0D743B; font-weight: 600; text-align: left; white-space: nowrap;
  padding: var(--phi-1) var(--phi-2); border-bottom: 2px solid #0D743B; }
.viz-table tbody th, .viz-table tbody td { padding: var(--phi-1) var(--phi-2); border-bottom: 1px solid #E6EAEC; vertical-align: top; text-align: left; }
.viz-table tbody th { font-weight: 400; color: #58595B; width: 36%; }
.viz-table td.v { font-weight: 600; color: #1E2B22; }
.viz-table td.m { color: #58595B; }
.viz-table td small { display: block; font-weight: 400; color: #6B7075; font-size: 0.74rem; line-height: 1.35; }
.viz-detail-note { margin: var(--phi-1) 0 0; color: #6B7075; font-size: 0.76rem; line-height: 1.4; }
/* Add-to-comparison button (every chart toolbar and the centre card): a green outlined pill, filled on
   hover; a quiet tag once the centre is added. In a toolbar it sits at the right edge, so the buttons line
   up from chart to chart; in the centre card it spans the card. */
.viz-toolbar .viz-add-slot { margin-left: auto; }
.viz-add { display: inline-flex; align-items: center; justify-content: center; gap: var(--phi-1); padding: var(--phi-1) var(--phi-2); border-radius: calc(999 * var(--u));
  border: 1px solid #0D743B; background: #FFFFFF; color: #0D743B; font-family: inherit; font-size: 0.8rem; font-weight: 600;
  line-height: 1.3; cursor: pointer; box-shadow: none; transition: background 0.15s, color 0.15s, border-color 0.15s; }
.viz-add i { font-size: 0.68rem; }
.viz-add:hover, .viz-add:focus-visible { background: #0D743B; color: #FFFFFF; border-color: #0D743B; outline: none; }
.viz-add:focus-visible { box-shadow: 0 0 0 calc(3 * var(--u)) rgba(13, 116, 59, 0.2); }
.viz-add-done { background: #EEF5E6; border-color: #BBD980; color: #4E5D68; }
/* In comparison: hover or focus turns it into a red Remove button */
.viz-add-done .va-in, .viz-add-done .va-rm { display: inline-flex; align-items: center; gap: var(--phi-1); }
.viz-add-done .va-rm { display: none; }
.viz-add-done:hover, .viz-add-done:focus-visible { background: #FFFFFF; border-color: #B03A2E; color: #B03A2E; box-shadow: none; }
.viz-add-done:hover .va-in, .viz-add-done:focus-visible .va-in { display: none; }
.viz-add-done:hover .va-rm, .viz-add-done:focus-visible .va-rm { display: inline-flex; }
.cc-add-slot { margin-top: calc(6 * var(--u)); }
.cc-add-slot .viz-add { width: 100%; }
.viz-label { display: none; font-family: 'IBM Plex Mono', monospace; font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase;
  color: #0D743B; margin-bottom: calc(10 * var(--u)); }
.viz-copy { font-size: 1.05rem; line-height: 1.75; color: #404041; margin: 0; }
.viz-copy strong { color: #1E2B22; font-weight: 600; }
/* A story told in separate paragraphs (V4): each one on its own, with space between */
.viz-para { display: block; }
.viz-para + .viz-para, .viz-para-gap { margin-top: 0.85em; }
@media (max-width: 1100px) { .viz-row { grid-template-columns: 1fr; gap: 16px; } }

/* About us: text and team on the left, the small feedback panel on the right */
.about-grid { display: grid; grid-template-columns: minmax(0, 1.6fr) minmax(0, 1fr); gap: calc(48 * var(--u)); align-items: start;
  max-width: calc(1400 * var(--u)); margin: 0 auto; padding-bottom: calc(40 * var(--u)); }
@media (max-width: 1100px) { .about-grid { grid-template-columns: 1fr; } }
.about-lead { font-size: 1.1rem; line-height: 1.75; color: #404041; margin: 0 0 calc(22 * var(--u)); }
.about-label { font-family: 'IBM Plex Mono', monospace; font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase;
  color: #0D743B; margin: calc(18 * var(--u)) 0 calc(10 * var(--u)); }
.team-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(calc(200 * var(--u)), 1fr)); gap: calc(10 * var(--u)); }
.team-card { background: #FFFFFF; border-radius: calc(7 * var(--u)); padding: calc(12 * var(--u)) calc(14 * var(--u)); box-shadow: 0 calc(4 * var(--u)) calc(12 * var(--u)) rgba(13, 116, 59, 0.08), 0 1px calc(3 * var(--u)) rgba(64, 64, 65, 0.06); }
.team-name { font-weight: 600; color: #1E2B22; }
.team-role { font-size: 0.85rem; color: #58595B; margin-top: 2px; }
.about-note { font-size: 0.92rem; line-height: 1.6; color: #58595B; margin: 0; }
/* Feedback panel: everything centred around the star rating */
.feedback-card > .card-body { padding: calc(30 * var(--u)) calc(32 * var(--u)); align-items: center; text-align: center; gap: 0; }
.feedback-card hr { align-self: stretch; margin: calc(24 * var(--u)) 0 calc(22 * var(--u)); border-color: #E3E8EB; opacity: 1; }
.feedback-title { font-weight: 600; font-size: 1.15rem; color: #1E2B22; }
.feedback-hint { font-size: 0.88rem; color: #808285; margin: calc(6 * var(--u)) 0 calc(14 * var(--u)); }
/* Star rating: stars are written 5..1 and shown right-to-left, so hovering a star lights it and those before it */
.star-rating { display: inline-flex; flex-direction: row-reverse; justify-content: center; gap: calc(8 * var(--u)); margin: calc(14 * var(--u)) auto 0; }
.star-rating input { position: absolute; opacity: 0; pointer-events: none; }
.star-rating label { font-size: 2.6rem; line-height: 1; color: #D9DEE3; cursor: pointer; transition: color 0.15s ease, transform 0.15s ease; }
.star-rating label:hover, .star-rating label:hover ~ label, .star-rating input:checked ~ label { color: #FAA619; }
.star-rating label:hover { transform: scale(1.12); }
.star-rating input:focus-visible + label { outline: 2px solid #0D743B; outline-offset: 2px; border-radius: calc(4 * var(--u)); }
.subscribe-row { display: flex; gap: calc(8 * var(--u)); align-self: stretch; }
.subscribe-input { flex: 1; min-width: 0; }
.subscribe-input:focus { border-color: #40AD49; box-shadow: 0 0 0 calc(3 * var(--u)) rgba(64, 173, 73, 0.18); }
.subscribe-btn { flex: none; padding: 0 calc(24 * var(--u)); background: #0D743B; border-color: #0D743B; color: #FFFFFF; font-weight: 600; }
.subscribe-btn:hover, .subscribe-btn:focus { background: #40AD49; border-color: #40AD49; color: #FFFFFF; }

/* Comparison: the section fills one screen below the header; the table card stretches to the
   bottom and its content (the empty-shortlist message or the table) sits in the middle */
#sec-compare { min-height: calc(100vh - calc(64 * var(--u))); display: flex; flex-direction: column; box-sizing: border-box; padding-bottom: calc(24 * var(--u)); }
#sec-compare > .card { flex: 1 1 auto; }
/* About us (last section) is at least one screen tall, so the menu can bring its rule under the header too */
#sec-about { min-height: calc(100vh - calc(64 * var(--u))); }
/* The table always starts at the top of the card; only the empty-shortlist message is centred */
#sec-compare > .card > .card-body { display: flex; flex-direction: column; justify-content: flex-start; }
#compare_ui { flex: 1 1 auto; display: flex; flex-direction: column; }
#compare_ui > .compare-empty { margin: auto 0; }

/* Map section: the filters are a sidebar inside this section only */
#sec-map .bslib-sidebar-layout { border: none; }

/* Dot navigation on the right edge (app_js): visible while scrolling, faded out when the page is still */
/* Top-right corner, under the header: one round cell per part, all on one vertical bar.
   A click on a cell goes straight to that part. */
/* --dn = the navigation's own unit: 25% larger than the page unit (user's request) */
.dot-nav { --dn: calc(1.25 * var(--u)); position: fixed; right: calc(18 * var(--u)); top: calc(84 * var(--u)); z-index: 1040;
  display: flex; flex-direction: column; gap: calc(22 * var(--dn)); padding: calc(14 * var(--dn)) calc(16 * var(--dn));
  opacity: 0; transition: opacity 0.35s ease; pointer-events: none; }
.dot-nav.show, .dot-nav:hover, .dot-nav:focus-within { opacity: 1; pointer-events: auto; }
/* the bar runs through the cells' centres: padding (16) + half a cell (13) - half the bar (1.5) */
.dot-nav::before { content: ''; position: absolute; top: calc(26 * var(--dn)); bottom: calc(26 * var(--dn)); right: calc(27.5 * var(--dn));
  width: calc(3 * var(--dn)); background: #DDE3E6; border-radius: calc(3 * var(--dn)); }
.dot-nav a { position: relative; display: flex; align-items: center; justify-content: flex-end; gap: calc(16 * var(--dn));
  min-height: calc(26 * var(--dn)); text-decoration: none; }
.dot-nav .dot { position: relative; z-index: 1; flex: none; width: calc(26 * var(--dn)); height: calc(26 * var(--dn)); border-radius: 50%; box-sizing: border-box;
  background: #FFFFFF; border: 2.5px solid #B8C2C7; box-shadow: 0 calc(2 * var(--dn)) calc(6 * var(--dn)) rgba(30, 43, 34, 0.12);
  transition: transform 0.25s ease, border-color 0.25s ease; }
/* the part in view: green ring with a green centre */
.dot-nav .dot::after { content: ''; position: absolute; inset: calc(5 * var(--dn)); border-radius: 50%; background: transparent; transition: background 0.25s ease; }
.dot-nav a:hover .dot { border-color: #0D743B; transform: scale(1.08); }
.dot-nav a.active .dot { border-color: #0D743B; box-shadow: 0 0 0 calc(5 * var(--dn)) rgba(13, 116, 59, 0.15); }
.dot-nav a.active .dot::after { background: #0D743B; }
/* hovering the column shows every cell's name in a small white pill to its left; only the part in view
   is bold green */
.dot-nav .label { position: absolute; right: calc(38 * var(--dn)); white-space: nowrap; font-size: 1.19rem;
  background: #FFFFFF; color: #58595B; font-weight: 400; padding: calc(5 * var(--dn)) calc(14 * var(--dn));
  border-radius: calc(999 * var(--dn)); box-shadow: 0 calc(4 * var(--dn)) calc(14 * var(--dn)) rgba(30, 43, 34, 0.14);
  opacity: 0; transform: translateX(calc(6 * var(--dn))); transition: opacity 0.2s ease, transform 0.2s ease; pointer-events: none; }
.dot-nav.peek .label, .dot-nav:hover .label, .dot-nav a.flash .label { opacity: 1; transform: none; }
.dot-nav a:hover .label { color: #1E2B22; }
.dot-nav a.active .label, .dot-nav a.active:hover .label { color: #0D743B; font-weight: 700; }
/* not over the open menu or an expanded card */
body:has(.bslib-card[data-full-screen='true']) .dot-nav { display: none; }
@media (prefers-reduced-motion: reduce) { .dot-nav, .dot-nav * { transition: none !important; } }
@media (max-width: 700px) { .dot-nav { right: calc(8 * var(--u)); } }

/* McKinsey-style sections */
/* Jumping to a section (menu, snap) leaves its rule 16 px under the 64 px header: 64 - 40 (head padding) + 16 */
.mck-section { scroll-margin-top: calc(40 * var(--u)); margin-bottom: calc(28 * var(--u)); scroll-snap-align: start; }
#sec-map.mck-section { margin-bottom: calc(13.6 * var(--u)); }
/* Section heading (as on MGI pages): a centred rule, then the title and one grey line, centred */
.sec-head { text-align: center; padding: calc(40 * var(--u)) 0 calc(32 * var(--u)); }
.sec-rule { width: min(calc(860 * var(--u)), 56%); height: 1px; background: #A7ADB2; margin: 0 auto calc(52 * var(--u)); }
.sec-title { font-family: 'Libre Baskerville', Georgia, serif; font-size: clamp(1.9rem, 2.8vw, 2.7rem); font-weight: 700; color: #1E2B22;
  margin: 0 0 calc(18 * var(--u)); line-height: 1.2; }
.sec-kicker { font-size: 1.15rem; color: #58595B; margin: 0 auto; max-width: calc(820 * var(--u)); line-height: 1.45; }
#sec-map .sec-kicker { max-width: none; white-space: nowrap; }   /* the map section's line stays on one line */

/* McKinsey-style exhibits: label, takeaway headline, subtitle, chart, source */
.exhibit > .card-body { padding: calc(22 * var(--u)) calc(26 * var(--u)); gap: calc(8 * var(--u)); }
.ex-headline { font-size: 1.35rem; font-weight: 600; color: #1E2B22; line-height: 1.3; margin-bottom: calc(4 * var(--u)); }
.ex-subtitle { font-size: 0.95rem; color: #404041; margin-bottom: calc(14 * var(--u)); }
.ex-subtitle b { color: #1E2B22; }
.ex-source { font-size: 0.75rem; font-style: italic; color: #808285; margin-top: 0; line-height: 1.35; }
/* Chart hover cards float above the chart: a soft shadow under plotly's white card (SVG, so drop-shadow) */
.js-plotly-plot .hoverlayer .hovertext { filter: drop-shadow(0 6px 14px rgba(30, 43, 34, 0.18)) drop-shadow(0 1px 3px rgba(30, 43, 34, 0.12)); }
/* Short notes under a chart, just above its source line */
.ex-note { font-size: 0.78rem; color: #58595B; line-height: 1.4; margin: var(--phi-1) 0 calc(4 * var(--u)); }
.ex-note b { color: #404041; font-weight: 600; }
/* Small-sample caution in a chart story (V9) */
.viz-caution { display: flex; align-items: flex-start; gap: var(--phi-1); margin-top: var(--phi-2); padding: var(--phi-1) var(--phi-2);
  border-radius: calc(8 * var(--u)); background: #FFF6E5; color: #6B4A0E; font-size: 0.82rem; line-height: 1.45; }
.viz-caution .fa, .viz-caution .fas, .viz-caution svg { flex: none; margin-top: 0.2em; color: #FAA619; }

/* Map */
.leaflet-container { background: #EDF0F2; border-radius: calc(6 * var(--u)); }
/* Expanded map card: the map takes all the height left after the legend and notes */
.bslib-card[data-full-screen='true'] > .card-body { display: flex; flex-direction: column; overflow: hidden; flex: 1 1 0; min-height: 0; }
.bslib-card[data-full-screen='true'] .map-wrap { flex: 1 1 auto; min-height: 0; }
/* Expanded card shrinking away before the page scrolls to a link's target (app_js) */
.bslib-card[data-full-screen='true'].fs-closing { transform: scale(0.92); opacity: 0; transition: transform 0.26s ease, opacity 0.26s ease; }
.bslib-card[data-full-screen='true'] #map { position: absolute !important; inset: 0 !important; height: auto !important; width: 100% !important; }
.bslib-card[data-full-screen='true'] .map-legend-below, .bslib-card[data-full-screen='true'] .map-note,
.bslib-card[data-full-screen='true'] .ex-source { flex: none; }
.map-legend-below .info-title { font-weight: 600; font-size: 0.85rem; }
.legend-row { display: flex; align-items: center; gap: calc(6 * var(--u)); font-size: 0.8rem; line-height: 1.3; margin-bottom: calc(4 * var(--u)); }
.legend-swatch { width: calc(18 * var(--u)); height: calc(12 * var(--u)); border-radius: calc(3 * var(--u)); border: 1px solid #C8CDD2; flex: none; }
.legend-dot { width: calc(12 * var(--u)); height: calc(12 * var(--u)); border-radius: 50%; background: #FAA619; flex: none; }
.mrt-icon { width: calc(16 * var(--u)); height: calc(16 * var(--u)); flex: none; vertical-align: calc(-3 * var(--u)); margin-right: 2px; }
.leaflet-marker-icon.leaflet-interactive { filter: drop-shadow(0 1px 2px rgba(30, 43, 34, 0.35)); }
.map-legend-below > div { display: flex; flex-wrap: wrap; align-items: center; gap: 0 calc(14 * var(--u)); margin-top: 0; }
.map-legend-below .info-title { width: 100%; margin: 0; line-height: 1.25; }
.map-legend-below .legend-row { margin-bottom: 0; line-height: 1.25; }
/* Note and source under the legend: tight lines, small gaps */
.map-note { margin: 0 !important; line-height: 1.25; }
.map-floating-card .ex-source { line-height: 1.25; }
.centre-card-box { border-radius: calc(6 * var(--u)); padding: calc(8 * var(--u)) calc(10 * var(--u)); background: #FFFFFF;
  box-shadow: 0 calc(4 * var(--u)) calc(14 * var(--u)) rgba(13, 116, 59, 0.16), 0 1px calc(3 * var(--u)) rgba(64, 64, 65, 0.12); }
/* Map spans the card; the centre card floats over its top-right corner.
   z-index 0 on the wrapper keeps the map and card below the sticky header. */
.map-wrap { position: relative; z-index: 0; }
/* The card's width follows the map's (26% of it), so names and tile labels stay on one or two lines */
.map-overlay { position: absolute; top: calc(12 * var(--u)); right: calc(12 * var(--u)); width: clamp(calc(280 * var(--u)), 26%, calc(420 * var(--u))); z-index: 950; }
.centre-card-box .info-title { font-size: 0.70rem; margin: 0 0 calc(5.6 * var(--u)); }
/* Centre search box over the map's top-left corner; zoom buttons move below it */
.map-search { position: absolute; top: calc(12 * var(--u)); left: calc(12 * var(--u)); width: calc(320 * var(--u)); z-index: 960; }
.map-search-box { position: relative; display: flex; align-items: center; background: #FFFFFF; border-radius: calc(6 * var(--u));
  box-shadow: 0 calc(4 * var(--u)) calc(14 * var(--u)) rgba(13, 116, 59, 0.16), 0 1px calc(3 * var(--u)) rgba(64, 64, 65, 0.12); }
.map-search-box:focus-within { box-shadow: 0 0 0 2px #0D743B, 0 calc(4 * var(--u)) calc(14 * var(--u)) rgba(13, 116, 59, 0.16); }
.map-search-icon { position: absolute; left: calc(13 * var(--u)); top: 50%; transform: translateY(-50%); z-index: 2;
  color: #808285; pointer-events: none; }
.map-search-input { width: 100%; border: none; outline: none; padding: calc(9 * var(--u)) calc(32 * var(--u)) calc(9 * var(--u)) calc(36 * var(--u)); border-radius: calc(6 * var(--u));
  font-size: 0.9rem; font-family: inherit; color: #1E2B22; background: transparent; }
.map-search-clear { position: absolute; right: calc(8 * var(--u)); top: 50%; transform: translateY(-50%); border: none;
  background: transparent; color: #808285; cursor: pointer; padding: 2px calc(6 * var(--u)); font-size: 0.85rem; display: none; }
.map-search-clear:hover { color: #1E2B22; }
.map-search-dropdown { position: absolute; top: calc(100% + calc(4 * var(--u))); left: 0; right: 0; background: #FFFFFF;
  border-radius: calc(6 * var(--u)); box-shadow: 0 calc(6 * var(--u)) calc(18 * var(--u)) rgba(30, 43, 34, 0.18); max-height: calc(280 * var(--u)); overflow-y: auto;
  z-index: 1003; display: none; }
.map-search-item { padding: calc(8 * var(--u)) calc(12 * var(--u)); cursor: pointer; border-bottom: 1px solid #F6F8F9; }
.map-search-item:last-child { border-bottom: none; }
.map-search-item:hover, .map-search-item.active { background: #E8F2DE; color: #0D743B; }
.map-search-item-name { font-weight: 600; font-size: 0.86rem; color: #1E2B22; line-height: 1.25; }
.map-search-item:hover .map-search-item-name { color: #0D743B; }
.map-search-item-pa { font-size: 0.74rem; color: #58595B; margin-top: 1px; }
.map-search-empty { padding: calc(12 * var(--u)); font-size: 0.82rem; color: #808285; text-align: center; }
.map-wrap .leaflet-top.leaflet-left { top: calc(56 * var(--u)); }

/* McKinsey-style tooltips on the map */
.leaflet-map-pane { z-index: auto !important; }
.leaflet-pane.leaflet-tooltip-pane { z-index: 1005 !important; }
.leaflet-tooltip.mck-tt { background: #FFFFFF; border: none; border-radius: calc(6 * var(--u)); padding: calc(10 * var(--u)) calc(12 * var(--u));
  box-shadow: 0 calc(6 * var(--u)) calc(18 * var(--u)) rgba(30, 43, 34, 0.18); font-family: 'IBM Plex Sans', sans-serif; color: #404041;
  z-index: 1005 !important; pointer-events: none; }
.leaflet-tooltip.mck-tt::before { display: none; }
/* Leaflet's own copy is never shown: only the floating copy below is (two cards appeared when the map
   redrew its circles under the mouse, because the script hid the original only when it opened) */
.leaflet-tooltip.mck-tt { visibility: hidden !important; }
/* Floating copy of the map tooltip, above everything (incl. the centre card) */
.float-tip { position: fixed; z-index: 2000; display: none; pointer-events: none; max-width: calc(320 * var(--u));
  background: #FFFFFF; border-radius: calc(6 * var(--u)); padding: calc(10 * var(--u)) calc(12 * var(--u)); box-shadow: 0 calc(6 * var(--u)) calc(18 * var(--u)) rgba(30, 43, 34, 0.18);
  font-family: 'IBM Plex Sans', sans-serif; font-size: 0.85rem; color: #404041; }
.tt-title { font-weight: 600; color: #1E2B22; font-size: 0.9rem; }
.tt-big { font-family: 'IBM Plex Sans', sans-serif; font-size: 1.7rem; color: #0D743B; line-height: 1.1; margin-top: 2px; }
.tt-sub { font-size: 0.78rem; color: #58595B; }
.tt-meta { font-size: 0.78rem; color: #404041; margin-top: calc(6 * var(--u)); padding-top: calc(6 * var(--u)); border-top: 1px solid #EDF0F2; }

/* Centre table: header in the app's light green with a dark green rule */
table.dataTable thead th { background: #E8F2DE !important; color: #0D743B; font-weight: 600; border-bottom: 2px solid #0D743B !important; }
/* Centre table title: the same size and weight as the chart headlines (e.g. Cuisine supply at ...) */
.table-card > .card-header { background: transparent; border-bottom: none; padding: var(--phi-3) var(--phi-3) var(--phi-1) !important; }
.table-title { margin-bottom: 0; }
/* Centre table tabs (All centres / Ranking) */
.table-card .card-header .nav-link { font-weight: 600; color: #58595B; font-size: 0.92rem; }
.table-card .card-header .nav-link.active { color: #0D743B; border-bottom-color: #0D743B; }
.table-card .card-header .bslib-navs-card-title { margin-right: var(--phi-3); }
.rank-budget input { width: 100%; border: none; outline: none; box-shadow: none; background: transparent;
  padding: calc(6 * var(--u)) calc(10 * var(--u)) calc(6 * var(--u)) calc(32 * var(--u)); font-size: 0.8rem; color: #1E2B22; }
.rank-budget input:focus { box-shadow: none; }
.viz-toolbar .rank-budget { width: calc(170 * var(--u)); }
.rank-reset { border-radius: calc(999 * var(--u)); font-size: 0.75rem; }
.table-card .viz-toolbar { flex-wrap: wrap; row-gap: calc(8 * var(--u)); margin-bottom: calc(8 * var(--u)); padding: 0 0 calc(8 * var(--u)); }
/* Ranking: toolbar, legend, distance line and chart close together */
.table-card .card-body, .table-card .tab-pane { gap: calc(6 * var(--u)) !important; }
.table-card .tab-pane > * { margin-top: 0; margin-bottom: 0; }
.table-card .rank-legend { margin-bottom: 0; }
.table-card .rank-dist-note { margin: 0; }
/* Ranking rows (HTML): name | stacked bar out of 10 | score circle | Add to comparison */
.rk-chart { margin-top: calc(8 * var(--u)); align-items: center; display: grid; grid-template-columns: max-content 1fr calc(210 * var(--u)); row-gap: calc(10 * var(--u)); column-gap: calc(14 * var(--u)); }
.rk-row { display: contents; }   /* the cells of every row share the chart's three columns: names line up */
.rk-name { text-align: right; cursor: pointer; font-size: 0.95rem; color: #404041; white-space: nowrap; align-self: center; }
.rk-name:hover .rk-centre { color: #0D743B; text-decoration: underline; }
.rk-km { color: #808285; margin-left: calc(6 * var(--u)); }
.rk-sel .rk-centre { font-weight: 700; color: #1E2B22; }
.rk-bar { display: flex; align-items: center; height: calc(36 * var(--u)); cursor: pointer; }
.rk-bar .rk-seg { height: calc(30 * var(--u)); }
.rk-bar .rk-score { margin-left: calc(14 * var(--u)); flex: none; }
.rk-seg { position: relative; height: 100%; }
.rk-seg:hover { filter: brightness(1.08); }
.rk-seg:hover::after { content: attr(data-tip); position: absolute; bottom: calc(100% + 8 * var(--u)); left: 50%; transform: translateX(-50%);
  white-space: pre; background: #FFFFFF; color: #1E2B22; font-size: 0.8rem; line-height: 1.4; padding: calc(8 * var(--u)) calc(12 * var(--u));
  border-radius: calc(6 * var(--u)); box-shadow: 0 calc(6 * var(--u)) calc(18 * var(--u)) rgba(30, 43, 34, 0.18); z-index: 5; pointer-events: none; }
.rk-score { width: calc(36 * var(--u)); height: calc(36 * var(--u)); border-radius: 50%; background: #0D743B; color: #FFFFFF;
  font-weight: 700; font-size: 0.85rem; display: inline-flex; align-items: center; justify-content: center; }
/* Ranking rows: Add to comparison shrinks to a round + button; hovering it slides the words out */
/* the + stays put at the right; the words slide out to its left (row-reverse), inside a fixed-width column
   so the bars never move */
.rk-add { white-space: nowrap; display: inline-flex; flex-direction: row-reverse; align-items: center; justify-content: center; justify-self: end;
  min-width: calc(36 * var(--u)); height: calc(36 * var(--u)); padding: 0 calc(10 * var(--u)) !important; border-radius: calc(999 * var(--u)); }
.rk-add .rk-add-label { display: inline-block; max-width: 0; opacity: 0; overflow: hidden; vertical-align: middle;
  transition: max-width 0.25s ease, opacity 0.2s ease, margin 0.25s ease; }
.rk-add:hover .rk-add-label, .rk-add:focus-visible .rk-add-label { max-width: calc(200 * var(--u)); opacity: 1; margin-right: calc(6 * var(--u)); }
.rk-add .rk-ico-rm { display: none; }
.rk-add.viz-add-done:hover .rk-ico-in { display: none; }
.rk-add.viz-add-done:hover .rk-ico-rm { display: inline-block; }
.rank-banner { font-size: 0.9rem; color: #404041; margin: 0 0 var(--phi-1); }
.rank-banner b { color: #1E2B22; }
.rank-legend { display: flex; flex-wrap: wrap; align-items: center; gap: var(--phi-2); font-size: 0.8rem; color: #58595B; margin-bottom: calc(4 * var(--u)); }
.rank-legend > span { display: inline-flex; align-items: center; gap: calc(6 * var(--u)); }
.rank-sw { width: calc(12 * var(--u)); height: calc(12 * var(--u)); border-radius: calc(3 * var(--u)); display: inline-block; }
.rank-legend-note { color: #808285; }
.rank-sw.rank-sw-score { background: #0D743B; border-radius: 50%; }
.rank-dist-note { font-size: 0.85rem; color: #58595B; margin: calc(6 * var(--u)) 0 0; }
.rank-dist-note .fa { color: #0D743B; }
/* Range slider of the monthly cost chart, in the toolbar */
.cost-slider .form-group { margin: 0; }
.cost-slider .irs { margin: calc(-6 * var(--u)) calc(6 * var(--u)) 0; }
/* Notes under the ranking: close together */
.rank-notes { display: flex; flex-direction: column; gap: calc(6 * var(--u)); margin-top: calc(6 * var(--u)); }
.rank-notes p, .rank-notes .ex-note, .rank-notes .ex-source { margin: 0; }
.rank-notes > .shiny-html-output:empty { display: none; }
.rank-unscored { font-size: 0.8rem; color: #6B7075; margin: var(--phi-1) 0 0; }
/* Segmented toggle (viz_toggle): the options side by side in one pill, the chosen one dark green */
.viz-toggle .form-group { margin: 0; }
.viz-toggle .shiny-options-group { display: inline-flex; border: 1px solid #D5DBDF; border-radius: calc(999 * var(--u)); padding: calc(3 * var(--u)); background: #FFFFFF; }
.viz-toggle .radio-inline { position: relative; margin: 0 !important; padding: 0 !important; }
.viz-toggle input[type=radio] { position: absolute; opacity: 0; pointer-events: none; }
.viz-toggle .radio-inline span { display: inline-block; padding: calc(5 * var(--u)) calc(14 * var(--u)); border-radius: calc(999 * var(--u));
  font-size: 0.8rem; color: #404041; cursor: pointer; transition: background 0.2s ease, color 0.2s ease; white-space: nowrap; }
.viz-toggle .radio-inline span:hover { color: #0D743B; }
.viz-toggle input[type=radio]:checked + span { background: #0D743B; color: #FFFFFF; font-weight: 600; }
.viz-toggle input[type=radio]:focus-visible + span { outline: 2px solid #FAA619; outline-offset: 2px; }
/* Surrounding land use: donut on the left, the Land use at a glance box beside it */
.lu-wrap { display: grid; grid-template-columns: minmax(0, 1.4fr) minmax(0, 1fr); gap: var(--phi-3); align-items: center; }
@media (max-width: 900px) { .lu-wrap { grid-template-columns: 1fr; } }
.lu-legend { background: #F6F9F5; border: 1px solid #E9EEE8; border-radius: calc(10 * var(--u)); padding: var(--phi-2) var(--phi-3); }
.lu-legend-title { font-family: 'IBM Plex Mono', monospace; font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase; color: #0D743B; font-weight: 600; }
.lu-legend-sub { font-size: 0.8rem; color: #6B7075; margin: calc(2 * var(--u)) 0 var(--phi-1); }
.lu-legend-row { display: grid; grid-template-columns: auto 1fr auto; align-items: center; gap: calc(8 * var(--u)); padding: calc(5 * var(--u)) 0;
  font-size: 0.88rem; color: #1E2B22; }
.lu-legend-pct { font-weight: 700; }
.lu-legend-other { border-top: 1px solid #E3E8E3; margin-top: calc(4 * var(--u)); padding-top: calc(8 * var(--u)); color: #6B7075; font-size: 0.8rem; }
.lu-legend-other .lu-legend-pct { font-weight: 400; }
.lu-legend-note { font-size: 0.75rem; color: #808285; margin-top: var(--phi-1); }
/* V8 detail: the rent category under the centre's AMR */
.amr-cat { display: inline-flex; align-items: center; gap: calc(6 * var(--u)); }
.amr-dot { width: calc(10 * var(--u)); height: calc(10 * var(--u)); border-radius: 50%; display: inline-block; flex: none; }
/* V8 detail: weekday vs weekend station activity, under the fact table */
.viz-week { margin-top: var(--phi-2); }
.viz-week-title { font-size: 0.9rem; font-weight: 600; color: #1E2B22; margin-bottom: calc(4 * var(--u)); }
.viz-week-key { display: flex; flex-wrap: nowrap; align-items: center; gap: calc(12 * var(--u)); font-size: 0.8rem; color: #6B7075; white-space: nowrap; }
.viz-week-key > span { display: inline-flex; align-items: center; gap: calc(6 * var(--u)); }
.viz-week-key .rank-sw { width: calc(16 * var(--u)); height: calc(16 * var(--u)); }
.viz-week-tick { display: inline-block; width: 4px; height: calc(18 * var(--u)); background: #1E2B22; border-radius: 1px; }
/* Centre table: compact rows, header on one line, tight search / info / pager */
.table-card > .card-body { padding-top: calc(6 * var(--u)); }
#centre_table table.dataTable > thead > tr > th,
#centre_table table.dataTable > tbody > tr > td { padding: calc(5 * var(--u)) calc(10 * var(--u)) !important; line-height: 1.3; }
#centre_table table.dataTable > thead > tr > th { white-space: nowrap; cursor: pointer; }
/* No sort arrows: click a header to sort, click again to reverse (the function stays, only the arrows go) */
#centre_table table.dataTable thead .sorting::before, #centre_table table.dataTable thead .sorting::after,
#centre_table table.dataTable thead .sorting_asc::before, #centre_table table.dataTable thead .sorting_asc::after,
#centre_table table.dataTable thead .sorting_desc::before, #centre_table table.dataTable thead .sorting_desc::after,
#centre_table table.dataTable thead .sorting_asc_disabled::before, #centre_table table.dataTable thead .sorting_asc_disabled::after,
#centre_table table.dataTable thead .sorting_desc_disabled::before, #centre_table table.dataTable thead .sorting_desc_disabled::after { display: none !important; }
#centre_table table.dataTable thead > tr > th.sorting, #centre_table table.dataTable thead > tr > th.sorting_asc,
#centre_table table.dataTable thead > tr > th.sorting_desc { background-image: none !important; }
/* The table's search box looks like the map's: white rounded box, magnifier icon, placeholder text */
#centre_table .dataTables_filter { float: none; text-align: left; margin: calc(4 * var(--u)) 0 calc(10 * var(--u)); }
#centre_table .dataTables_filter label { font-size: 0; display: block; }
#centre_table .dataTables_filter input { margin: 0; width: calc(340 * var(--u)); max-width: 100%; box-sizing: border-box; font-size: 0.9rem;
  font-family: inherit; color: #1E2B22; padding: calc(9 * var(--u)) calc(12 * var(--u)) calc(9 * var(--u)) calc(38 * var(--u)); border: 1px solid #D5DBDF; border-radius: calc(6 * var(--u)); outline: none;
  background: #FFFFFF url('data:image/svg+xml;utf8,<svg xmlns=%27http://www.w3.org/2000/svg%27 viewBox=%270 0 24 24%27 fill=%27none%27 stroke=%27%23808285%27 stroke-width=%272%27><circle cx=%2711%27 cy=%2711%27 r=%277%27/><path d=%27M20 20l-4-4%27/></svg>') no-repeat 12px center / 16px 16px; }
#centre_table .dataTables_filter input:focus { border-color: #0D743B; box-shadow: 0 0 0 calc(3 * var(--u)) rgba(13, 116, 59, 0.15); }
#centre_table .dataTables_info { padding-top: calc(6 * var(--u)) !important; font-size: 0.82rem; }
#centre_table .dataTables_paginate { padding-top: calc(4 * var(--u)) !important; }
#centre_table .pagination { margin: 0; }
#centre_table .page-link { padding: 2px calc(10 * var(--u)); font-size: 0.85rem; }

/* Comparison table: drag handle, sortable headers, remove button, empty state */
.compare-table { margin-bottom: 0; }
.compare-table thead th { background: #E8F2DE; color: #0D743B; font-weight: 600; border-bottom: 2px solid #0D743B; white-space: nowrap; }
.compare-table th.sortable-col { cursor: pointer; user-select: none; }
.compare-table th.sortable-col:hover { text-decoration: underline; }
.compare-table td { vertical-align: middle; user-select: none; }
.compare-table > :not(caption) > tr > th, .compare-table > :not(caption) > tr > td { padding: calc(5 * var(--u)) calc(10 * var(--u)); line-height: 1.3; }
.compare-table tbody tr { cursor: pointer; }
.compare-table tbody tr:hover td { background: #F2F8EC; }
.compare-table tbody tr.is-selected td { background: #E8F2DE; }
.compare-table tbody tr.is-selected td:first-child { box-shadow: inset calc(4 * var(--u)) 0 0 #0D743B; }
/* Selected centre on the map: a pulsing ring */
.sel-ring { animation: sel-pulse 1.6s ease-in-out infinite; }
@keyframes sel-pulse { 0%, 100% { stroke-opacity: 1; } 50% { stroke-opacity: 0.25; } }
@media (prefers-reduced-motion: reduce) { .sel-ring { animation: none; } }
.compare-table td.num { text-align: right; }
.compare-scroll { overflow-x: auto; }
.compare-table thead th small { display: block; font-weight: 400; font-size: 0.68rem; color: #58595B; letter-spacing: 0; }
/* Open to you?: a bar of the stalls this cuisine may bid on (green: most, amber: some, grey: few or none) */
.open-cell { display: flex; align-items: center; gap: calc(8 * var(--u)); min-width: calc(150 * var(--u)); }
.open-bar { flex: 0 0 calc(62 * var(--u)); height: calc(8 * var(--u)); background: #E3E8EB; border-radius: calc(999 * var(--u)); overflow: hidden; }
.open-fill { display: block; height: 100%; border-radius: calc(999 * var(--u)); }
.open-fill.ok { background: #0D743B; }
.open-fill.some { background: #FAA619; }
.open-fill.none { background: #B8C0C6; }
.open-text { font-size: 0.82rem; color: #404041; white-space: nowrap; }
.vac-dot { display: inline-block; width: calc(8 * var(--u)); height: calc(8 * var(--u)); border-radius: 50%; background: #BAC4CA; margin-right: calc(6 * var(--u)); }
.vac-dot.vac-yes { background: #40AD49; }
.gap-pos { color: #0D743B; font-weight: 600; }
.gap-neg { color: #C27C0E; }
.compare-note { font-size: 0.78rem; color: #6B7075; margin: var(--phi-2) 2px 0; line-height: 1.4; }
/* Hints under the comparison table: short tips in one row, then one line each for the cuisine columns and the CSV */
.compare-help { display: flex; flex-direction: column; gap: var(--phi-1); font-size: 0.8rem; color: #58595B; line-height: 1.45; }
.compare-tips { display: flex; flex-wrap: wrap; gap: var(--phi-1) var(--phi-3); }
.compare-tips b, .compare-help b { color: #1E2B22; font-weight: 600; }
.compare-help p { margin: 0; }
.drag-handle { cursor: grab; color: #808285; width: calc(28 * var(--u)); text-align: center; font-weight: 700; letter-spacing: -2px; }
.sortable-ghost { opacity: 0.4; background: #E8F2DE; }
.btn-remove { border: none; background: transparent; color: #808285; font-size: 1rem; line-height: 1; padding: 2px calc(6 * var(--u)); }
.btn-remove:hover { color: #B03A2E; }
.compare-empty { text-align: center; padding: calc(28 * var(--u)) calc(16 * var(--u)); }
.compare-empty-icon { font-size: 2rem; color: #0D743B; margin-bottom: calc(6 * var(--u)); }
.compare-empty-title { font-family: 'IBM Plex Sans', sans-serif; font-size: 1.35rem; color: #1E2B22; }
.compare-empty-text { color: #58595B; margin: calc(6 * var(--u)) auto calc(14 * var(--u)); max-width: calc(560 * var(--u)); }
.compare-empty-quick { display: flex; flex-direction: column; align-items: center; gap: var(--phi-2); }
.qs-label { margin: 0; }
/* Quick starts on one row, each button as wide as its label; a narrow screen lets them wrap */
.qs-grid { display: flex; flex-wrap: nowrap; justify-content: center; gap: calc(10 * var(--u)); max-width: 100%; }
.qs-grid .btn { width: auto; flex: none; white-space: nowrap; }
@media (max-width: 1100px) { .qs-grid { flex-wrap: wrap; } }

/* Back to map: round button, bottom right, shown when the map is out of view (class name kept from the old back-to-top) */
.back-to-top { position: fixed; right: calc(24 * var(--u)); bottom: calc(24 * var(--u)); z-index: 1050; width: calc(46 * var(--u)); height: calc(46 * var(--u)); border-radius: 50%;
  border: none; background: #0D743B; color: #FFFFFF; font-size: 1.1rem; display: flex; align-items: center; justify-content: center;
  box-shadow: 0 calc(6 * var(--u)) calc(16 * var(--u)) rgba(13, 116, 59, 0.30); opacity: 0; pointer-events: none; transform: translateY(calc(10 * var(--u)));
  transition: opacity 0.25s ease, transform 0.25s ease, background 0.2s ease; }
.back-to-top.show { opacity: 1; pointer-events: auto; transform: none; }
.back-to-top:hover { background: #40AD49; }
/* Notifications: white cards in the page's style, above the back-to-top button. Each one fades in
   while flying in from the right (opening its own space, so older ones are pushed up smoothly: the
   panel is anchored at the bottom and new ones are added below), and fades out when it goes. */
#shiny-notification-panel { bottom: calc(80 * var(--u)) !important; right: calc(18 * var(--u)) !important; width: calc(360 * var(--u)) !important; max-width: calc(100vw - calc(36 * var(--u))); padding: 0 !important; }
#shiny-notification-panel .shiny-notification {
  background: #FFFFFF; color: #1E2B22; opacity: 1; border: none; border-left: calc(4 * var(--u)) solid #0D743B; border-radius: calc(8 * var(--u));
  box-shadow: 0 calc(10 * var(--u)) calc(28 * var(--u)) rgba(13, 116, 59, 0.16), 0 2px calc(6 * var(--u)) rgba(30, 43, 34, 0.10);
  padding: calc(12 * var(--u)) calc(14 * var(--u)) calc(12 * var(--u)) calc(16 * var(--u)); margin: 0 0 calc(10 * var(--u)); font-family: 'IBM Plex Sans', sans-serif; font-size: 0.92rem; line-height: 1.45;
  animation: notif-in 0.55s cubic-bezier(0.22, 1, 0.36, 1) backwards; }
#shiny-notification-panel .shiny-notification-message { border-left-color: #0D743B; }
#shiny-notification-panel .shiny-notification-warning { border-left-color: #FAA619; }
#shiny-notification-panel .shiny-notification-error { border-left-color: #C2412D; }
#shiny-notification-panel .shiny-notification a { color: #0D743B; font-weight: 600; }
#shiny-notification-panel .shiny-notification-close { color: #808285; font-weight: 400; }
#shiny-notification-panel .shiny-notification-close:hover { color: #1E2B22; }
@keyframes notif-in {
  0%   { opacity: 0; transform: translateX(calc(70 * var(--u))); max-height: 0; margin-bottom: 0; padding-top: 0; padding-bottom: 0; overflow: hidden; }
  35%  { max-height: calc(240 * var(--u)); margin-bottom: calc(10 * var(--u)); padding-top: calc(12 * var(--u)); padding-bottom: calc(12 * var(--u)); }
  100% { opacity: 1; transform: none; max-height: calc(240 * var(--u)); } }
@media (prefers-reduced-motion: reduce) { #shiny-notification-panel .shiny-notification { animation: none; } }

/* Centre card: header, stat tiles, cuisine mix (compacted 30%) */
.cc-name { font-family: 'IBM Plex Sans', sans-serif; font-size: 1.05rem; font-weight: 700; line-height: 1.35; color: #0D743B; }
/* The centre's name stands out by its bold green type alone (no marker stroke behind it) */
.cc-meta { font-size: 0.65rem; color: #58595B; margin-top: 2px; display: flex; flex-wrap: wrap; gap: calc(4 * var(--u)); align-items: center; }
.cc-chip { background: #DDECC1; color: #0D743B; font-weight: 600; font-size: 0.58rem; padding: 1px calc(5 * var(--u)); border-radius: calc(999 * var(--u)); }
.cc-grid { display: grid; grid-template-columns: 1fr 1fr; gap: calc(5 * var(--u)); margin: calc(8 * var(--u)) 0 calc(6 * var(--u)); }
.cc-stat { background: #F6F8F9; border-radius: calc(4 * var(--u)); padding: calc(5 * var(--u)) calc(6 * var(--u)); }
.cc-label { font-size: 0.52rem; text-transform: uppercase; letter-spacing: 0.04em; color: #58595B; font-weight: 600; }
.cc-value { font-family: 'IBM Plex Sans', sans-serif; font-size: 0.88rem; color: #1E2B22; line-height: 1.15; }
.cc-note { font-size: 0.55rem; color: #808285; line-height: 1.15; }
.cc-section-title { font-size: 0.65rem; font-weight: 600; color: #1E2B22; margin: calc(3 * var(--u)) 0 0; }
.cc-title-row { display: flex; align-items: center; justify-content: space-between; gap: calc(8 * var(--u)); }
.cc-link { font-size: 0.65rem; font-weight: 600; color: #0D743B; text-decoration: none; white-space: nowrap; }
.cc-link:hover { text-decoration: underline; color: #0D743B; }
.cc-link .fa { margin-left: calc(3 * var(--u)); font-size: 0.6rem; }
.cc-empty { text-align: center; color: #58595B; padding: calc(18 * var(--u)) calc(6 * var(--u)); font-size: 0.72rem; }
.cc-empty .fa, .cc-empty svg { font-size: 1.2rem; color: #0D743B; display: block; margin: 0 auto calc(6 * var(--u)); }

/* Sections fade up as they scroll into view. Only armed once the script has
   checked the browser supports it (html.js-reveal), so content is never hidden
   if the script does not run. */
.js-reveal .reveal { opacity: 0; transform: translateY(calc(24 * var(--u))); transition: opacity 0.7s ease, transform 0.7s ease; }
.js-reveal .reveal.is-visible { opacity: 1; transform: none; }
@media (prefers-reduced-motion: reduce) {
  html { scroll-behavior: auto; }
  .js-reveal .reveal { opacity: 1; transform: none; transition: none; }
}
"

app_js <- "
$(function () {

  // Map filters toggle (bslib's arrow on the sidebar edge): the map changes width,
  // so let it resize. Safety net: if bslib is still mid-switch after its animation
  // should have ended (transitionend missed), tell it the animation has ended.
  document.querySelectorAll('#sec-map .bslib-sidebar-layout > .collapse-toggle').forEach(function (t) {
    // Tooltip says what a click will do
    var lay0 = t.closest('.bslib-sidebar-layout');
    var tip = function () {
      var shut = lay0 && lay0.classList.contains('sidebar-collapsed');
      t.title = shut ? 'Show filters' : 'Hide filters';
      t.setAttribute('aria-label', t.title);
    };
    tip();
    if (lay0) new MutationObserver(tip).observe(lay0, { attributes: true, attributeFilter: ['class'] });
    t.addEventListener('click', function () {
      setTimeout(function () {
        var lay = t.closest('.bslib-sidebar-layout'), icon = t.querySelector('.collapse-icon');
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
    });
  });

  // Default map view: Singapore fitted to the map's real size. It is refitted whenever the map
  // changes size (window, filters sidebar, full screen) as long as nobody has moved the map since
  // the last fit, so a user's own zoom and the zooms from filters and clicks are never undone.
  // The bounds are the built-up main island (no far-off islets), so Singapore fills the map.
  var SG_BOUNDS = [[1.235, 103.62], [1.47, 104.03]], sgLast = null, sgTouched = false;
  function sgMap() {
    var w = window.HTMLWidgets && HTMLWidgets.find('#map');
    return w && w.getMap ? w.getMap() : null;
  }
  // On a wide map the floating centre card covers its right side: room for the card at its real width
  function cardPad(m, min) {
    var card = document.querySelector('.map-overlay');
    return (card && m.getSize().x >= 800) ? card.offsetWidth + 24 : min;
  }
  function sgFit() {
    var m = sgMap();
    if (!m) return;
    m.invalidateSize();
    // On a wide map Singapore fills the part left of the floating centre card; on a narrow one it uses everything
    m.fitBounds(SG_BOUNDS, { paddingTopLeft: [4, 4], paddingBottomRight: [cardPad(m, 4), 4], animate: false });
    sgLast = { c: m.getCenter(), z: m.getZoom() };
  }
  function sgAtDefault(m) {
    if (!sgLast) return true;
    var c = m.getCenter();
    return Math.abs(c.lat - sgLast.c.lat) < 1e-4 && Math.abs(c.lng - sgLast.c.lng) < 1e-4 &&
           Math.abs(m.getZoom() - sgLast.z) < 1e-4;
  }
  var sgTimer = null;
  function sgOnResize() {
    clearTimeout(sgTimer);
    sgTimer = setTimeout(function () {
      var m = sgMap();
      if (!m) return;
      m.invalidateSize();
      if (!sgTouched || sgAtDefault(m)) sgFit();
    }, 150);
  }
  var mapEl = document.getElementById('map');
  if (mapEl && window.ResizeObserver) new ResizeObserver(sgOnResize).observe(mapEl);
  window.addEventListener('resize', sgOnResize);
  // Once someone drags, zooms or clicks the map, later resizes only refit if it is still at the default view
  if (mapEl) ['mousedown', 'wheel', 'touchstart'].forEach(function (ev) {
    mapEl.addEventListener(ev, function () { sgTouched = true; }, { passive: true });
  });
  // First draw of the map: fit once it exists, and again while the page settles (fonts, sidebar, images)
  $(document).on('shiny:value', function (e) {
    if (e.name === 'map') [400, 1500, 3500].forEach(function (ms) {
      setTimeout(function () { if (!sgTouched) sgFit(); }, ms);
    });
  });
  window.addEventListener('load', function () { setTimeout(function () { if (!sgTouched) sgFit(); }, 800); });
  // Expanding or collapsing the map card: resize the map and fit Singapore again
  new MutationObserver(function () { setTimeout(function () { sgTouched = false; sgFit(); }, 350); })
    .observe(document.body, { attributes: true, subtree: true, attributeFilter: ['data-full-screen'] });
  // Changing a box in the Ranking tab, or any chart's own control, redraws that part in place. The page's
  // scroll snapping would then jump to the nearest section; instead the page stays exactly where it is:
  // snapping is paused, the scroll position kept, and snapping comes back once Shiny is idle.
  var keepY = null, snapTimer = null;
  var holdScroll = function () {
    if (keepY === null) keepY = window.scrollY;
    document.documentElement.style.scrollSnapType = 'none';
    clearTimeout(snapTimer);
  };
  $(document).on('shiny:inputchanged', function (e) {
    if (/^(rank_|cost_window|v[0-9]_cuisine|v8_view|lu_radius|price_|table_tabs)/.test(e.name)) holdScroll();
  });
  var backTo = function () { if (Math.abs(window.scrollY - keepY) > 1) window.scrollTo({ top: keepY, behavior: 'instant' }); };
  $(document).on('shiny:value', function () { if (keepY !== null) setTimeout(backTo, 0); });
  $(document).on('shiny:idle', function () {
    if (keepY === null) return;
    backTo();
    snapTimer = setTimeout(function () { document.documentElement.style.scrollSnapType = ''; keepY = null; }, 400);
  });

  // A link inside the expanded map card (the centre card's Full breakdown): the card first shrinks
  // back into the page, then the page scrolls smoothly down to the link's target
  $(document).on('click', '.bslib-card[data-full-screen=true] a[href^=\\'#\\']', function (e) {
    var target = document.querySelector(this.getAttribute('href'));
    var card = $(this).closest('.bslib-card')[0];
    if (!target || !card) return;
    e.preventDefault();
    card.classList.add('fs-closing');
    setTimeout(function () {
      var exit = document.querySelector('.bslib-full-screen-exit');
      if (exit) exit.click();
      else document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
      card.classList.remove('fs-closing');
      setTimeout(function () { target.scrollIntoView({ behavior: 'smooth', block: 'start' }); }, 120);
    }, 260);
  });
  // Zoom to an area picked in the filters (fit_map, sent by server.R), clear of the search box (top left)
  // and the centre card (top right) at their real sizes; 'all' flies back to Singapore
  if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('fit_map', function (msg) {
    var m = sgMap();
    if (!m) return;
    if (msg.all) {
      m.once('moveend', function () { sgLast = { c: m.getCenter(), z: m.getZoom() }; });   // the default view again
      m.flyToBounds(SG_BOUNDS, { paddingTopLeft: [4, 4], paddingBottomRight: [cardPad(m, 4), 4] });
      return;
    }
    var b = msg.bounds, box = document.querySelector('#sec-map .map-search');
    var top = box ? box.offsetTop + box.offsetHeight + 12 : 60;
    m.flyToBounds([[b[1], b[0]], [b[3], b[2]]], { paddingTopLeft: [20, top], paddingBottomRight: [cardPad(m, 20), 20] });
  });

  // Centre search boxes (map and V3): autocomplete over a list of centres, never opens on click
  function centreSearch(cfg) {
    var centres = cfg.data || [];
    var $input = $(cfg.input);
    var $drop = $(cfg.drop);
    var $clear = $(cfg.clear);
    var $box = $input.closest('.map-search');
    if (!$input.length) return;

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
      Shiny.setInputValue(cfg.shinyId, id, { priority: 'event' });
    });

    // Clear button (x)
    $clear.on('click', function () {
      $input.val('').focus();
      $drop.hide().empty();
      $clear.hide();
    });

    // Close on click outside or Escape
    $(document).on('click', function (e) {
      if (!$(e.target).closest($box).length) {
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

    // Set search text when the centre is selected elsewhere (map marker, table row)
    if (cfg.message && window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler(cfg.message, function (data) {
        if (data && data.name) {
          $input.val(data.name);
          $clear.show();
          $drop.hide();
        }
      });
    }
  }

  centreSearch({ data: window.CENTRES_DATA, input: '#map_search_input', drop: '#map_search_dropdown',
                 clear: '#map_search_clear', shinyId: 'cg_centre', message: 'set_search_centre' });
  centreSearch({ data: window.V3_CENTRES_DATA, input: '#v3_search_input', drop: '#v3_search_dropdown',
                 clear: '#v3_search_clear', shinyId: 'v3_centre', message: 'set_v3_search' });
  centreSearch({ data: window.V6_CENTRES_DATA, input: '#v6_search_input', drop: '#v6_search_dropdown',
                 clear: '#v6_search_clear', shinyId: 'v6_centre', message: 'set_v6_search' });
  centreSearch({ data: window.CENTRES_DATA, input: '#rank_search_input', drop: '#rank_search_dropdown',
                 clear: '#rank_search_clear', shinyId: 'rank_centre', message: 'set_rank_search' });

  // The map selection is highlighted in the comparison table (compare_selected, sent by server.R)
  // without the server redrawing the table
  if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('compare_selected', function (msg) {
    document.querySelectorAll('#compare_rows > tr').forEach(function (tr) {
      tr.classList.toggle('is-selected', tr.getAttribute('data-id') === String(msg.id));
    });
  });

  // Map: a centre circle (orange) turns dark green while the mouse is over it, and a double-click on a
  // centre adds it to the comparison table. The centre circles are the ones in the 'centres' pane (server.R).
  var hookMap = setInterval(function () {
    var w = window.HTMLWidgets && HTMLWidgets.find('#map');
    var m = w && w.getMap && w.getMap();
    if (!m) return;
    clearInterval(hookMap);
    // Only one circle can be hovered at a time. Leaflet skips mouseout when the map
    // flies to a clicked centre (the circle slides away under a still mouse), which
    // used to leave several circles stuck in dark green; any map move now resets it.
    var hovered = null, clickedCentre = null, clicks = [null, null];
    var unhover = function () {
      if (!hovered) return;
      hovered.setStyle(hovered._baseStyle);
      hovered.setRadius(hovered._baseRadius);
      hovered = null;
    };
    var hook = function (l) {
      if (!(l instanceof L.CircleMarker) || l.options.pane !== 'centres' || l._hoverHooked) return;
      l._hoverHooked = true;
      l._baseRadius = l.getRadius();
      l._baseStyle = { fillColor: l.options.fillColor, fillOpacity: l.options.fillOpacity };
      l.on('mouseover', function () {
        if (hovered !== l) unhover();
        hovered = l;
        l.setStyle({ fillColor: '#0D743B', fillOpacity: 1 }); l.setRadius(l._baseRadius + 2); l.bringToFront();
      });
      l.on('mouseout', function () { if (hovered === l) unhover(); });
      l.on('remove', function () { if (hovered === l) hovered = null; });
      l.on('click', function () { clickedCentre = l.options.layerId; });   // runs just before the map's click
    };
    m.eachLayer(hook);
    m.on('layeradd', function (e) { hook(e.layer); });
    m.on('movestart zoomstart mouseout', unhover);
    // Double-click a centre = add it to the comparison table (map_centre_dblclick in server.R). The map flies
    // to a centre on its first click, so the second click can land beside the circle: a double-click counts
    // for the centre of its second click, or else of its first.
    m.on('click', function () {
      clicks = [clicks[1], { id: clickedCentre, t: Date.now() }];
      clickedCentre = null;
    });
    m.on('dblclick', function () {
      var c = (clicks[1] && clicks[1].id != null) ? clicks[1] : clicks[0];
      if (c && c.id != null && Date.now() - c.t < 1000 && window.Shiny)
        Shiny.setInputValue('map_centre_dblclick', c.id, { priority: 'event' });
      clicks = [null, null];
    });

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
    m.on('movestart zoomstart mouseout', function () { active = null; tip.style.display = 'none'; });
  }, 300);

  // Scroll progress: the line under the header fills as the page scrolls. The page parts are looked up
  // once, and the work runs at most once per frame however often the browser reports a scroll.
  var bar = document.querySelector('.scroll-progress'), up = document.querySelector('.back-to-top'),
      mapSec = document.getElementById('sec-map'), hdr = document.querySelector('.app-header'),
      hero = document.getElementById('sec-home'), heroText = document.querySelector('.hero-content'),
      heroDown = document.querySelector('.hero-down'), heroShade = document.querySelector('.hero-shade');
  var progress = function () {
    var h = document.documentElement, max = h.scrollHeight - h.clientHeight;
    if (bar) bar.style.width = (max > 0 ? Math.min(100, h.scrollTop / max * 100) : 0) + '%';
    // Back-to-map button appears once the opening photos are behind and the map is out of view
    if (up) {
      var mr = mapSec ? mapSec.getBoundingClientRect() : null;
      var mapInView = mr && mr.top < window.innerHeight * 0.5 && mr.bottom > window.innerHeight * 0.5;
      up.classList.toggle('show', h.scrollTop > 400 && !mapInView);
    }
    // Header: transparent over the opening photos, white once they have scrolled by
    if (hdr) hdr.classList.toggle('solid', !hero || h.scrollTop > hero.offsetHeight - hdr.offsetHeight - 10);
    // Opening: the title fades and rises over the first 40% of a screen of scrolling,
    // then only the photos show (the shade lightens too) until the page moves on
    if (heroText) {
      var f = Math.min(1, Math.max(0, h.scrollTop / (window.innerHeight * 0.4)));
      heroText.style.opacity = String(1 - f);
      heroText.style.transform = 'translate(-50%, ' + Math.round(-50 * f) + 'px)';
      if (heroDown) heroDown.style.opacity = String(0.9 * (1 - Math.min(1, 2 * f)));
      if (heroShade) heroShade.style.opacity = String(1 - 0.65 * f);
    }
  };
  var progressQueued = false;
  var onScroll = function () {
    if (progressQueued) return;
    progressQueued = true;
    requestAnimationFrame(function () { progressQueued = false; progress(); });
  };
  window.addEventListener('scroll', onScroll, { passive: true });
  window.addEventListener('resize', onScroll);
  progress();

  // The map grows down so the map card ends level with the Reset filters button
  // (never shorter than its design height); in full screen or a stacked layout it keeps that height.
  // Checked when the filters or the window change size (scrolling moves both together, so not then).
  var mapBaseCss = null, mapBaseH = null, mapExtra = -1;
  var fitMapToFilters = function () {
    var wrap = document.querySelector('.map-sticky-wrap');
    var card = document.querySelector('.map-floating-card');
    var mapEl = document.getElementById('map');
    var resetBtn = document.getElementById('reset_t1');
    if (!wrap || !card || !mapEl || !resetBtn) return;
    if (mapBaseCss === null) mapBaseCss = mapEl.style.height;
    if (card.getAttribute('data-full-screen') === 'true') return;
    if (mapBaseH === null) {   // the design height in px at this window width (it follows --u)
      var probe = document.createElement('div');
      probe.style.cssText = 'position:absolute;visibility:hidden;height:' + mapBaseCss;
      document.body.appendChild(probe);
      mapBaseH = probe.offsetHeight;
      probe.remove();
    }
    var cardBaseH = card.offsetHeight - mapEl.offsetHeight + mapBaseH;
    var targetBottom = resetBtn.getBoundingClientRect().bottom - wrap.getBoundingClientRect().top;
    var extra = Math.max(0, Math.round(targetBottom - cardBaseH));
    if (extra === mapExtra) return;
    mapExtra = extra;
    // The map's own ResizeObserver redraws it and refits Singapore
    mapEl.style.height = extra > 0 ? (mapBaseH + extra) + 'px' : mapBaseCss;
  };
  var mapSidebar = document.querySelector('#sec-map .bslib-sidebar-layout > .sidebar');
  if (mapSidebar && window.ResizeObserver) new ResizeObserver(fitMapToFilters).observe(mapSidebar);
  window.addEventListener('resize', function () { mapBaseH = null; mapExtra = -1; fitMapToFilters(); });
  setTimeout(fitMapToFilters, 500);
  setTimeout(fitMapToFilters, 1500);

  if (!('IntersectionObserver' in window)) return;  // old browser: no effects, all visible

  // Sections fade up as they scroll into view
  document.documentElement.classList.add('js-reveal');
  var reveal = new IntersectionObserver(function (entries) {
    entries.forEach(function (e) {
      if (e.isIntersecting) { e.target.classList.add('is-visible'); reveal.unobserve(e.target); }
    });
  }, { threshold: 0.08 });
  document.querySelectorAll('.reveal').forEach(function (el) { reveal.observe(el); });

  // About us: a star click sends the rating (1-5); Enter in the email box submits
  document.querySelectorAll('.star-rating input').forEach(function (s) {
    s.addEventListener('change', function () {
      if (window.Shiny) Shiny.setInputValue('rating', Number(s.value), { priority: 'event' });
    });
  });
  var email = document.getElementById('sub_email');
  if (email) email.addEventListener('keydown', function (e) {
    if (e.key === 'Enter') { e.preventDefault(); document.getElementById('sub_submit').click(); }
  });

  // Overview photos: slide one photo to the left every 5 s, no controls. A copy of
  // the first photo sits at the end, so after the last photo the track slides onto
  // the copy and then jumps back to the start without the jump being visible.
  // Waits while the browser tab is hidden; with reduced motion the photos simply change.
  document.querySelectorAll('.ov-slider').forEach(function (slider) {
    var track = slider.querySelector('.ov-track');
    var n = track.children.length;
    if (n < 2) return;
    track.appendChild(track.children[0].cloneNode(true));
    var i = 0;
    var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    var show = function (k, animate) {
      track.style.transition = (animate && !reduce) ? '' : 'none';
      track.style.transform = 'translateX(' + (-100 * k) + '%)';
    };
    track.addEventListener('transitionend', function (e) {
      if (e.propertyName === 'transform' && i === n) { i = 0; show(0, false); }
    });
    setInterval(function () {
      if (document.hidden) return;
      i += 1;
      show(i, true);
      if (reduce && i === n) { i = 0; show(0, false); }
    }, 5000);
  });

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


  // Dot navigation (top right): shown while scrolling with the part in view lit up, hidden 1.5 s after
  // scrolling stops; the mouse in the green margin right of the content brings it back with every part's name
  var dotNav = document.querySelector('.dot-nav');
  if (dotNav) {
    var dots = Array.prototype.slice.call(dotNav.querySelectorAll('a'));
    var dotSecs = dots.map(function (a) { return document.querySelector(a.getAttribute('href')); });
    var dotTimer = null, dotTick = false;
    var dotHide = function () { if (!dotNav.matches(':hover')) dotNav.classList.remove('show'); };
    var dotShow = function () { dotNav.classList.add('show'); clearTimeout(dotTimer); dotTimer = setTimeout(dotHide, 1500); };
    // the part in view = the last section whose top has passed the middle of the screen
    var dotCur = -1, flashTimer = null;
    var dotUpdate = function () {
      var mid = window.innerHeight / 2, cur = 0;
      dotSecs.forEach(function (s, i) { if (s && s.getBoundingClientRect().top <= mid) cur = i; });
      dots.forEach(function (a, i) { a.classList.toggle('active', i === cur); });
      // a new part: its name shows for 2 s beside its cell
      if (dotCur !== -1 && cur !== dotCur) {
        dots.forEach(function (a) { a.classList.remove('flash'); });
        dots[cur].classList.add('flash');
        clearTimeout(flashTimer);
        flashTimer = setTimeout(function () { dots[cur].classList.remove('flash'); }, 2000);
      }
      dotCur = cur;
    };
    window.addEventListener('scroll', function () {
      dotShow();
      if (!dotTick) { dotTick = true; requestAnimationFrame(function () { dotUpdate(); dotTick = false; }); }
    }, { passive: true });
    // Only the green margin right of the page's cards brings the dots and their names up: the mouse over
    // a card or chart never does, so the names never cover the content being read
    var contentRight = 0;
    var measure = function () {
      contentRight = 0;
      document.querySelectorAll('.page-main .card').forEach(function (c) {
        var r = c.getBoundingClientRect(); if (r.width > 0) contentRight = Math.max(contentRight, r.right);
      });
      if (!contentRight) contentRight = window.innerWidth - 70;
    };
    window.addEventListener('resize', measure);
    window.addEventListener('load', function () { measure(); setTimeout(measure, 2000); });
    measure();
    document.addEventListener('mousemove', function (e) {
      var inMargin = e.clientX > contentRight + 2 || dotNav.contains(e.target);   // the dots themselves count too
      dotNav.classList.toggle('peek', inMargin);
      if (inMargin && !dotNav.classList.contains('show')) dotShow();
    }, { passive: true });
    dotNav.addEventListener('mouseleave', function () { clearTimeout(dotTimer); dotTimer = setTimeout(dotHide, 800); });
    dotUpdate();
  }
});
"
# McKinsey-style exhibit frame: headline, subtitle, the chart (`...`) and a source line
exhibit <- function(code, headline, subtitle, source, ...) {
  card(fill = FALSE, class = "exhibit", `data-code` = code,
       div(class = "ex-headline", headline),
       div(class = "ex-subtitle", if (is.character(subtitle)) HTML(subtitle) else subtitle),
       ...,
       div(class = "ex-source", source))
}

# Section with a centred heading: a rule, the title and a one-line kicker
mck_section <- function(id, title, kicker, ..., reveal = TRUE) {
  tags$section(id = id, class = paste("mck-section", if (reveal) "reveal"),
               div(class = "sec-head",
                   div(class = "sec-rule"),
                   h2(class = "sec-title", title),
                   p(class = "sec-kicker", kicker)),
               ...)
}

# Centre search box (autocomplete handled by centreSearch() in app_js): prefix gives the element ids
viz_centre_search <- function(prefix, default_name) {
  div(class = "map-search",
      div(class = "map-search-box",
          icon("magnifying-glass", class = "map-search-icon"),
          # not a Shiny input (every keystroke would go to the server): the page script reads it
          tags$input(type = "text", id = paste0(prefix, "_search_input"), class = "map-search-input",
                     value = default_name, placeholder = "Search hawker centre",
                     autocomplete = "off", spellcheck = "false", `data-shiny-no-bind-input` = NA),
          tags$button(type = "button", id = paste0(prefix, "_search_clear"), class = "map-search-clear",
                      style = "display: block;", `aria-label` = "Clear search", tags$i(class = "fa fa-xmark"))),
      div(id = paste0(prefix, "_search_dropdown"), class = "map-search-dropdown"))
}

# Toolbar above a chart: a small label and the control that filters that chart
viz_toolbar <- function(label, ...) div(class = "viz-toolbar", span(class = "viz-toolbar-label", label), ...)

# Two or more options as one segmented pill toggle (V8 view, land-use radius)
viz_toggle <- function(id, choices, selected)
  div(class = "viz-toggle", radioButtons(id, NULL, choices = choices, selected = selected, inline = TRUE))

# Dropdown in the same white box as the centre search
viz_select <- function(id, choices, selected, icon_name = "utensils") {
  div(class = "viz-tool",
      div(class = "map-search-box viz-select-box",
          icon(icon_name, class = "map-search-icon"),
          selectInput(id, NULL, choices = choices, selected = selected, selectize = FALSE, width = "100%")))
}

# Data visualization row: the chart on the left and its story on the right (a
# mirrored McKinsey layout), one row per chart. The story opens with a bold
# takeaway that runs into the explanation. `...` = the toolbar and the chart; `detail` = an optional
# box under the story (V8).
viz_row <- function(code, label, headline, subtitle, source, lead, body, ..., detail = NULL) {
  div(class = "viz-row reveal", id = paste0("viz-", tolower(code)),
      div(class = "viz-chart", exhibit(code, headline, subtitle, source, ...)),
      div(class = "viz-text",
          div(class = "viz-label", label),
          p(class = "viz-copy", tags$strong(lead), " ", body),
          detail))
}

# Month label that does not depend on the computer's language, e.g. "Oct 25"
month_label <- function(d) paste(month.abb[as.integer(format(d, "%m"))], format(d, "%y"))
month_full  <- function(d) paste(month.name[as.integer(format(d, "%m"))], format(d, "%Y"))   # "October 2025", in any locale

TENDER_SOURCE <- paste0("Source: NEA tender notices, ", month_label(MONTHS[1]), "–", month_label(MONTHS[2]))

# Opening photos (faces already hidden), shown as a slider behind the title
OVERVIEW_SLIDES <- sort(list.files("www/slides", pattern = "\\.(jpg|jpeg|png|webp)$"))

# Map filters: a sidebar inside the map section only, grouped by type
map_filters <- sidebar(
  title = "Filters", width = "calc(min(max(250px, 22vw), 600px))",  # about 22% of the window on every screen (calc() so validateCssUnit accepts it)
  selectInput("region", "Region", choices = c("All regions" = "All", named_choices(REGIONS))),
  selectInput("pa", "Planning area", choices = c("All planning areas" = "All", named_choices(PAS))),
  selectInput("cuisine", "Centres that sell", choices = c("Any cuisine" = "All", CUISINE_GROUPS)),
  selectInput("map_metric", "Shade planning areas by",
              choices = c("Number of centres" = "n",
                          "Centres per 10 km²" = "density",
                          "Cooked-food stalls" = "stalls",
                          "Residents" = "residents",
                          "Cuisine engagement share (%)" = "reviews")),
  conditionalPanel("input.map_metric == 'reviews'",
    tag_note("Share of Google Maps restaurant reviews, as in V4. Uses the cuisine chosen in \"Centres that sell\"; \"Leading cuisine by area (Overview)\" shades each area by its leading cuisine. Planning areas without hawker centres are excluded (light grey).")),
  checkboxInput("only_tenders", "Only centres with tender listings", FALSE),
  checkboxInput("only_vacant", "Only centres with vacant stalls", FALSE),
  checkboxInput("map_exits", tagList(mrt_icon_img(), "Show MRT/LRT exits"), FALSE),
  tags$hr(class = "my-1"),
  sliderInput("max_amr", "Median rent (AMR, S$)", min = AMR_RANGE[1], max = AMR_RANGE[2],
              value = AMR_RANGE, step = 50, pre = "S$"),
  sliderInput("max_mrt", "Distance to MRT exit (m)", min = 0, max = MRT_MAX,
              value = c(0, MRT_MAX), step = 100),
  sliderInput("min_vacant", "Min. vacant stalls (Hawker Guide)", min = 0, max = VAC_MAX,
              value = 0, step = 1),
  uiOutput("t1_count"),
  reset_button("reset_t1"),
  tag_note("A region limits the planning areas; a planning area sets its region. Cuisines and slider ranges follow the chosen area. To find one hawker centre, use the search box at the top left of the map.")
)

ui <- page_fluid(
  title = "HawkerRadar",
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
    heading_font = font_google("IBM Plex Sans", local = FALSE),   # sans-serif everywhere (user's request)
    code_font = font_google("IBM Plex Mono", local = FALSE)
  ),
  tags$head(tags$style(HTML(app_css)), tags$script(HTML(paste0("window.CENTRES_DATA = ", CENTRES_SEARCH_JSON, "; window.V3_CENTRES_DATA = ", V3_SEARCH_JSON, "; window.V6_CENTRES_DATA = ", V6_SEARCH_JSON, ";"))), tags$script(HTML(app_js)),
            # Serif for the headings: Libre Baskerville (Google Fonts)
            tags$link(rel = "stylesheet",
                      href = "https://fonts.googleapis.com/css2?family=Libre+Baskerville:wght@400;700&display=swap"),
            tags$meta(name = "theme-color", content = "#0D743B")),

  # ---------------------------------------------------------------------------
  # Header (McKinsey style): the name; transparent over the opening photos, white below them.
  # The ☰ menu was removed (user's request); the dot navigation below moves between the parts.
  # ---------------------------------------------------------------------------
  div(class = "app-header", id = "top",
      tags$a(href = "#sec-home", class = "brand", span(class = "app-title", "HawkerRadar")),
      div(class = "scroll-progress")),
  # Dot navigation on the right edge (script in app_js): one dot per part of the page
  tags$nav(class = "dot-nav", `aria-label` = "Page sections",
           lapply(list(c("sec-home", "Home"), c("sec-map", "Map"),
                       c("sec-cuisine", "Cuisine Landscape & Opportunities"),
                       c("sec-tender", "Tender Market & Stall Opportunities"),
                       c("sec-landuse", "Surrounding Land Use"),
                       c("sec-transport", "Transport Accessibility & Activity"),
                       c("sec-compare", "Compare your shortlist"), c("sec-about", "About us")),
                  function(s) tags$a(href = paste0("#", s[1]), span(class = "label", s[2]), span(class = "dot")))),

  # ---------------------------------------------------------------------------
  # Home: full-screen opening, Lau Pa Sat photos sliding behind the title
  # (www/slides, faces hidden; script in app_js), arrow down to the map
  # ---------------------------------------------------------------------------
  tags$section(id = "sec-home", class = "hero",
    # The stage stays on screen while the tall section scrolls by (title fades, photos stay)
    div(class = "hero-stage",
      if (length(OVERVIEW_SLIDES))
        div(class = "ov-slider", role = "img", `aria-label` = "Photos of Lau Pa Sat hawker centre, Singapore",
            div(class = "ov-track",
                lapply(OVERVIEW_SLIDES, function(f)
                  tags$img(src = paste0("slides/", f), class = "ov-slide", alt = "", draggable = "false")))),
      div(class = "hero-shade"),
      div(class = "hero-content",
          div(class = "hero-kicker", "DBA3702 · SA1 Team 03"),
          h1(class = "hero-title", "HawkerRadar"),
          p(class = "hero-sub", "Where should a new hawker open, and what should they sell?"),
          div(class = "hero-meta", "Built by a team of NUS students · 2026 edition")),
      tags$a(href = "#sec-map", class = "hero-down", `aria-label` = "Scroll down to the map",
             HTML("<svg viewBox='0 0 46 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M2 2l21 20L44 2'/></svg>")),
      if (length(OVERVIEW_SLIDES)) div(class = "hero-credit", "Photos: Lau Pa Sat, Singapore; faces hidden for privacy")),
    div(class = "hero-rest")
  ),

  div(class = "page-main",

    # -------------------------------------------------------------------------
    # Map (V1 + V2), with its filters in a sidebar beside it
    # -------------------------------------------------------------------------
    mck_section(
      "sec-map", "Where hawker centres are",
      "Location, density and residents, filtered by your budget and the vacant stalls you need",
      reveal = FALSE,
      layout_sidebar(
        fillable = FALSE, border = FALSE, sidebar = map_filters,
        layout_columns(
          fill = FALSE, gap = "calc(10 * var(--u))",
          value_box("Hawker centres", textOutput("vb_centres"),
                    tags$p(class = "vb-sub", textOutput("vb_centres_sub", inline = TRUE))),
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
                leafletOutput("map", height = "calc(620 * var(--u))"),
                # Search box over the map's top-left corner: pick a centre to zoom to it
                # and open its centre card (replaces the old sidebar dropdown)
                div(class = "map-search",
                    div(class = "map-search-box",
                        icon("magnifying-glass", class = "map-search-icon"),
                        tags$input(type = "text", id = "map_search_input", class = "map-search-input",   # read by the page script, not Shiny
                                   placeholder = "Search hawker centre", autocomplete = "off", spellcheck = "false",
                                   `data-shiny-no-bind-input` = NA),
                        tags$button(type = "button", id = "map_search_clear", class = "map-search-clear",
                                    `aria-label` = "Clear search", tags$i(class = "fa fa-xmark"))),
                    div(id = "map_search_dropdown", class = "map-search-dropdown")),
                div(class = "centre-card-box map-overlay",
                    div(class = "info-title", "Centre card"),
                    uiOutput("centre_card"),
                    # Shown once a centre is chosen: its cuisine mix, then the Add to comparison button
                    conditionalPanel("output.centre_selected",
                      div(class = "cc-section-title cc-title-row", "Cuisine mix (stalls)",
                          tags$a(href = "#viz-v3", class = "cc-link", "Full breakdown", icon("arrow-down"))),
                      plotlyOutput("centre_cuisine", height = "calc(120 * var(--u))"),
                      div(class = "cc-add-slot", uiOutput("cc_add"))))),
            # Legend in one row right below the map
            div(class = "map-legend-below", uiOutput("map_legend")),
            tag_note(class = "map-note", "Search a centre (top left of the map) to zoom to it. Hover a planning area or a circle for details; click a circle or a table row to open its centre card (top right of the map). Double-click a circle, or press Add to comparison, to add it to the comparison table."),
            div(class = "ex-source",
                "Source: NEA hawker centres and tender notices; Hawker Guide; URA Master Plan 2019; SingStat, June 2026; LTA MRT/LRT exits")
          )
        ),
        # fill = FALSE: the card grows with the table instead of scrolling. Two tabs: every centre that
        # passes the filters, and the Ranking (scores out of 10, server.R)
        tagAppendAttributes(class = "table-card", navset_card_underline(
          id = "table_tabs", full_screen = FALSE,
          title = div(class = "ex-headline table-title", "Centre table"),
          nav_panel("All centres", value = "all",
                    DTOutput("centre_table", fill = FALSE)),  # size to its rows, no 400 px fill box
          nav_panel("Find Your Best Fit", value = "rank",
                    viz_toolbar("Centre",
                                viz_centre_search("rank", centres$centre_name[centres$centre_id == DEFAULT_CENTRE]),
                                span(class = "viz-toolbar-label", "Area"),
                                viz_select("rank_pa", c("Nearest to the selected centre" = "near", named_choices(PAS)),
                                           "near", icon_name = "location-dot"),
                                span(class = "viz-toolbar-label", "Cuisine"),
                                viz_select("rank_cuisine", c("Any cuisine (best fit)" = "All", V3_ORDER), "All"),
                                span(class = "viz-toolbar-label", "Budget"),
                                div(class = "viz-tool rank-budget",
                                    div(class = "map-search-box viz-select-box",
                                        icon("wallet", class = "map-search-icon"),
                                        numericInput("rank_budget", NULL, value = RANK_BUDGET_DEFAULT,
                                                     min = 0, step = 50, width = "100%"))),
                                actionButton("rank_reset", "Reset ranking", icon = icon("rotate-left"),
                                             class = "btn-sm btn-outline-secondary rank-reset")),
                    div(class = "rank-legend",
                        span(span(class = "rank-sw", style = "background:#0D743B"), "Accessibility"),
                        span(span(class = "rank-sw", style = "background:#99CA3C"), "Budget"),
                        span(span(class = "rank-sw", style = "background:#FAA619"), "Cuisine"),
                        span(span(class = "rank-sw rank-sw-score"), "Overall score"),
                        span(class = "rank-legend-note", "Three equally weighted components, each worth up to 3.33 points (10 total).")),
                    uiOutput("rank_dist_note"),
                    uiOutput("rank_plot_ui"),   # height follows the number of centres ranked
                    div(class = "rank-notes",
                    uiOutput("rank_unscored"),
                    uiOutput("rank_budget_note"),
                    div(class = "ex-note", tags$b("How it is scored:"),
                        sprintf(" three scores out of 10 with equal weights. Accessibility averages distance to the nearest MRT/LRT exit (0 m = 10, %s m or more = 0) and the station's daily tap-outs (log scale). Budget is how close the centre's median rent is to your budget; going over counts double. Cuisine is the engagement–supply gap, where no gap = 5 and the largest gap = 10.",
                                format(RANK_DIST_CAP, big.mark = ","))),
                    div(class = "ex-source", "A screening aid based on reference rent (AMR, not the winning bid), straight-line rail access and online cuisine engagement; not a sales forecast."))),
          nav_spacer(),
          nav_item(downloadButton("centre_csv", "CSV", icon = icon("file-csv"),
                                  class = "btn-sm btn-outline-success"))
        ))
      )
    ),

    # -------------------------------------------------------------------------
    # Charts, one section per part of the visualisation plan (team's "Latest Decision Template"):
    # Part 2 cuisine (V3, V4, V6, V7), Part 3 tender market (V9), Part 5 transport (V8). Part 4 (land use)
    # has no chart yet (KIV). One row per chart (chart left, story right); all drawn live (server.R), and a
    # centre picked on the map carries over to every chart.
    # -------------------------------------------------------------------------
    mck_section(
      "sec-cuisine", "Cuisine Landscape & Opportunities",
      "Which cuisines are already offered, and where might opportunities exist?",
      reveal = FALSE,
      div(class = "viz-rows",
        viz_row("V3", "RQ1 · V3 · Cuisine supply",
                uiOutput("v3_headline", inline = TRUE), "<b>Occupied main-meal stalls by cuisine</b>, count and share", "Source: Hawker Guide. Figures reflect observed classified occupied main-meal stalls, not an official census.",
                uiOutput("v3_lead", inline = TRUE),
                uiOutput("v3_body", inline = TRUE),
                viz_toolbar("Centre", viz_centre_search("v3", centres$centre_name[centres$centre_id == V3_DEFAULT_ID]),
                            div(class = "viz-add-slot", uiOutput("v3_add"))),
                plotlyOutput("v3_plot", height = "44vh")),
        viz_row("Price", "Price points by cuisine",
                uiOutput("price_headline", inline = TRUE),
                "<b>Mid-point of each stall's listed price range</b>, S$, by cuisine",
                sprintf("Source: Hawker Guide listings (v3). Most stalls (%.0f%%) list only the website's default price of $3–$10: it is shown as the grey band and counted at the end of each row, not as dots. Dots are the stalls with a real price range; cuisines with fewer than %d of them are not shown.",
                        100 * attr(prices, "placeholder_share"), PRICE_MIN_STALLS),
                uiOutput("price_lead", inline = TRUE),
                uiOutput("price_body", inline = TRUE),
                viz_toolbar("Centre", uiOutput("price_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("price_add"))),
                plotlyOutput("price_plot", height = "46vh"),
                div(class = "ex-note", "Each dot is one stall (mid-point of its listed price range); the box spans the middle half of the stalls and the thick line is the median. Orange dots = stalls at the centre selected on the map.")),
        viz_row("V4", "RQ2 · V4 · Cuisine engagement",
                uiOutput("v4_headline", inline = TRUE), "<b>Share of Google Maps restaurant reviews by cuisine and planning area</b> (%)",
                "Source: Google Maps restaurant reviews (classified v3); URA Master Plan 2019. Engagement reflects the share of classified reviews, not actual cuisine purchases. Areas with fewer than 30 classified establishments are excluded from comparison.",
                uiOutput("v4_lead", inline = TRUE),
                uiOutput("v4_body", inline = TRUE),
                viz_toolbar("Cuisine", viz_select("v4_cuisine", c("Leading cuisine by area (Overview)" = "All", V3_ORDER), "All"),
                            uiOutput("v4_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("v4_add"))),
                leafletOutput("v4_map", height = "46vh"),
                uiOutput("v4_note")),
        viz_row("V6", "RQ3 · V6 · Cuisine engagement-supply gap",
                uiOutput("v6_headline", inline = TRUE),
                uiOutput("v6_subtitle", inline = TRUE),
                "Sources: Google Maps reviews (v3); Hawker Guide. Engagement is not measured consumer demand.",
                uiOutput("v6_lead", inline = TRUE),
                uiOutput("v6_body", inline = TRUE),
                viz_toolbar("Centre", viz_centre_search("v6", centres$centre_name[centres$centre_id == V6_DEFAULT_ID]),
                            div(class = "viz-add-slot", uiOutput("v6_add"))),
                plotlyOutput("v6_plot", height = "46vh"),
                div(class = "ex-note", "Negative values indicate higher supply share; positive values indicate higher engagement share.")),
        viz_row("V7", "RQ3 · V7 · Cuisine engagement-supply gap vs rent",
                uiOutput("v7_headline", inline = TRUE), "Compare cuisine engagement\u2013supply gaps and rental costs across hawker centres",
                "Source: Google Maps reviews (v3), Hawker Guide and NEA tender notices. Assessed market rent is an NEA reference value, not the actual winning bid.",
                uiOutput("v7_lead", inline = TRUE),
                uiOutput("v7_body", inline = TRUE),
                viz_toolbar("Cuisine", viz_select("v7_cuisine", V3_ORDER, "Western / European"), uiOutput("v7_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("v7_add"))),
                plotlyOutput("v7_plot", height = "46vh"))
      )
    ),
    mck_section(
      "sec-tender", "Tender Market & Stall Opportunities",
      "Where have stalls been advertised for tender, and what do historical tender patterns and stall characteristics reveal about potential opportunities?",
      reveal = FALSE,
      div(class = "viz-rows",
        viz_row("V9", "RQ5 · V9 · Relisting",
                uiOutput("v9_headline", inline = TRUE),
                "<b>Share of assessed stalls not re-advertised in subsequent tenders</b>",
                paste0("Source: NEA tender notices, ", month_full(MONTHS[1]), "–", month_full(MONTHS[2]),
                       ". Non-return is a proxy for stall take-up, not confirmation of a successful award."),
                uiOutput("v9_lead", inline = TRUE),
                uiOutput("v9_body", inline = TRUE),
                viz_toolbar("Centre", uiOutput("v9_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("v9_add"))),
                plotlyOutput("v9_plot", height = "46vh"),
                uiOutput("v9_note")),
        viz_row("Cost", "Estimated Monthly Rental & Charges",
                uiOutput("cost_headline", inline = TRUE),
                uiOutput("cost_subtitle", inline = TRUE),
                paste0("Source: NEA tender notices, ", month_full(MONTHS[1]), "–", month_full(MONTHS[2]),
                       ". Fees are the mid-points of NEA's ranges; the thin line marks their low–high range. Rent is the assessed market rent, not the winning bid."),
                uiOutput("cost_lead", inline = TRUE),
                uiOutput("cost_body", inline = TRUE),
                viz_toolbar("Range",
                            div(class = "viz-tool cost-slider",
                                sliderInput("cost_window", NULL, min = 50, max = 300, value = COST_WINDOW, step = 50,
                                            pre = "±S$", ticks = FALSE, width = "100%")),
                            uiOutput("cost_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("cost_add"))),
                div(class = "rank-legend",
                    span(span(class = "rank-sw", style = "background:#0D743B"), "Rent (AMR)"),
                    span(span(class = "rank-sw", style = "background:#99CA3C"), "Cleaning fee"),
                    span(span(class = "rank-sw", style = "background:#FAA619"), "Service & conservancy charges")),
                plotlyOutput("cost_plot", height = "46vh"),
                uiOutput("cost_count"))
      )
    ),
    mck_section(
      "sec-landuse", "Surrounding Land Use",
      "What types of land uses surround each hawker centre, and how might their environments differ?",
      reveal = FALSE,
      div(class = "viz-rows",
        viz_row("Land", "Surrounding land use",
                uiOutput("lu_headline", inline = TRUE),
                uiOutput("lu_subtitle", inline = TRUE),
                "Source: URA Master Plan 2025 zoning, measured inside a circle around each centre. Percentages are of the whole circle, including roads, parks and other land (grey). Zoning shows planned use, not customers or footfall.",
                uiOutput("lu_lead", inline = TRUE),
                uiOutput("lu_body", inline = TRUE),
                viz_toolbar("Radius", viz_toggle("lu_radius", c("500 m" = "500", "1 km" = "1000"), "500"),
                            span(class = "viz-toolbar-label", "Centre"), uiOutput("lu_sel", inline = TRUE),
                            div(class = "viz-add-slot", uiOutput("lu_add"))),
                div(class = "lu-wrap",
                    div(class = "lu-donut", plotlyOutput("lu_plot", height = "44vh")),
                    uiOutput("lu_legend")))
      )
    ),
    mck_section(
      "sec-transport", "Transport Accessibility & Activity",
      "Which centres have convenient rail access and are near stations with higher passenger activity?",
      reveal = FALSE,
      div(class = "viz-rows",
        viz_row("V8", "RQ4 · V8 · Access and residents",
                "MRT accessibility and local market potential",
                uiOutput("v8_subtitle", inline = TRUE),
                uiOutput("v8_source", inline = TRUE),
                uiOutput("v8_lead", inline = TRUE),
                uiOutput("v8_body", inline = TRUE),
                viz_toolbar("View",
                            viz_toggle("v8_view", c("Residential population" = "pop", "Rental affordability" = "rent"), "pop"),
                            span(class = "viz-toolbar-label", "Centre"), uiOutput("v8_sel", inline = TRUE),
                            div(class = "viz-add-slot", uiOutput("v8_add"))),
                uiOutput("v8_key"),
                plotlyOutput("v8_plot", height = "42vh"),
                uiOutput("v8_lines"),
                uiOutput("v8_note"),
                detail = uiOutput("v8_detail"))
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
                                       class = "btn-sm btn-outline-success",
                                       title = "Export the table plus every other column we hold about these centres"))),
        uiOutput("compare_ui"),
        card_footer(uiOutput("compare_help"))
      )
    ),

    # -------------------------------------------------------------------------
    # About us: the project, the team, the data, and a small feedback panel
    # (1-5 star rating + subscribe with an email address; server.R answers them)
    # -------------------------------------------------------------------------
    mck_section(
      "sec-about", "About us",
      "The team and the data behind HawkerRadar",
      div(class = "about-grid",
        div(class = "about-main",
          p(class = "about-lead",
            "HawkerRadar is a student project for DBA3702 Descriptive Analytics with AI at the National University of Singapore. It brings NEA's hawker centre list and monthly tender notices, the Hawker Guide's stall listings, SingStat's June 2026 population and LTA's MRT data together, so a prospective hawker can see where the centres are, what they already sell, what renting there costs and how competitive the tenders are."),
          div(class = "about-label", "SA1 Team 03"),
          div(class = "team-grid",
            lapply(list(
              c("Thanh Mai", "Dashboard, map and comparison table"),
              c("Zhen Rong", "Database, rent and budget"),
              c("Ridhwan", "Cuisine supply and demand"),
              c("Lucien", "Access to MRT and residents"),
              c("Casper", "The tender market")),
              function(m) div(class = "team-card", div(class = "team-name", m[1]), div(class = "team-role", m[2])))),
          div(class = "about-label", "Data"),
          p(class = "about-note",
            "NEA hawker centres and tender notices (Oct 2025 – Sep 2026); Hawker Guide; SingStat population, June 2026; URA Master Plan 2019 subzones; LTA MRT/LRT exits and ridership. Rents are NEA's assessed market rents, a reference value rather than the winning bid.")),
        card(
          fill = FALSE, class = "feedback-card",
          div(class = "feedback-title", "How useful was this?"),
          # Five stars; a click sends the rating to the server (input$rating)
          div(class = "star-rating", role = "radiogroup", `aria-label` = "Rate HawkerRadar from 1 to 5 stars",
              lapply(5:1, function(i) tagList(
                tags$input(type = "radio", id = paste0("star-", i), name = "star-rating", value = i),
                tags$label(`for` = paste0("star-", i), title = paste(i, if (i == 1) "star" else "stars"), HTML("&#9733;"))))),
          tags$hr(),
          div(class = "feedback-title", "Stay updated"),
          p(class = "feedback-hint", "Get a note when the tender data or the charts are updated."),
          div(class = "subscribe-row",
              tags$input(type = "email", id = "sub_email", class = "form-control subscribe-input",
                         placeholder = "Email address", autocomplete = "email", `aria-label` = "Email address"),
              actionButton("sub_submit", "Submit", class = "btn-success subscribe-btn"))
        )
      )
    ),

    # Back to map: round button fixed at the bottom right, shown whenever the map is out of view
    tags$button(type = "button", class = "back-to-top", title = "Back to map", `aria-label` = "Back to map",
                onclick = "var m = document.getElementById('sec-map'); if (m) m.scrollIntoView({ behavior: 'smooth', block: 'start' });",
                icon("map-location-dot"))
  )
)
