# =============================================================================
# Hawker Site Finder — ui.R
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
/* While the menu is open the page does not scroll (the gap left by the scrollbar is padded in toggleNav) */
html.nav-lock { overflow: hidden; }
/* Scrollbar in the page's greens (older Chromium / Safari use these rules) */
::-webkit-scrollbar { width: calc(12 * var(--u)); }
::-webkit-scrollbar-track { background: #EEF5E6; }
::-webkit-scrollbar-thumb { background: #80C683; border-radius: calc(8 * var(--u)); border: calc(3 * var(--u)) solid #EEF5E6; }
::-webkit-scrollbar-thumb:hover { background: #40AD49; }
.hero-rest { scroll-snap-align: start; }
body { background: linear-gradient(180deg, #E9F3E0 0%, #F4F9EF 40%, #FFFFFF 100%); background-attachment: fixed; }

/* Header (McKinsey style): menu button in a square on the left, then the name.
   Transparent with white text over the opening photos; white once scrolled past them. */
.app-header { position: fixed; top: 0; left: 0; right: 0; z-index: 1200; height: calc(64 * var(--u)); display: flex; align-items: center;
  background: transparent; color: #FFFFFF; transition: background 0.3s ease, color 0.3s ease, box-shadow 0.3s ease; }
.app-header.solid { background: #FFFFFF; color: #1E2B22; box-shadow: 0 2px calc(12 * var(--u)) rgba(30, 43, 34, 0.08); }
.nav-open .app-header { background: transparent; color: #FFFFFF; box-shadow: none; }
/* Scroll progress: the line under the header fills as you scroll down */
.scroll-progress { position: absolute; left: 0; bottom: 0; height: calc(3 * var(--u)); width: 0; background: #FAA619; transition: opacity 0.2s ease; }
.nav-open .scroll-progress { opacity: 0; }
.brand { display: flex; align-items: center; gap: calc(10 * var(--u)); padding: 0 calc(18 * var(--u)); color: inherit; text-decoration: none; }
.brand:hover { color: inherit; }
.app-title { font-family: 'Libre Baskerville', Georgia, serif; font-weight: 700; font-size: 1.12rem; line-height: 1; }
.hamburger-btn { width: calc(64 * var(--u)); height: calc(64 * var(--u)); flex: none; border: none; border-right: 1px solid rgba(255, 255, 255, 0.28);
  background: transparent; color: inherit; display: flex; flex-direction: column; align-items: center; justify-content: center;
  gap: calc(6 * var(--u)); cursor: pointer; }
.app-header.solid .hamburger-btn { border-right-color: #E3E8EB; }
.hamburger-btn span { display: block; width: calc(26 * var(--u)); height: 2px; border-radius: 2px; background: currentColor;
  transition: transform 0.3s ease, opacity 0.3s ease; }
.hamburger-btn:hover { background: rgba(255, 255, 255, 0.10); }
.app-header.solid .hamburger-btn:hover { background: #EEF5E6; }
.hamburger-btn:focus-visible { outline: 2px solid #FAA619; outline-offset: calc(-4 * var(--u)); }
.nav-open .hamburger-btn span:nth-child(1) { transform: translateY(calc(8 * var(--u))) rotate(45deg); }
.nav-open .hamburger-btn span:nth-child(2) { opacity: 0; }
.nav-open .hamburger-btn span:nth-child(3) { transform: translateY(calc(-8 * var(--u))) rotate(-45deg); }

/* Menu drawer opened by the header button */
.nav-drawer { position: fixed; top: 0; left: 0; bottom: 0; z-index: 1100; width: calc(340 * var(--u)); max-width: 86vw; background: #0D743B;
  color: #FFFFFF; padding: calc(96 * var(--u)) calc(36 * var(--u)) calc(32 * var(--u)); transform: translateX(-100%); transition: transform 0.35s ease; visibility: hidden; }
.nav-open .nav-drawer { transform: none; visibility: visible; }
.nav-drawer nav { display: flex; flex-direction: column; }
.nav-drawer a { color: #FFFFFF; text-decoration: none; font-family: 'IBM Plex Sans', sans-serif; font-size: 1.55rem;
  padding: calc(14 * var(--u)) 0; border-bottom: 1px solid rgba(255, 255, 255, 0.18); }
.nav-drawer a:hover, .nav-drawer a.active { color: #FAA619; }
/* Frosted overlay over the whole page while the menu is open, tinted in the menu's green */
.nav-backdrop { position: fixed; inset: 0; z-index: 1090; background: rgba(9, 52, 29, 0.42);
  -webkit-backdrop-filter: blur(calc(7 * var(--u))) saturate(115%); backdrop-filter: blur(calc(7 * var(--u))) saturate(115%);
  opacity: 0; pointer-events: none; transition: opacity 0.35s ease; }
.nav-open .nav-backdrop { opacity: 1; pointer-events: auto; }

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
.hero-sub { font-size: clamp(1.05rem, 1.5vw, 1.35rem); max-width: calc(760 * var(--u)); margin: calc(16 * var(--u)) auto 0; opacity: 0.95; }
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
   left arrow sitting on the sidebar's edge, level with the Filters title. Closed: a green tab on the left edge
   reading Filters with a right arrow. Both fill green on hover. (Phones keep bslib's own toggle.) */
@media (min-width: 576px) {
  #sec-map .bslib-sidebar-layout { position: relative; }
  #sec-map .bslib-sidebar-layout > .collapse-toggle {
    position: absolute; top: calc(10 * var(--u)); left: calc(var(--_sidebar-width) - 17 * var(--u)); right: auto; bottom: auto;
    display: inline-flex; flex-direction: column; align-items: center; justify-content: center; gap: calc(8 * var(--u));
    width: calc(34 * var(--u)); min-height: calc(34 * var(--u)); padding: 0; margin: 0; border-radius: calc(999 * var(--u));
    border: 1px solid #0D743B; background: #FFFFFF; color: #0D743B; opacity: 1; z-index: 1000; cursor: pointer;
    box-shadow: 0 calc(3 * var(--u)) calc(10 * var(--u)) rgba(13, 116, 59, 0.18), 0 1px calc(2 * var(--u)) rgba(64, 64, 65, 0.12);
    transition: left var(--_transition-duration, 0.3s) var(--_transition-easing, ease), background 0.15s, color 0.15s, box-shadow 0.15s; }
  #sec-map .bslib-sidebar-layout > .collapse-toggle > .collapse-icon { opacity: 1; width: calc(15 * var(--u)); height: calc(15 * var(--u)); }
  #sec-map .bslib-sidebar-layout > .collapse-toggle:hover, #sec-map .bslib-sidebar-layout > .collapse-toggle:focus-visible {
    background: #0D743B; color: #FFFFFF; outline: none; box-shadow: 0 0 0 calc(3 * var(--u)) rgba(13, 116, 59, 0.2); }
  #sec-map .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle {
    left: calc(10 * var(--u)); padding: calc(12 * var(--u)) 0; background: #0D743B; color: #FFFFFF; }
  #sec-map .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle:hover { background: #0A5C2F; }
  #sec-map .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle::before {
    content: 'Filters'; writing-mode: vertical-rl; transform: rotate(180deg); font-size: 0.8rem; font-weight: 600; letter-spacing: 0.04em; }
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
.viz-toolbar .map-search, .viz-toolbar .viz-tool { position: relative; top: auto; left: auto; right: auto; width: calc(260 * var(--u)); max-width: 100%; }
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
.viz-add-done, .viz-add-done:hover { background: #EEF5E6; border-color: #BBD980; color: #4E5D68; cursor: default; }
.cc-add-slot { margin-top: calc(6 * var(--u)); }
.cc-add-slot .viz-add { width: 100%; }
.viz-label { font-family: 'IBM Plex Mono', monospace; font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase;
  color: #0D743B; margin-bottom: calc(10 * var(--u)); }
.viz-copy { font-size: 1.05rem; line-height: 1.75; color: #404041; margin: 0; }
.viz-copy strong { color: #1E2B22; font-weight: 600; }
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

/* McKinsey-style exhibits: label, takeaway headline, subtitle, chart, source */
.exhibit > .card-body { padding: calc(22 * var(--u)) calc(26 * var(--u)); gap: calc(8 * var(--u)); }
.ex-headline { font-size: 1.35rem; font-weight: 600; color: #1E2B22; line-height: 1.3; margin-bottom: calc(4 * var(--u)); }
.ex-subtitle { font-size: 0.95rem; color: #404041; margin-bottom: calc(14 * var(--u)); }
.ex-subtitle b { color: #1E2B22; }
.ex-source { font-size: 0.75rem; font-style: italic; color: #808285; margin-top: 0; line-height: 1.35; }

/* Map */
.leaflet-container { background: #EDF0F2; border-radius: calc(6 * var(--u)); }
/* Expanded map card: the map takes all the height left after the legend and notes */
.bslib-card[data-full-screen='true'] > .card-body { display: flex; flex-direction: column; overflow: hidden; flex: 1 1 0; min-height: 0; }
.bslib-card[data-full-screen='true'] .map-wrap { flex: 1 1 auto; min-height: 0; }
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
// Header menu button: open or close the menu (Home, Data visualization, Map, Table comparison)
function toggleNav(open) {
  var on = (typeof open === 'boolean') ? open : !document.body.classList.contains('nav-open');
  // Lock the page while the menu is open; pad the scrollbar's width so nothing shifts
  var root = document.documentElement;
  if (on && !root.classList.contains('nav-lock')) {
    document.body.style.paddingRight = (window.innerWidth - root.clientWidth) + 'px';
    root.classList.add('nav-lock');
  } else if (!on) {
    root.classList.remove('nav-lock');
    document.body.style.paddingRight = '';
  }
  document.body.classList.toggle('nav-open', on);
  var btn = document.querySelector('.hamburger-btn');
  if (btn) btn.setAttribute('aria-expanded', on ? 'true' : 'false');
  var drawer = document.getElementById('nav-drawer');
  if (drawer) drawer.setAttribute('aria-hidden', on ? 'false' : 'true');
}

$(function () {
  // A menu link, the dimmed backdrop or Escape closes the menu
  document.querySelectorAll('#nav-drawer a, .nav-backdrop').forEach(function (el) {
    el.addEventListener('click', function () { toggleNav(false); });
  });
  document.addEventListener('keydown', function (e) { if (e.key === 'Escape') toggleNav(false); });

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
  function sgFit() {
    var m = sgMap();
    if (!m) return;
    m.invalidateSize();
    // On a wide map Singapore fills the part left of the floating centre card; on a narrow one it uses everything
    var card = document.querySelector('.map-overlay'), padR = 4;
    if (card && m.getSize().x >= 800) padR = card.offsetWidth + 24;
    m.fitBounds(SG_BOUNDS, { paddingTopLeft: [4, 4], paddingBottomRight: [padR, 4], animate: false });
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

  // Map: a centre circle (orange) turns dark green while the mouse is over it
  var hookMap = setInterval(function () {
    var w = window.HTMLWidgets && HTMLWidgets.find('#map');
    var m = w && w.getMap && w.getMap();
    if (!m) return;
    clearInterval(hookMap);
    // Only one circle can be hovered at a time. Leaflet skips mouseout when the map
    // flies to a clicked centre (the circle slides away under a still mouse), which
    // used to leave several circles stuck in dark green; any map move now resets it.
    var hovered = null;
    var unhover = function () {
      if (!hovered) return;
      hovered.setStyle({ fillColor: '#FAA619', fillOpacity: 0.9 });
      hovered.setRadius(hovered._baseRadius);
      hovered = null;
    };
    var hover = function (l) {
      if (!(l instanceof L.CircleMarker) || l.options.fillColor !== '#FAA619' || l._hoverHooked) return;
      l._hoverHooked = true;
      l._baseRadius = l.getRadius();
      l.on('mouseover', function () {
        if (hovered !== l) unhover();
        hovered = l;
        l.setStyle({ fillColor: '#0D743B', fillOpacity: 1 }); l.setRadius(l._baseRadius + 2); l.bringToFront();
      });
      l.on('mouseout', function () { if (hovered === l) unhover(); });
      l.on('remove', function () { if (hovered === l) hovered = null; });
    };
    m.eachLayer(hover);
    m.on('layeradd', function (e) { hover(e.layer); });
    m.on('movestart zoomstart mouseout', unhover);

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
    // Back-to-map button appears once the opening photos are behind and the map is out of view
    var up = document.querySelector('.back-to-top');
    if (up) {
      var mp = document.getElementById('sec-map'), mr = mp ? mp.getBoundingClientRect() : null;
      var mapInView = mr && mr.top < window.innerHeight * 0.5 && mr.bottom > window.innerHeight * 0.5;
      up.classList.toggle('show', h.scrollTop > 400 && !mapInView);
    }
    // Header: transparent over the opening photos, white once they have scrolled by
    var hdr = document.querySelector('.app-header'), hero = document.getElementById('sec-home');
    if (hdr) hdr.classList.toggle('solid', !hero || h.scrollTop > hero.offsetHeight - hdr.offsetHeight - 10);
    // Opening: the title fades and rises over the first 40% of a screen of scrolling,
    // then only the photos show (the shade lightens too) until the page moves on
    var heroText = document.querySelector('.hero-content');
    if (heroText) {
      var f = Math.min(1, Math.max(0, h.scrollTop / (window.innerHeight * 0.4)));
      heroText.style.opacity = String(1 - f);
      heroText.style.transform = 'translate(-50%, ' + Math.round(-50 * f) + 'px)';
      var down = document.querySelector('.hero-down'), shade = document.querySelector('.hero-shade');
      if (down) down.style.opacity = String(0.9 * (1 - Math.min(1, 2 * f)));
      if (shade) shade.style.opacity = String(1 - 0.65 * f);
    }
  };
  window.addEventListener('scroll', progress, { passive: true });
  window.addEventListener('resize', progress);
  progress();

  // The map grows down so the map card ends level with the Reset filters button
  // (never shorter than its design height); in full screen or a stacked layout it keeps that height
  var mapBaseCss = null, mapExtra = -1;
  var syncMapStickyTrack = function () {
    var wrap = document.querySelector('.map-sticky-wrap');
    var card = document.querySelector('.map-floating-card');
    var mapEl = document.getElementById('map');
    var resetBtn = document.getElementById('reset_t1');
    if (!wrap || !card || !mapEl || !resetBtn) return;
    if (mapBaseCss === null) mapBaseCss = mapEl.style.height;
    if (card.getAttribute('data-full-screen') === 'true') return;
    var probe = document.createElement('div');
    probe.style.cssText = 'position:absolute;visibility:hidden;height:' + mapBaseCss;
    document.body.appendChild(probe);
    var baseH = probe.offsetHeight;
    probe.remove();
    var cardBaseH = card.offsetHeight - mapEl.offsetHeight + baseH;
    var targetBottom = resetBtn.getBoundingClientRect().bottom - wrap.getBoundingClientRect().top;
    var extra = Math.max(0, Math.round(targetBottom - cardBaseH));
    if (extra === mapExtra) return;
    mapExtra = extra;
    // The map's own ResizeObserver redraws it and refits Singapore
    mapEl.style.height = extra > 0 ? (baseH + extra) + 'px' : mapBaseCss;
  };
  var mapSidebar = document.querySelector('#sec-map .bslib-sidebar-layout > .sidebar');
  if (mapSidebar && window.ResizeObserver) new ResizeObserver(syncMapStickyTrack).observe(mapSidebar);
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

  // Menu: highlight the section in view
  var links = document.querySelectorAll('#nav-drawer a');
  var spy = new IntersectionObserver(function (entries) {
    entries.forEach(function (e) {
      if (!e.isIntersecting) return;
      links.forEach(function (a) { a.classList.toggle('active', a.getAttribute('href') === '#' + e.target.id); });
    });
  }, { rootMargin: '-45% 0px -50% 0px' });
  document.querySelectorAll('.hero, .mck-section').forEach(function (s) { spy.observe(s); });
});
"
# McKinsey-style exhibit frame: headline, subtitle, the chart (`...`) and a source line
exhibit <- function(code, headline, subtitle, source, ...) {
  card(fill = FALSE, class = "exhibit", `data-code` = code,
       div(class = "ex-headline", headline),
       div(class = "ex-subtitle", HTML(subtitle)),
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
          tags$input(type = "text", id = paste0(prefix, "_search_input"), class = "map-search-input",
                     value = default_name, placeholder = "Search hawker centre",
                     autocomplete = "off", spellcheck = "false"),
          tags$button(type = "button", id = paste0(prefix, "_search_clear"), class = "map-search-clear",
                      style = "display: block;", `aria-label` = "Clear search", tags$i(class = "fa fa-xmark"))),
      div(id = paste0(prefix, "_search_dropdown"), class = "map-search-dropdown"))
}

# Toolbar above a chart: a small label and the control that filters that chart
viz_toolbar <- function(label, ...) div(class = "viz-toolbar", span(class = "viz-toolbar-label", label), ...)

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
                          "Share of Google Maps reviews, %" = "reviews")),
  conditionalPanel("input.map_metric == 'reviews'",
    tag_note("Uses the cuisine chosen in \"Centres that sell\"; \"Any cuisine\" shades each area by its leading cuisine.")),
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
    heading_font = font_google("IBM Plex Sans", local = FALSE),   # sans-serif everywhere (user's request)
    code_font = font_google("IBM Plex Mono", local = FALSE)
  ),
  tags$head(tags$style(HTML(app_css)), tags$script(HTML(paste0("window.CENTRES_DATA = ", CENTRES_SEARCH_JSON, "; window.V3_CENTRES_DATA = ", V3_SEARCH_JSON, "; window.V6_CENTRES_DATA = ", V6_SEARCH_JSON, ";"))), tags$script(HTML(app_js)),
            # Serif for the headings: Libre Baskerville (Google Fonts)
            tags$link(rel = "stylesheet",
                      href = "https://fonts.googleapis.com/css2?family=Libre+Baskerville:wght@400;700&display=swap"),
            tags$meta(name = "theme-color", content = "#0D743B")),

  # ---------------------------------------------------------------------------
  # Header (McKinsey style): menu button and name; transparent over the opening
  # photos, white below them. The menu lists the four parts of the page.
  # ---------------------------------------------------------------------------
  div(class = "app-header", id = "top",
      tags$button(type = "button", class = "hamburger-btn", title = "Menu", `aria-label` = "Menu",
                  `aria-controls` = "nav-drawer", `aria-expanded` = "false", onclick = "toggleNav()",
                  tags$span(), tags$span(), tags$span()),
      tags$a(href = "#sec-home", class = "brand", span(class = "app-title", "Hawker Site Finder")),
      div(class = "scroll-progress")),
  div(id = "nav-drawer", class = "nav-drawer", `aria-hidden` = "true",
      tags$nav(`aria-label` = "Page sections",
               tags$a(href = "#sec-home", class = "active", "Home"),
               tags$a(href = "#sec-map", "Map"),
               tags$a(href = "#sec-viz", "Data visualization"),
               tags$a(href = "#sec-compare", "Table comparison"),
               tags$a(href = "#sec-about", "About us"))),
  div(class = "nav-backdrop"),

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
          h1(class = "hero-title", "Hawker Site Finder"),
          p(class = "hero-sub", "Where should a new hawker open, and what should they sell?"),
          div(class = "hero-meta", paste0("Singapore hawker centres | NEA tender notices, ",
                                          month_label(MONTHS[1]), " – ", month_label(MONTHS[2])))),
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
                leafletOutput("map", height = "calc(620 * var(--u))"),
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
        # fill = FALSE: the card grows with the table instead of scrolling
        card(
          fill = FALSE, class = "table-card",
          card_header(class = "d-flex align-items-center",
                      div(class = "ex-headline table-title", "Centre table"),
                      div(class = "ms-auto",
                          downloadButton("centre_csv", "CSV", icon = icon("file-csv"),
                                         class = "btn-sm btn-outline-success"))),
          DTOutput("centre_table", fill = FALSE)  # size to its rows, no 400 px fill box
        )
      )
    ),

    # -------------------------------------------------------------------------
    # Data visualization: one row per chart (chart left, story right), V3-V9.
    # All charts are drawn live (server.R); a centre picked on the map carries over to V3, V4, V6, V7 and V8.
    # -------------------------------------------------------------------------
    mck_section(
      "sec-viz", "Data visualization",
      "What the data says about cuisine supply and demand, access to customers and the tender market",
      reveal = FALSE,
      div(class = "viz-rows",
        viz_row("V3", "RQ1 · V3 · Cuisine supply",
                uiOutput("v3_headline", inline = TRUE), "<b>Occupied main-meal stalls by cuisine</b>, count and share", "Source: Hawker Guide. Figures reflect observed classified occupied main-meal stalls, not an official census.",
                uiOutput("v3_lead", inline = TRUE),
                uiOutput("v3_body", inline = TRUE),
                viz_toolbar("Centre", viz_centre_search("v3", centres$centre_name[centres$centre_id == V3_DEFAULT_ID]),
                            div(class = "viz-add-slot", uiOutput("v3_add"))),
                plotlyOutput("v3_plot", height = "44vh")),
        viz_row("V4", "RQ2 · V4 · Cuisine engagement",
                uiOutput("v4_headline", inline = TRUE), "<b>Share of classified Google Maps restaurant reviews</b>, % by planning area",
                "Source: Google Maps restaurant reviews (classified v3); URA Master Plan 2019. Engagement reflects the share of classified reviews, not actual cuisine purchases. Areas with fewer than 30 classified establishments are excluded from comparison.",
                uiOutput("v4_lead", inline = TRUE),
                uiOutput("v4_body", inline = TRUE),
                viz_toolbar("Cuisine", viz_select("v4_cuisine", c("Any cuisine (leading one)" = "All", V3_ORDER), "All"),
                            uiOutput("v4_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("v4_add"))),
                leafletOutput("v4_map", height = "46vh")),
        viz_row("V6", "RQ3 · V6 · Engagement vs supply",
                uiOutput("v6_headline", inline = TRUE),
                "<b>Share of Google Maps reviews minus share of occupied stalls</b>, percentage points",
                "Sources: Google Maps reviews (v3); Hawker Guide. Engagement is not measured consumer demand.",
                uiOutput("v6_lead", inline = TRUE),
                uiOutput("v6_body", inline = TRUE),
                viz_toolbar("Centre", viz_centre_search("v6", centres$centre_name[centres$centre_id == V6_DEFAULT_ID]),
                            div(class = "viz-add-slot", uiOutput("v6_add"))),
                plotlyOutput("v6_plot", height = "46vh")),
        viz_row("V7", "RQ3 · V7 · Gap vs rent",
                "Where an opening and a lower rent meet", "<b>Engagement\u2013supply gap vs median assessed market rent</b>, percentage points and S$",
                "Source: Google Maps reviews (v3), Hawker Guide and NEA tender notices. Engagement–supply gaps are exploratory indicators, not measured unmet demand. Assessed market rent is an NEA reference value, not the actual winning bid.",
                uiOutput("v7_lead", inline = TRUE),
                uiOutput("v7_body", inline = TRUE),
                viz_toolbar("Cuisine", viz_select("v7_cuisine", V3_ORDER, "Western / European"), uiOutput("v7_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("v7_add"))),
                plotlyOutput("v7_plot", height = "46vh")),
        viz_row("V8", "RQ4 · V8 · Access and residents",
                "MRT accessibility and local market potential",
                "<b>Exit distance, station tap-outs and subzone residents</b>",
                "Source: LTA MRT/LRT exits and Sep 2026 station activity; SingStat 2026 population",
                uiOutput("v8_lead", inline = TRUE),
                "The chart compares 127 centres using straight-line distance to the nearest MRT/LRT exit, average daily tap-outs at its station, and residents of its subzone. Bubble size shows population, and green bubbles are the close and busy ones. Station exits are not measured hawker visitors.",
                viz_toolbar("Centre", uiOutput("v8_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("v8_add"))),
                plotlyOutput("v8_plot", height = "42vh"),
                detail = uiOutput("v8_detail")),
        viz_row("V9", "RQ5 · V9 · Relisting",
                uiOutput("v9_headline", inline = TRUE),
                "<b>Share of assessed stalls not re-advertised</b>, % at the centre picked on the map",
                paste0(TENDER_SOURCE, ". Non-return is a proxy for a stall being taken, not proof of an award."),
                uiOutput("v9_lead", inline = TRUE),
                uiOutput("v9_body", inline = TRUE),
                viz_toolbar("Centre", uiOutput("v9_sel", inline = TRUE), div(class = "viz-add-slot", uiOutput("v9_add"))),
                plotlyOutput("v9_plot", height = "46vh"))
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
      "The team and the data behind Hawker Site Finder",
      div(class = "about-grid",
        div(class = "about-main",
          p(class = "about-lead",
            "Hawker Site Finder is a student project for DBA3702 Descriptive Analytics with AI at the National University of Singapore. It brings NEA's hawker centre list and monthly tender notices, the Hawker Guide's stall listings, SingStat's June 2026 population and LTA's MRT data together, so a prospective hawker can see where the centres are, what they already sell, what renting there costs and how competitive the tenders are."),
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
          div(class = "star-rating", role = "radiogroup", `aria-label` = "Rate Hawker Site Finder from 1 to 5 stars",
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
