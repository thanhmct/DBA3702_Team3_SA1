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
html { scroll-behavior: smooth; scroll-snap-type: y proximity; scrollbar-color: #80C683 #EEF5E6; }
/* While the menu is open the page does not scroll (the gap left by the scrollbar is padded in toggleNav) */
html.nav-lock { overflow: hidden; }
/* Scrollbar in the page's greens (older Chromium / Safari use these rules) */
::-webkit-scrollbar { width: 12px; }
::-webkit-scrollbar-track { background: #EEF5E6; }
::-webkit-scrollbar-thumb { background: #80C683; border-radius: 8px; border: 3px solid #EEF5E6; }
::-webkit-scrollbar-thumb:hover { background: #40AD49; }
.hero-rest { scroll-snap-align: start; }
body { background: linear-gradient(180deg, #E9F3E0 0%, #F4F9EF 40%, #FFFFFF 100%); background-attachment: fixed; }

/* Header (McKinsey style): menu button in a square on the left, then the name.
   Transparent with white text over the opening photos; white once scrolled past them. */
.app-header { position: fixed; top: 0; left: 0; right: 0; z-index: 1200; height: 64px; display: flex; align-items: center;
  background: transparent; color: #FFFFFF; transition: background 0.3s ease, color 0.3s ease, box-shadow 0.3s ease; }
.app-header.solid { background: #FFFFFF; color: #1E2B22; box-shadow: 0 2px 12px rgba(30, 43, 34, 0.08); }
.nav-open .app-header { background: transparent; color: #FFFFFF; box-shadow: none; }
/* Scroll progress: the line under the header fills as you scroll down */
.scroll-progress { position: absolute; left: 0; bottom: 0; height: 3px; width: 0; background: #FAA619; transition: opacity 0.2s ease; }
.nav-open .scroll-progress { opacity: 0; }
.brand { display: flex; align-items: center; gap: 10px; padding: 0 18px; color: inherit; text-decoration: none; }
.brand:hover { color: inherit; }
.app-title { font-family: 'Libre Baskerville', Georgia, serif; font-weight: 700; font-size: 1.12rem; line-height: 1; }
.hamburger-btn { width: 64px; height: 64px; flex: none; border: none; border-right: 1px solid rgba(255, 255, 255, 0.28);
  background: transparent; color: inherit; display: flex; flex-direction: column; align-items: center; justify-content: center;
  gap: 6px; cursor: pointer; }
.app-header.solid .hamburger-btn { border-right-color: #E3E8EB; }
.hamburger-btn span { display: block; width: 26px; height: 2px; border-radius: 2px; background: currentColor;
  transition: transform 0.3s ease, opacity 0.3s ease; }
.hamburger-btn:hover { background: rgba(255, 255, 255, 0.10); }
.app-header.solid .hamburger-btn:hover { background: #EEF5E6; }
.hamburger-btn:focus-visible { outline: 2px solid #FAA619; outline-offset: -4px; }
.nav-open .hamburger-btn span:nth-child(1) { transform: translateY(8px) rotate(45deg); }
.nav-open .hamburger-btn span:nth-child(2) { opacity: 0; }
.nav-open .hamburger-btn span:nth-child(3) { transform: translateY(-8px) rotate(-45deg); }

/* Menu drawer opened by the header button */
.nav-drawer { position: fixed; top: 0; left: 0; bottom: 0; z-index: 1100; width: 340px; max-width: 86vw; background: #0D743B;
  color: #FFFFFF; padding: 96px 36px 32px; transform: translateX(-100%); transition: transform 0.35s ease; visibility: hidden; }
.nav-open .nav-drawer { transform: none; visibility: visible; }
.nav-drawer nav { display: flex; flex-direction: column; }
.nav-drawer a { color: #FFFFFF; text-decoration: none; font-family: 'IBM Plex Sans', sans-serif; font-size: 1.55rem;
  padding: 14px 0; border-bottom: 1px solid rgba(255, 255, 255, 0.18); }
.nav-drawer a:hover, .nav-drawer a.active { color: #FAA619; }
/* Frosted overlay over the whole page while the menu is open, tinted in the menu's green */
.nav-backdrop { position: fixed; inset: 0; z-index: 1090; background: rgba(9, 52, 29, 0.42);
  -webkit-backdrop-filter: blur(7px) saturate(115%); backdrop-filter: blur(7px) saturate(115%);
  opacity: 0; pointer-events: none; transition: opacity 0.35s ease; }
.nav-open .nav-backdrop { opacity: 1; pointer-events: auto; }

/* Opening section: photos sliding behind the title. The section is taller than the screen and its
   stage stays put (sticky) while you scroll: the title fades away (script in app_js), then for a
   stretch only the photos show, then the page moves on. */
.hero { position: relative; height: 175vh; margin: 0 -0.75rem; background: #10241A; color: #FFFFFF; scroll-snap-align: start; }
.hero-stage { position: sticky; top: 0; height: 100vh; min-height: 560px; overflow: hidden; }
.hero-rest { position: absolute; top: 62vh; left: 0; width: 1px; height: 1px; }   /* resting point: photos only */
.hero .ov-slider { position: absolute; inset: 0; aspect-ratio: auto; border-radius: 0; }
.hero-shade { position: absolute; inset: 0; pointer-events: none;
  background: linear-gradient(180deg, rgba(8, 20, 14, 0.55) 0%, rgba(8, 20, 14, 0.10) 30%, rgba(8, 20, 14, 0.30) 58%, rgba(8, 20, 14, 0.88) 100%); }
.hero-content { position: absolute; left: 50%; bottom: 17vh; transform: translateX(-50%); width: min(1100px, 90vw); text-align: center;
  will-change: opacity, transform; }
.hero-kicker { font-size: 1rem; opacity: 0.9; margin-bottom: 12px; }
.hero-title { font-family: 'Libre Baskerville', Georgia, serif; font-weight: 700; font-size: clamp(2.4rem, 4.6vw, 4.4rem); line-height: 1.15;
  margin: 0; text-shadow: 0 2px 24px rgba(0, 0, 0, 0.35); }
.hero-shade { will-change: opacity; }
.hero-sub { font-size: clamp(1.05rem, 1.5vw, 1.35rem); max-width: 760px; margin: 16px auto 0; opacity: 0.95; }
.hero-meta { font-size: 0.95rem; margin-top: 22px; opacity: 0.85; }
.hero-down { position: absolute; left: 50%; bottom: 4vh; transform: translateX(-50%); color: #FFFFFF; opacity: 0.9; }
.hero-down:hover { color: #FFFFFF; opacity: 1; }
.hero-down svg { display: block; width: 46px; height: 24px; }
.hero-credit { position: absolute; right: 18px; bottom: 12px; font-size: 0.72rem; opacity: 0.7; }
@media (prefers-reduced-motion: no-preference) { .hero-down { animation: hero-bob 2.4s ease-in-out infinite; } }
@keyframes hero-bob { 0%, 100% { transform: translate(-50%, 0); } 50% { transform: translate(-50%, 6px); } }

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
.sidebar .btn { padding: 4px 8px !important; font-size: 0.78rem !important; }

/* Sticky floating map: stays fixed & elevated until scrolled past reset filter */
.map-sticky-wrap { position: relative; margin-bottom: 14px; }
.map-floating-card { position: sticky; top: 72px; z-index: 95; border-radius: 8px;
  box-shadow: 0 12px 30px rgba(13, 116, 59, 0.16), 0 4px 12px rgba(64, 64, 65, 0.10) !important;
  transition: box-shadow 0.25s ease; }

/* Map filters: bslib's own collapse toggle (on the sidebar edge) hides or shows them */
.bslib-sidebar-layout > .main { padding: 8px 16px 20px !important; }

/* Boxes: no borders, soft outer shadow, compact & balanced spacing */
.card, .bslib-value-box { border: none !important; border-radius: 7px; background: #FFFFFF;
  box-shadow: 0 4px 12px rgba(13, 116, 59, 0.08), 0 1px 3px rgba(64, 64, 65, 0.06) !important; }
.card-header, .card-footer { background: #FFFFFF; border: none; }
.bslib-value-box { min-height: unset !important; height: auto !important; margin-bottom: 0 !important; }
.bslib-value-box > .card-body { padding: 0 !important; margin: 0 !important; }
.bslib-value-box .value-box-grid { grid-template-columns: 1fr !important; gap: 0 !important; }
.bslib-value-box .value-box-area { padding: 8px 12px !important; display: flex !important; flex-direction: column !important; justify-content: center !important; gap: 2px !important; }
.bslib-value-box .value-box-title { font-size: 0.72rem !important; font-weight: 500 !important; color: #64748B !important; line-height: 1.2 !important; min-height: 2.1em !important; margin: 0 0 1px !important; display: flex !important; align-items: flex-start !important; }
.bslib-value-box .value-box-value { font-family: 'IBM Plex Sans', sans-serif !important; font-weight: 600 !important; font-size: 1.35rem !important; color: #1E2B22 !important; line-height: 1.15 !important; margin: 1px 0 !important; white-space: nowrap !important; }
.vb-sub { font-size: 0.70rem !important; color: #64748B !important; margin: 0 !important; line-height: 1.2 !important; min-height: 1.2em !important; white-space: nowrap !important; overflow: hidden !important; text-overflow: ellipsis !important; }
.mono { font-family: 'IBM Plex Mono', monospace !important; }
.card.exhibit[data-code='V1+V2'] > .card-body { padding: 12px 14px; gap: 4px; }
#sec-map .bslib-grid { gap: 10px !important; margin-bottom: 10px !important; }
#sec-map .bslib-grid .bslib-grid-item { min-height: unset !important; }

/* Photo slider: the track holds the photos side by side and slides left */
.ov-slider { position: relative; overflow: hidden; background: #1E2B22; }
.ov-track { display: flex; height: 100%; transition: transform 1.1s cubic-bezier(0.65, 0, 0.35, 1); will-change: transform; }
.ov-slide { flex: 0 0 100%; width: 100%; height: 100%; object-fit: cover; object-position: center bottom; display: block; user-select: none; }

/* Page body below the opening photos (the filters sit beside the map only) */
.page-main { padding: 28px 20px 24px; }

/* Data visualization: one row per chart, chart left and story right (mirrored McKinsey layout) */
/* Each row fills one screen below the header, so a chart and its story are seen on their own;
   scrolling settles on a row (proximity snap: free scrolling elsewhere, e.g. in the map) */
.viz-rows { margin-top: 4px; }
.viz-row { display: grid; grid-template-columns: minmax(0, 1.65fr) minmax(0, 1fr); gap: 56px; align-items: center;
  min-height: calc(100vh - 64px); box-sizing: border-box; padding: 40px 0; border-top: 1px solid #E3E8EB;
  scroll-snap-align: start; scroll-margin-top: 64px; }
.viz-rows > .viz-row:first-child { border-top: none; }
.viz-chart .exhibit { margin: 0; }
.viz-row .placeholder-body { height: min(56vh, 560px); }
.viz-image-link { display: block; }
.viz-image { display: block; width: 100%; height: auto; max-height: 62vh; object-fit: contain; }
/* Exhibits in the Data visualization rows: heading on the left, then one labelled toolbar (search, dropdown or
   segmented control) directly above the chart it controls, then the chart */
.viz-toolbar { display: flex; align-items: center; justify-content: center; gap: 14px; flex-wrap: wrap;
  margin: 0 0 16px; padding: 4px 0 16px; border-bottom: 1px solid #EDF0F2; }
.viz-toolbar-label { font-family: 'IBM Plex Mono', monospace; font-size: 0.72rem; letter-spacing: 0.08em;
  text-transform: uppercase; color: #0D743B; }
.viz-toolbar .map-search, .viz-toolbar .viz-tool { position: relative; top: auto; left: auto; right: auto; width: 340px; max-width: 100%; }
.viz-toolbar .map-search-box { box-shadow: none; border: 1px solid #D5DBDF; }
.viz-toolbar .map-search-box:focus-within { border-color: #0D743B; box-shadow: 0 0 0 3px rgba(13, 116, 59, 0.15); }
/* Dropdown inside the same box as the search */
.viz-select-box .form-group { margin: 0; width: 100%; }
.viz-select-box select { width: 100%; border: none; outline: none; box-shadow: none; background-color: transparent;
  padding: 9px 34px 9px 36px; border-radius: 6px; font-size: 0.9rem; font-family: inherit; color: #1E2B22; cursor: pointer; }
.viz-select-box select:focus { box-shadow: none; }
/* Segmented control: two or three choices side by side, the chosen one filled green */
/* Chip naming the centre picked on the map (V4, V7, V8) */
.viz-chip { display: inline-flex; align-items: center; gap: 6px; font-size: 0.82rem; color: #404041;
  background: #F1F4F2; border-radius: 999px; padding: 6px 12px; }
.viz-chip b { color: #1E2B22; font-weight: 600; }
.viz-chip-dot { width: 9px; height: 9px; border-radius: 50%; background: #FAA619; box-shadow: 0 0 0 2px #FFFFFF, 0 0 0 3.5px #1E2B22; }
.viz-chip-off, .viz-chip-off b { color: #808285; font-weight: 400; }
.viz-seg { display: inline-flex; background: #F1F4F2; border-radius: 8px; padding: 3px; gap: 2px; }
.viz-seg-btn { border: none; background: transparent; color: #404041; font-family: inherit; font-size: 0.88rem;
  padding: 7px 18px; border-radius: 6px; cursor: pointer; transition: background 0.15s, color 0.15s; }
.viz-seg-btn:hover { background: #E3EBE4; }
.viz-seg-btn.active, .viz-seg-btn.active:hover { background: #0D743B; color: #FFFFFF; }
.viz-top5 { margin: 10px 0 0; padding-left: 0; list-style: none; font-size: 0.8rem; line-height: 1.5; color: #58595B; }
.viz-top5 b { color: #1E2B22; font-weight: 600; }
.viz-text { max-width: 520px; }
.viz-label { font-family: 'IBM Plex Mono', monospace; font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase;
  color: #0D743B; margin-bottom: 10px; }
.viz-copy { font-size: 1.05rem; line-height: 1.75; color: #404041; margin: 0; }
.viz-copy strong { color: #1E2B22; font-weight: 600; }
@media (max-width: 1100px) { .viz-row { grid-template-columns: 1fr; gap: 16px; } .viz-text { max-width: none; } }

/* About us: text and team on the left, the small feedback panel on the right */
.about-grid { display: grid; grid-template-columns: minmax(0, 1.6fr) minmax(0, 1fr); gap: 48px; align-items: start;
  max-width: 1400px; margin: 0 auto; padding-bottom: 40px; }
@media (max-width: 1100px) { .about-grid { grid-template-columns: 1fr; } }
.about-lead { font-size: 1.1rem; line-height: 1.75; color: #404041; margin: 0 0 22px; }
.about-label { font-family: 'IBM Plex Mono', monospace; font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase;
  color: #0D743B; margin: 18px 0 10px; }
.team-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 10px; }
.team-card { background: #FFFFFF; border-radius: 7px; padding: 12px 14px; box-shadow: 0 4px 12px rgba(13, 116, 59, 0.08), 0 1px 3px rgba(64, 64, 65, 0.06); }
.team-name { font-weight: 600; color: #1E2B22; }
.team-role { font-size: 0.85rem; color: #58595B; margin-top: 2px; }
.about-note { font-size: 0.92rem; line-height: 1.6; color: #58595B; margin: 0; }
/* Feedback panel: everything centred around the star rating */
.feedback-card > .card-body { padding: 30px 32px; align-items: center; text-align: center; gap: 0; }
.feedback-card hr { align-self: stretch; margin: 24px 0 22px; border-color: #E3E8EB; opacity: 1; }
.feedback-title { font-weight: 600; font-size: 1.15rem; color: #1E2B22; }
.feedback-hint { font-size: 0.88rem; color: #808285; margin: 6px 0 14px; }
/* Star rating: stars are written 5..1 and shown right-to-left, so hovering a star lights it and those before it */
.star-rating { display: inline-flex; flex-direction: row-reverse; justify-content: center; gap: 8px; margin: 14px auto 0; }
.star-rating input { position: absolute; opacity: 0; pointer-events: none; }
.star-rating label { font-size: 2.6rem; line-height: 1; color: #D9DEE3; cursor: pointer; transition: color 0.15s ease, transform 0.15s ease; }
.star-rating label:hover, .star-rating label:hover ~ label, .star-rating input:checked ~ label { color: #FAA619; }
.star-rating label:hover { transform: scale(1.12); }
.star-rating input:focus-visible + label { outline: 2px solid #0D743B; outline-offset: 2px; border-radius: 4px; }
.subscribe-row { display: flex; gap: 8px; align-self: stretch; }
.subscribe-input { flex: 1; min-width: 0; }
.subscribe-input:focus { border-color: #40AD49; box-shadow: 0 0 0 3px rgba(64, 173, 73, 0.18); }
.subscribe-btn { flex: none; padding: 0 24px; background: #0D743B; border-color: #0D743B; color: #FFFFFF; font-weight: 600; }
.subscribe-btn:hover, .subscribe-btn:focus { background: #40AD49; border-color: #40AD49; color: #FFFFFF; }

/* Comparison: the section fills one screen below the header; the table card stretches to the
   bottom and its content (the empty-shortlist message or the table) sits in the middle */
#sec-compare { min-height: calc(100vh - 64px); display: flex; flex-direction: column; box-sizing: border-box; padding-bottom: 24px; }
#sec-compare > .card { flex: 1 1 auto; }
/* About us (last section) is at least one screen tall, so the menu can bring its rule under the header too */
#sec-about { min-height: calc(100vh - 64px); }
/* The table always starts at the top of the card; only the empty-shortlist message is centred */
#sec-compare > .card > .card-body { display: flex; flex-direction: column; justify-content: flex-start; }
#compare_ui { flex: 1 1 auto; display: flex; flex-direction: column; }
#compare_ui > .compare-empty { margin: auto 0; }

/* Map section: the filters are a sidebar inside this section only */
#sec-map .bslib-sidebar-layout { border: none; }

/* McKinsey-style sections */
/* Jumping to a section (menu, snap) leaves its rule 16 px under the 64 px header: 64 - 40 (head padding) + 16 */
.mck-section { scroll-margin-top: 40px; margin-bottom: 1.75rem; scroll-snap-align: start; }
#sec-map.mck-section { margin-bottom: 0.85rem; }
/* Section heading (as on MGI pages): a centred rule, then the title and one grey line, centred */
.sec-head { text-align: center; padding: 40px 0 32px; }
.sec-rule { width: min(860px, 56%); height: 1px; background: #A7ADB2; margin: 0 auto 52px; }
.sec-title { font-family: 'Libre Baskerville', Georgia, serif; font-size: clamp(1.9rem, 2.8vw, 2.7rem); font-weight: 700; color: #1E2B22;
  margin: 0 0 18px; line-height: 1.2; }
.sec-kicker { font-size: 1.15rem; color: #58595B; margin: 0 auto; max-width: 820px; line-height: 1.45; }

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
/* Expanded map card: the map takes all the height left after the legend and notes */
.bslib-card[data-full-screen='true'] > .card-body { display: flex; flex-direction: column; overflow: hidden; }
.bslib-card[data-full-screen='true'] .map-wrap { flex: 1 1 auto; min-height: 0; }
.bslib-card[data-full-screen='true'] #map { position: absolute; inset: 0; height: auto !important; width: 100% !important; }
.bslib-card[data-full-screen='true'] .map-legend-below, .bslib-card[data-full-screen='true'] .map-note,
.bslib-card[data-full-screen='true'] .ex-source { flex: none; }
.map-legend-below .info-title { font-weight: 600; font-size: 0.85rem; }
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
.tt-big { font-family: 'IBM Plex Sans', sans-serif; font-size: 1.7rem; color: #0D743B; line-height: 1.1; margin-top: 2px; }
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
.compare-empty-title { font-family: 'IBM Plex Sans', sans-serif; font-size: 1.35rem; color: #1E2B22; }
.compare-empty-text { color: #58595B; margin: 6px auto 14px; max-width: 560px; }
.compare-empty-quick { display: flex; flex-wrap: wrap; gap: 8px; justify-content: center; align-items: center; }

/* Back to top: round up-arrow button, bottom right, shown after scrolling down */
.back-to-top { position: fixed; right: 24px; bottom: 24px; z-index: 1050; width: 46px; height: 46px; border-radius: 50%;
  border: none; background: #0D743B; color: #FFFFFF; font-size: 1.1rem; display: flex; align-items: center; justify-content: center;
  box-shadow: 0 6px 16px rgba(13, 116, 59, 0.30); opacity: 0; pointer-events: none; transform: translateY(10px);
  transition: opacity 0.25s ease, transform 0.25s ease, background 0.2s ease; }
.back-to-top.show { opacity: 1; pointer-events: auto; transform: none; }
.back-to-top:hover { background: #40AD49; }
/* Notifications: white cards in the page's style, above the back-to-top button. Each one fades in
   while flying in from the right (opening its own space, so older ones are pushed up smoothly: the
   panel is anchored at the bottom and new ones are added below), and fades out when it goes. */
#shiny-notification-panel { bottom: 80px !important; right: 18px !important; width: 360px !important; max-width: calc(100vw - 36px); padding: 0 !important; }
#shiny-notification-panel .shiny-notification {
  background: #FFFFFF; color: #1E2B22; opacity: 1; border: none; border-left: 4px solid #0D743B; border-radius: 8px;
  box-shadow: 0 10px 28px rgba(13, 116, 59, 0.16), 0 2px 6px rgba(30, 43, 34, 0.10);
  padding: 12px 14px 12px 16px; margin: 0 0 10px; font-family: 'IBM Plex Sans', sans-serif; font-size: 0.92rem; line-height: 1.45;
  animation: notif-in 0.55s cubic-bezier(0.22, 1, 0.36, 1) backwards; }
#shiny-notification-panel .shiny-notification-message { border-left-color: #0D743B; }
#shiny-notification-panel .shiny-notification-warning { border-left-color: #FAA619; }
#shiny-notification-panel .shiny-notification-error { border-left-color: #C2412D; }
#shiny-notification-panel .shiny-notification a { color: #0D743B; font-weight: 600; }
#shiny-notification-panel .shiny-notification-close { color: #808285; font-weight: 400; }
#shiny-notification-panel .shiny-notification-close:hover { color: #1E2B22; }
@keyframes notif-in {
  0%   { opacity: 0; transform: translateX(70px); max-height: 0; margin-bottom: 0; padding-top: 0; padding-bottom: 0; overflow: hidden; }
  35%  { max-height: 240px; margin-bottom: 10px; padding-top: 12px; padding-bottom: 12px; }
  100% { opacity: 1; transform: none; max-height: 240px; } }
@media (prefers-reduced-motion: reduce) { #shiny-notification-panel .shiny-notification { animation: none; } }

/* Centre card: header, stat tiles, cuisine mix (compacted 30%) */
.cc-name { font-family: 'IBM Plex Sans', sans-serif; font-size: 0.92rem; line-height: 1.2; color: #1E2B22; }
.cc-meta { font-size: 0.65rem; color: #58595B; margin-top: 2px; display: flex; flex-wrap: wrap; gap: 4px; align-items: center; }
.cc-chip { background: #DDECC1; color: #0D743B; font-weight: 600; font-size: 0.58rem; padding: 1px 5px; border-radius: 999px; }
.cc-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 5px; margin: 8px 0 6px; }
.cc-stat { background: #F6F8F9; border-radius: 4px; padding: 5px 6px; }
.cc-label { font-size: 0.52rem; text-transform: uppercase; letter-spacing: 0.04em; color: #58595B; font-weight: 600; }
.cc-value { font-family: 'IBM Plex Sans', sans-serif; font-size: 0.88rem; color: #1E2B22; line-height: 1.15; }
.cc-note { font-size: 0.55rem; color: #808285; line-height: 1.15; }
.cc-section-title { font-size: 0.65rem; font-weight: 600; color: #1E2B22; margin: 3px 0 0; }
.cc-title-row { display: flex; align-items: center; justify-content: space-between; gap: 8px; }
.cc-link { font-size: 0.65rem; font-weight: 600; color: #0D743B; text-decoration: none; white-space: nowrap; }
.cc-link:hover { text-decoration: underline; color: #0D743B; }
.cc-link .fa { margin-left: 3px; font-size: 0.6rem; }
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

  // Segmented controls (V9): the buttons set a Shiny input
  $(document).on('click', '.viz-seg-btn', function () {
    var $b = $(this);
    var $g = $b.closest('.viz-seg');
    $g.find('.viz-seg-btn').removeClass('active');
    $b.addClass('active');
    Shiny.setInputValue($g.data('input'), $b.data('value'));
  });

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
    // Back-to-top button appears after scrolling down a little
    var up = document.querySelector('.back-to-top');
    if (up) up.classList.toggle('show', h.scrollTop > 400);
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

# Segmented control: buttons that set input$<id> to the value of the chosen one
viz_segmented <- function(id, choices, selected = choices[[1]]) {
  div(class = "viz-seg", `data-input` = id,
      lapply(seq_along(choices), function(i) {
        tags$button(type = "button", class = paste("viz-seg-btn", if (identical(choices[[i]], selected)) "active"),
                    `data-value` = choices[[i]], names(choices)[i])
      }))
}

# Data visualization row: the chart on the left and its story on the right (a
# mirrored McKinsey layout), one row per chart. The story opens with a bold
# takeaway that runs into the explanation. `...` = the chart output; empty = placeholder.
viz_row <- function(code, label, headline, subtitle, source, lead, body, ...) {
  div(class = "viz-row reveal", id = paste0("viz-", tolower(code)),
      div(class = "viz-chart", exhibit(code, headline, subtitle, source, ...)),
      div(class = "viz-text",
          div(class = "viz-label", label),
          p(class = "viz-copy", tags$strong(lead), " ", body)))
}

# Month label that does not depend on the computer's language, e.g. "Oct 25"
month_label <- function(d) paste(month.abb[as.integer(format(d, "%m"))], format(d, "%y"))

TENDER_SOURCE <- paste0("Source: NEA tender notices, ", month_label(MONTHS[1]), "–", month_label(MONTHS[2]))

# Opening photos (faces already hidden), shown as a slider behind the title
OVERVIEW_SLIDES <- sort(list.files("www/slides", pattern = "\\.(jpg|jpeg|png|webp)$"))

# Map filters: a sidebar inside the map section only, grouped by type
map_filters <- sidebar(
  title = "Filters", width = 270,
  selectInput("region", "Region", choices = c("All regions" = "All", named_choices(REGIONS))),
  selectInput("pa", "Planning area", choices = c("All planning areas" = "All", named_choices(PAS))),
  selectInput("cuisine", "Centres that sell", choices = c("Any cuisine" = "All", CUISINE_GROUPS)),
  selectInput("map_metric", "Shade planning areas by",
              choices = c("Number of centres" = "n",
                          "Centres per 10 km²" = "density",
                          "Cooked-food stalls" = "stalls",
                          "Residents" = "residents")),
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
                      div(class = "cc-section-title cc-title-row", "Cuisine mix (stalls)",
                          tags$a(href = "#viz-v3", class = "cc-link", "Full breakdown", icon("arrow-down"))),
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
      )
    ),

    # -------------------------------------------------------------------------
    # Data visualization: one row per chart (chart left, story right), V3-V10.
    # All charts are drawn live (server.R); a centre picked on the map carries over to V3, V4, V6, V7 and V8.
    # -------------------------------------------------------------------------
    mck_section(
      "sec-viz", "Data visualization",
      "What the data says about cuisine supply and demand, access to customers and the tender market",
      reveal = FALSE,
      div(class = "viz-rows",
        viz_row("V3", "RQ1 · V3 · Cuisine supply",
                uiOutput("v3_headline", inline = TRUE), "<b>Occupied main-meal stalls by cuisine</b>, count and share", "Source: Hawker Guide",
                uiOutput("v3_lead", inline = TRUE),
                uiOutput("v3_body", inline = TRUE),
                viz_toolbar("Centre", viz_centre_search("v3", centres$centre_name[centres$centre_id == V3_DEFAULT_ID])),
                plotlyOutput("v3_plot", height = "44vh")),
        viz_row("V4", "RQ2 · V4 · Cuisine engagement",
                "Which cuisines draw attention, area by area", "<b>Share of Google Maps reviews for the chosen cuisine</b>, %",
                "Source: Google Maps establishments classified by cuisine (v3); URA Master Plan 2019",
                "Western food draws the most reviews in 21 of the 31 areas with hawker centres.",
                "Choose a cuisine and the map shades each planning area by its share of classified Google Maps reviews. Chinese food leads in 9 areas and Indian in one, Rochor. Western food takes 7% to 74% of reviews, with a median of 32%. Only about half of the places could be classified, areas with fewer than 30 classified places stay grey, and review share shows attention rather than measured demand.",
                viz_toolbar("Cuisine", viz_select("v4_cuisine", V3_ORDER, "Western / European"), uiOutput("v4_sel", inline = TRUE)),
                leafletOutput("v4_map", height = "46vh")),
        viz_row("V5", "RQ2 · V5 · Preference by age · waiting for the survey",
                "Age-grouped cuisine preference", "<b>Estimated preference by age group</b>, %",
                "Source: synthetic cuisine survey; SingStat, June 2026",
                "Tastes differ by age, and so do neighbourhoods.",
                "Grouped bars will compare cuisine preferences across age groups, so a hawker can match the menu to the residents who dominate an area, from young families to retirees. The survey's age groups have to be matched to SingStat's single-year ages before they can be weighted."),
        viz_row("V6", "RQ3 · V6 · Engagement vs supply",
                uiOutput("v6_headline", inline = TRUE),
                "<b>Share of Google Maps reviews minus share of occupied stalls</b>, percentage points",
                "Source: Google Maps reviews by cuisine (v3); Hawker Guide",
                uiOutput("v6_lead", inline = TRUE),
                uiOutput("v6_body", inline = TRUE),
                viz_toolbar("Centre", viz_centre_search("v6", centres$centre_name[centres$centre_id == V6_DEFAULT_ID])),
                plotlyOutput("v6_plot", height = "46vh")),
        viz_row("V7", "RQ3 · V7 · Gap vs rent",
                "Where an opening and a lower rent meet", "<b>Engagement\u2013supply gap vs median assessed market rent</b>, percentage points and S$",
                "Source: Google Maps reviews by cuisine (v3); Hawker Guide; NEA tender notices",
                "For Western food, 33 of 68 centres pair an opening with a below-median rent.",
                "Each dot is a centre with tender listings: across, the cuisine's share of reviews in its planning area minus its share of stalls at the centre; up, its median assessed market rent. Green dots are under-supplied and below the median rent (S$1,115 for Western). Western food is 1% of stalls at the median centre and absent from 33 centres. The gap is an exploratory signal, not unmet demand, and the rent is NEA's reference value, not the winning bid.",
                viz_toolbar("Cuisine", viz_select("v7_cuisine", V3_ORDER, "Western / European"), uiOutput("v7_sel", inline = TRUE)),
                plotlyOutput("v7_plot", height = "46vh")),
        viz_row("V8", "RQ4 · V8 · Access and residents",
                "MRT accessibility and local market potential",
                "<b>Exit distance, station tap-outs and subzone residents</b>",
                "Source: LTA MRT/LRT exits and Sep 2026 station activity; SingStat 2026 population",
                "Our Tampines Hub ranks first on the three-part access and market score.",
                "The chart compares 127 centres using straight-line distance to the nearest MRT/LRT exit, average daily tap-outs at its station, and residents of its subzone. Bubble size shows population. The three measures have equal weight in a relative score; station exits are not measured hawker visitors.",
                viz_toolbar("Centre", uiOutput("v8_sel", inline = TRUE)),
                plotlyOutput("v8_plot", height = "42vh"), uiOutput("v8_top5")),
        viz_row("V9", "RQ5 · V9 · Relisting",
                "Where stalls get taken", "<b>Share of assessed stalls not re-advertised</b>, %", TENDER_SOURCE,
                uiOutput("v9_lead", inline = TRUE),
                uiOutput("v9_body", inline = TRUE),
                viz_toolbar("Show",
                    viz_segmented("v9_half", c("At or above the overall rate" = "best", "Below the overall rate" = "worst"))),
                plotlyOutput("v9_plot", height = "46vh")),
        viz_row("V10", "RQ5 · V10 · Tender timeline",
                "Tender timeline", "<b>Listings per month</b>, number", TENDER_SOURCE,
                "A non-halal hawker can bid on about 15 stalls a month.",
                "Month by month, the stalls not reserved for Indian or halal food ranged from 10 to 20 (14.8 on average), and 83% of those listings were in the Central region. April 2026 stood out, with 12 second-release stalls, where any single bid wins. Per centre the picture is sparse: only about a quarter of the centre-month cells have a listing, which shapes how this timeline is drawn.")
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
        card_footer(tag_note("Double-click a row to show the centre on the map · drag ⋮⋮ to reorder · click a column header to sort · ✕ removes a centre · the cuisine column follows \"Centres that sell\" in the map filters (stalls of that cuisine, or the main cuisine when it is \"Any cuisine\")."))
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

    # Back to top: round up-arrow button fixed at the bottom right
    tags$button(type = "button", class = "back-to-top", title = "Back to top", `aria-label` = "Back to top",
                onclick = "window.scrollTo({ top: 0, behavior: 'smooth' });",
                icon("arrow-up"))
  )
)
