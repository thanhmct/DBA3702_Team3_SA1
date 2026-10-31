# =============================================================================
# Hawker Site Finder — server.R
# =============================================================================

server <- function(input, output, session) {

  # A leaflet map only accepts leafletProxy() calls once it has rendered (a map
  # on a hidden tab renders when the tab is first shown). Its _bounds input
  # arrives at that moment, so observers wait for this flag before drawing.
  map_ready <- function(id) {
    ready <- reactiveVal(FALSE)
    observeEvent(input[[paste0(id, "_bounds")]], ready(TRUE), once = TRUE)
    ready
  }

  # ===========================================================================
  # Map filters
  # ===========================================================================

  # Region and planning area go together: a region limits the planning areas,
  # and picking a planning area sets its region.
  observeEvent(input$region, {
    pas <- if (input$region == "All") PAS else
      sort(unique(centres$planning_area_name[centres$region_name == input$region]))
    keep <- if (isolate(input$pa) %in% pas) isolate(input$pa) else "All"
    updateSelectInput(session, "pa", choices = c("All planning areas" = "All", named_choices(pas)),
                      selected = keep)
  }, ignoreInit = TRUE)

  observeEvent(input$pa, {
    if (input$pa == "All") return()
    r <- unique(centres$region_name[centres$planning_area_name == input$pa])[1]
    if (!is.na(r) && input$region != r) updateSelectInput(session, "region", selected = r)
  }, ignoreInit = TRUE)

  # Centres in the chosen region / planning area. A planning area left over from
  # another region is ignored (the observer above is about to reset it).
  area_centres <- function(region, pa) {
    d <- centres
    if (region != "All") d <- filter(d, region_name == region)
    if (pa != "All" && pa %in% d$planning_area_name) d <- filter(d, planning_area_name == pa)
    d
  }

  # Rent range of an area's centres in S$50 steps: what the rent slider spans there
  rent_range <- function(b) {
    rent <- b$median_amr[!is.na(b$median_amr)]
    if (!length(rent)) return(AMR_RANGE)
    r <- c(floor(min(rent) / 50) * 50, ceiling(max(rent) / 50) * 50)
    if (r[1] == r[2]) r[2] <- r[1] + 50
    r
  }

  # What the map filters use: the area (a planning area, or else the region), the cuisine and the slider
  # values. They follow the inputs, except that picking an area resets the cuisine list and the sliders
  # below and takes the new values at once, so the map is drawn once and not again when the browser sends
  # the same values back. A planning area that only sets its region keeps the same area: no redraw either.
  filt <- reactiveValues(area = list(region = "All"), cuisine = "All", amr = AMR_RANGE, mrt = c(0, MRT_MAX), vac = 0)
  observeEvent(input$cuisine, filt$cuisine <- input$cuisine)
  # The "all cuisines" entry of "Centres that sell" reads "Leading cuisine by area (Overview)" while the map is
  # shaded by cuisine engagement share (there it shades each area by its leading cuisine), "Any cuisine" otherwise
  cuisine_choices <- function(cu)
    setNames(c("All", cu), c(if (identical(input$map_metric, "reviews")) "Leading cuisine by area (Overview)" else "Any cuisine", cu))
  observeEvent(input$map_metric, {
    b <- area_centres(input$region, input$pa)
    cu <- intersect(CUISINE_GROUPS, cuisine_by_centre$cuisine_group[cuisine_by_centre$centre_id %in% b$centre_id])
    cur <- input$cuisine
    if (cur != "All") cu <- intersect(CUISINE_GROUPS, union(cu, cur))
    updateSelectInput(session, "cuisine", choices = cuisine_choices(cu), selected = cur)
  }, ignoreInit = TRUE)
  observeEvent(input$max_amr, filt$amr <- as.numeric(input$max_amr))
  observeEvent(input$max_mrt, filt$mrt <- as.numeric(input$max_mrt))
  observeEvent(input$min_vacant, filt$vac <- as.numeric(input$min_vacant))

  # Keep the other filters consistent with the area: only cuisines sold there,
  # and rent / MRT sliders that span what exists there.
  observeEvent(list(input$region, input$pa), {
    b <- area_centres(input$region, input$pa)
    cu <- intersect(CUISINE_GROUPS,
                    cuisine_by_centre$cuisine_group[cuisine_by_centre$centre_id %in% b$centre_id])
    cur <- if (isTRUE(input$cuisine %in% cu)) input$cuisine else "All"
    updateSelectInput(session, "cuisine", choices = cuisine_choices(cu), selected = cur)
    r <- rent_range(b)
    updateSliderInput(session, "max_amr", min = r[1], max = r[2], value = r)
    mrt <- b$nearest_exit_distance_m[!is.na(b$nearest_exit_distance_m)]
    m <- if (length(mrt)) ceiling(max(mrt) / 100) * 100 else MRT_MAX
    updateSliderInput(session, "max_mrt", min = 0, max = m, value = c(0, m))
    vmax <- max(c(1, b$vacant_units), na.rm = TRUE)
    updateSliderInput(session, "min_vacant", min = 0, max = vmax, value = 0)
    filt$area <- if (isTRUE(input$pa %in% b$planning_area_name)) list(pa = input$pa) else list(region = input$region)
    filt$cuisine <- cur; filt$amr <- r; filt$mrt <- c(0, m); filt$vac <- 0
  }, ignoreInit = TRUE, priority = 10)   # before the map redraws, so they see the new values

  observeEvent(input$reset_t1, {
    updateSelectInput(session, "region", selected = "All")
    updateSelectInput(session, "pa", choices = c("All planning areas" = "All", named_choices(PAS)),
                      selected = "All")
    updateSelectInput(session, "cuisine", choices = cuisine_choices(CUISINE_GROUPS),
                      selected = "All")
    updateSliderInput(session, "max_amr", min = AMR_RANGE[1], max = AMR_RANGE[2], value = AMR_RANGE)
    updateSliderInput(session, "max_mrt", min = 0, max = MRT_MAX, value = c(0, MRT_MAX))
    updateCheckboxInput(session, "only_tenders", value = FALSE)
    updateCheckboxInput(session, "only_vacant", value = FALSE)
    updateSliderInput(session, "min_vacant", min = 0, max = VAC_MAX, value = 0)
    updateSelectInput(session, "map_metric", selected = "n")
    updateCheckboxInput(session, "map_exits", value = FALSE)
    filt$area <- list(region = "All"); filt$cuisine <- "All"
    filt$amr <- AMR_RANGE; filt$mrt <- c(0, MRT_MAX); filt$vac <- 0
    selected_centre(DEFAULT_CENTRE)  # the map search box follows; no zoom on reset
  }, priority = 10)

  # Centres that pass the map filters
  fc <- reactive({
    a <- filt$area
    area <- if (!is.null(a$pa)) centres[centres$planning_area_name %in% a$pa, ]
            else if (a$region != "All") centres[centres$region_name %in% a$region, ]
            else centres
    d <- area
    if (filt$cuisine != "All") {
      ids <- cuisine_by_centre$centre_id[cuisine_by_centre$cuisine_group == filt$cuisine]
      d <- filter(d, centre_id %in% ids)
    }
    # Centres without a rent figure stay in until the lower handle is moved above the area's lowest rent
    # (the area's own: its slider starts there, not at the lowest rent in Singapore)
    rent <- filt$amr
    if (rent[1] > rent_range(area)[1]) {
      d <- filter(d, !is.na(median_amr) & median_amr >= rent[1] & median_amr <= rent[2])
    } else {
      d <- filter(d, is.na(median_amr) | (median_amr >= rent[1] & median_amr <= rent[2]))
    }
    mrt <- filt$mrt
    if (mrt[1] > 0) {
      d <- filter(d, !is.na(nearest_exit_distance_m) & nearest_exit_distance_m >= mrt[1] & nearest_exit_distance_m <= mrt[2])
    } else {
      d <- filter(d, is.na(nearest_exit_distance_m) | (nearest_exit_distance_m >= mrt[1] & nearest_exit_distance_m <= mrt[2]))
    }
    if (isTRUE(input$only_tenders)) d <- filter(d, n_listings > 0)
    # Only centres where Hawker Guide recorded at least one vacant unit (no data counts as none)
    if (isTRUE(input$only_vacant)) d <- filter(d, coalesce(vacant_units, 0) > 0)
    # V2 constraint: vacant stalls (Hawker Guide); centres without data count as 0
    d <- filter(d, coalesce(vacant_units, 0) >= filt$vac)
    d
  })

  selected_centre <- reactiveVal(DEFAULT_CENTRE)  # card opens on Tekka Centre (has data in every chart)

  # ===========================================================================
  # Map section: headline numbers, map, centre card, centre table
  # ===========================================================================

  output$vb_centres <- renderText(comma(nrow(fc())))
  output$vb_listings <- renderText(comma(sum(fc()$n_listings)))
  output$vb_listings_sub <- renderText({
    d <- fc()
    paste0(plural(sum(d$n_tender_stalls), "distinct stall"), " · ",
           plural(sum(d$n_listings > 0), "centre"))
  })
  output$vb_rent <- renderText({
    r <- suppressWarnings(range(fc()$median_amr, na.rm = TRUE))
    if (!all(is.finite(r))) "n/a"
    else if (r[1] == r[2]) fmt_sgd(r[1])
    else paste0(fmt_sgd(r[1]), "–", comma(r[2]))
  })

  # How many centres pass the filters; warn when none do
  output$t1_count <- renderUI({
    n <- nrow(fc())
    if (n == 0) {
      div(class = "alert alert-warning small p-2 mb-2",
          "No centres match these filters. Loosen a filter or press Reset filters.")
    } else {
      tag_note(paste(n, "of", nrow(centres), "centres match."))
    }
  })
  output$vb_mrt <- renderText({
    m <- mean(fc()$nearest_exit_distance_m, na.rm = TRUE)
    if (is.nan(m)) "n/a" else paste0(comma(round(m)), " m")
  })

  # Planning-area statistics for the choropleth
  area_stats <- reactive({
    s <- fc() |>
      group_by(planning_area_name) |>
      summarise(n = n(), stalls = sum(stalls, na.rm = TRUE), .groups = "drop")
    a <- planning_areas |>
      left_join(s, by = "planning_area_name") |>
      mutate(n = coalesce(n, 0L), stalls = coalesce(stalls, 0),
             density = n / area_km2 * 10,
             residents = coalesce(resident_population, 0))
    # Share of Google Maps reviews for the chosen cuisine (same rule as V4: areas with
    # fewer than V47_MIN_ESTABLISHMENTS classified places stay NA / grey)
    a$reviews <- NA_real_
    a$lead_cuisine <- NA_character_
    if (identical(input$map_metric, "reviews")) {
      r <- review_share_by_area(filt$cuisine)   # "Any cuisine" = each area's leading cuisine
      i <- match(a$planning_area_name, r$planning_area_name)
      a$reviews <- r$reviews[i]
      a$lead_cuisine <- r$lead_cuisine[i]
    }
    a
  })

  # pad_right is a first guess for the centre card; the page script refits Singapore to its real width
  output$map <- renderLeaflet(
    base_map(pad_right = 270) |>
      addMapPane("centres", zIndex = 410) |>    # centre circles above the shaded areas, whichever is redrawn last
      addMapPane("selected", zIndex = 430))     # the selected centre above both

  # Bins, colours and labels for the planning-area shading: one definition used
  # by the map and by the legend in the info panel beside it
  map_scale <- reactive({
    five <- BRAND_SEQ5
    if (identical(input$map_metric, "reviews")) {
      # five equal bins that follow the cuisine shown (review_scale in global.R); same as V4
      sc <- review_scale(area_stats()$reviews)
      return(list(bins = sc$bins, cols = REVIEW_COLS, labels = sc$labels,
                  title = if (identical(filt$cuisine, "All")) "Leading cuisine's<br>engagement share (%)"
                          else paste0(filt$cuisine, "<br>engagement share (%)")))
    }
    switch(input$map_metric,
      n = list(bins = c(0, 1, 3, 6, 10, Inf), cols = five,
               labels = c("0", "1–2", "3–5", "6–9", "10 or more"),
               title = "Hawker centres per planning area"),
      density = list(bins = c(0, 0.001, 2, 4, 7, Inf), cols = five,
                     labels = c("0", "Under 2", "2–4", "4–7", "7 or more"),
                     title = "Centres per 10 km²"),
      stalls = list(bins = c(0, 1, 101, 251, 451, Inf), cols = five,
                    labels = c("0", "1–100", "101–250", "251–450", "451 or more"),
                    title = "Cooked-food stalls per planning area"),
      residents = list(bins = c(0, 1, 10000, 50000, 100000, 200000, Inf), cols = BRAND_SEQ,
                       labels = c("0", "Under 10,000", "10,000–49,999", "50,000–99,999",
                                  "100,000–199,999", "200,000 or more"),
                       title = "Residents per planning area (2026)"),
      reviews = NULL)
  })

  # Units shown under the big number in the tooltips
  METRIC_UNITS <- list(n = "hawker centres", density = "centres per 10 km²",
                       stalls = "cooked-food stalls", residents = "residents (June 2026)")
  fmt_metric <- function(m, v) {
    if (m == "density") sprintf("%.1f", v)
    else if (m == "reviews") ifelse(is.na(v), "n/a", sprintf("%.1f%%", v))
    else comma(round(v))
  }
  metric_unit <- function(m, v) {
    if (m != "reviews") return(METRIC_UNITS[[m]])
    if (filt$cuisine == "All")
      return(ifelse(is.na(v), FEW_PLACES_TIP,
                    paste0("of reviews go to ", area_stats()$lead_cuisine, ", the leading cuisine")))
    ifelse(is.na(v), FEW_PLACES_TIP,
           paste0("of classified Google Maps reviews · ", filt$cuisine))
  }

  output$map_legend <- renderUI({
    sc <- map_scale()
    tagList(
      div(class = "info-title", HTML(sc$title)),
      lapply(seq_along(sc$labels), function(i)
        div(class = "legend-row",
            span(class = "legend-swatch", style = paste0("background:", sc$cols[i], ";")),
            sc$labels[i])),
      if (identical(input$map_metric, "reviews"))
        tagList(
          div(class = "legend-row", span(class = "legend-swatch", style = paste0("background:", FEW_PLACES_FILL, ";")),
              "Too few food establishments"),
          div(class = "legend-row", span(class = "legend-swatch", style = paste0("background:", NO_CENTRE_FILL, ";")),
              "No hawker centres (excluded)")),
      div(class = "legend-row", span(class = "legend-dot"), "Hawker centre"),
      if (isTRUE(input$map_exits))
        div(class = "legend-row", mrt_icon_img(),
            paste0("MRT/LRT exit (", comma(nrow(mrt_exits)), ")"))
    )
  })

  t1_map_ready <- map_ready("map")

  # Zoom to the chosen planning area or region whenever the area changes; "All" (or Reset) zooms back out
  # to Singapore. The page script (fit_map in app_js) keeps the area clear of the search box (top left)
  # and the floating centre card (top right), measured at their real sizes.
  observeEvent(filt$area, {
    req(t1_map_ready())
    a <- filt$area
    sel <- if (!is.null(a$pa)) planning_areas[planning_areas$planning_area_name %in% a$pa, ]
           else if (a$region != "All") planning_areas[planning_areas$region_name %in% a$region, ]
    if (is.null(sel) || !nrow(sel)) session$sendCustomMessage("fit_map", list(all = TRUE))
    else session$sendCustomMessage("fit_map", list(all = FALSE, bounds = as.numeric(sf::st_bbox(sel))))
  }, ignoreInit = TRUE)

  # The map is drawn in layers that update on their own: the shaded planning areas, the centres, the
  # MRT/LRT exits and the selected centre. Ticking the exits, say, redraws only the exits.
  map_tt <- labelOptions(className = "mck-tt", direction = "right", offset = c(18, 0))
  observe({
    req(t1_map_ready())
    a <- area_stats()
    metric <- input$map_metric
    vals <- a[[metric]]
    sc <- map_scale()
    pal <- colorBin(sc$cols, domain = vals, bins = sc$bins, right = FALSE, na.color = FEW_PLACES_FILL)
    fills <- pal(vals)
    # McKinsey-style tooltips: name, the shaded metric as a big serif number, details
    big <- fmt_metric(metric, vals)
    area_labels <- sprintf(
      "<div class='tt-title'>%s</div><div class='tt-big'>%s</div><div class='tt-sub'>%s</div><div class='tt-meta'>%d centres · %s stalls · %s residents</div>",
      a$pa_title, big, metric_unit(metric, vals),
      a$n, comma(a$stalls), comma(a$residents)) |>
      lapply(htmltools::HTML)
    if (metric == "reviews") {   # no hawker centre there: excluded from the comparison
      nc <- a$planning_area_name %in% NO_CENTRE_AREAS
      fills[nc] <- NO_CENTRE_FILL
      area_labels[nc] <- lapply(sprintf("<div class='tt-title'>%s</div><div class='tt-big'>–</div><div class='tt-sub'>%s</div>",
                                        a$pa_title[nc], NO_CENTRE_TIP), htmltools::HTML)
    }
    leafletProxy("map") |>
      clearGroup("areas") |>
      addPolygons(data = a, group = "areas", fillColor = fills, fillOpacity = 0.75,
                  color = "#FFFFFF", weight = 1, label = area_labels, labelOptions = map_tt,
                  highlightOptions = highlightOptions(weight = 2.5, color = "#1E2B22",
                                                      fillOpacity = 0.95, bringToFront = FALSE))
  })

  observe({
    req(t1_map_ready())
    d <- fc()
    p <- leafletProxy("map") |> clearGroup("centres")
    if (!nrow(d)) return()
    # Centre tooltip (V1+V2): cuisine mix, vacant stalls and rent of the centre
    centre_labels <- sprintf(
      "<div class='tt-title'>%s</div><div class='tt-big'>%s</div><div class='tt-sub'>cooked-food stalls · %s</div><div class='tt-meta'>%s<br>Vacant stalls: %s of %s observed · %d tender listings%s</div>",
      d$centre_name, fmt_num(d$stalls), d$pa_title,
      ifelse(is.na(d$top3), "No cuisine data", d$top3),
      fmt_num(d$vacant_units), fmt_num(d$observed_units), d$n_listings,
      ifelse(is.na(d$median_amr), "", paste0("<br>Median rent ", fmt_sgd(d$median_amr)))) |>
      lapply(htmltools::HTML)
    # pane "centres": the page script finds these circles by it (hover, double-click)
    p |> addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude, layerId = ~centre_id, group = "centres",
                          radius = ~3 + sqrt(coalesce(stalls, 0)) / 2,
                          color = "#FFFFFF", weight = 1.5, fillColor = "#FAA619", fillOpacity = 0.9,
                          label = centre_labels, labelOptions = map_tt, options = pathOptions(pane = "centres"))
  })

  # MRT/LRT exits (merged from the old Location tab)
  observe({
    req(t1_map_ready())
    p <- leafletProxy("map") |> clearGroup("exits")
    if (isTRUE(input$map_exits))
      p |> addMarkers(data = mrt_exits, lng = ~longitude, lat = ~latitude, group = "exits",
                      icon = mrt_marker_icon, label = ~station_name,
                      options = markerOptions(riseOnHover = TRUE))
  })

  # Selected centre: drawn on top in dark green with a pulsing ring (group and
  # pane "selected"). It ignores the mouse, so hover and clicks reach the circle below.
  draw_selected <- function(id) {
    p <- leafletProxy("map") |> clearGroup("selected")
    if (is.null(id)) return(invisible())
    c <- centres[centres$centre_id == as.numeric(id), ]
    if (!nrow(c) || is.na(c$latitude)) return(invisible())
    r <- 3 + sqrt(coalesce(c$stalls, 0)) / 2
    p |>
      addCircleMarkers(lng = c$longitude, lat = c$latitude, group = "selected", radius = r + 8,
                       color = BRAND$green_dark, weight = 2.5, fill = FALSE,
                       options = pathOptions(pane = "selected", interactive = FALSE, className = "sel-ring")) |>
      addCircleMarkers(lng = c$longitude, lat = c$latitude, group = "selected", radius = r + 1.5,
                       color = "#FFFFFF", weight = 2, fillColor = BRAND$green_dark, fillOpacity = 1,
                       options = pathOptions(pane = "selected", interactive = FALSE, className = "sel-dot"))
  }
  observe({
    req(t1_map_ready())
    draw_selected(selected_centre())
  })

  # Double-click a row of the comparison table: show that centre on the map
  # (the row's script also scrolls the page up to the map)
  observeEvent(input$compare_focus, {
    id <- as.numeric(input$compare_focus)
    c <- centres[centres$centre_id == id, ]
    if (!nrow(c)) return()
    selected_centre(id)
    leafletProxy("map") |> flyTo(c$longitude, c$latitude, zoom = 14)
  })

  # Click = show the centre card (and pick the centre for the charts)
  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    if (is.null(id)) return()  # MRT/LRT exit dots have no id
    selected_centre(id)
    c <- centres[centres$centre_id == id, ]
    if (nrow(c) && !is.na(c$latitude)) {
      leafletProxy("map") |> flyTo(c$longitude, c$latitude, zoom = 15)
    }
  })
  # Double-click = add the centre to the comparison table. The browser spots the double-click and sends
  # the centre's id (app_js), so a busy server cannot miss it; the map's own double-click zoom is off (base_map())
  observeEvent(input$map_centre_dblclick, add_to_compare(input$map_centre_dblclick))

  # Lets the UI show the cuisine chart and Add to comparison button only once a centre is chosen
  output$centre_selected <- reactive(!is.null(selected_centre()))
  outputOptions(output, "centre_selected", suspendWhenHidden = FALSE)

  # Centre card: name and status, then key numbers as labelled tiles
  output$centre_card <- renderUI({
    id <- selected_centre()
    if (is.null(id)) {
      return(div(class = "cc-empty", icon("hand-pointer"),
                 "Click a centre on the map or a row in the table to see its details here."))
    }
    c <- centres[centres$centre_id == id, ]
    tile <- function(label, value, note = NULL) {
      div(class = "cc-stat", div(class = "cc-label", label), div(class = "cc-value", value),
          if (!is.null(note)) div(class = "cc-note", note))
    }
    tags$div(
      div(class = "cc-name", c$centre_name),
      div(class = "cc-meta", span(class = "cc-chip", c$nea_status), paste0(c$pa_title, " · ", c$region_title)),
      div(class = "cc-grid",
          tile("Cooked-food stalls", fmt_num(c$stalls), "NEA count"),
          tile("Median rent", fmt_sgd(c$median_amr), if (is.na(c$median_amr)) "no tender listing" else "per month (AMR)"),
          tile("Tender listings", c$n_listings, if (c$n_listings > 0) c$latest_listing else "none in Oct 25 – Sep 26"),
          tile("Vacant stalls", fmt_num(c$vacant_units),
               if (is.na(c$observed_units)) "no Hawker Guide data" else paste("of", c$observed_units, "observed")),
          tile("Residents", fmt_num(c$resident_population), "in the subzone, 2026"),
          tile("Nearest MRT", paste0(fmt_num(c$nearest_exit_distance_m), " m"), c$nearest_mrt_station))
    )
  })

  observeEvent(input$add_compare, {
    id <- selected_centre()
    if (!is.null(id)) toggle_compare(id)
  })

  output$centre_cuisine <- renderPlotly({
    id <- selected_centre()
    if (is.null(id)) return(empty_plot("Cuisine mix appears here"))
    d <- filter(cuisine_by_centre, centre_id == id) |> arrange(stalls)
    # Centre being rebuilt whose stalls moved to an interim centre in the same
    # subzone (e.g. Bukit Timah Market -> Bukit Timah Interim Hawker Centre):
    # the Hawker Guide stalls are recorded under the interim centre
    c <- centres[centres$centre_id == id, ]
    has_interim <- isTRUE(c$nea_status == "Under Construction") &&
      any(centres$nea_status == "Interim Centre" & centres$subzone_name == c$subzone_name, na.rm = TRUE)
    if (nrow(d) == 0 && has_interim) return(empty_plot("Under construction – stalls operate at the interim centre"))
    if (nrow(d) == 0) return(empty_plot("No cuisine data for this centre"))
    # McKinsey style: direct labels, accent colour on the main cuisine, grey for the rest
    cols <- ifelse(d$stalls == max(d$stalls), BRAND$green_dark, "#C8CDD2")
    plot_ly(d, x = ~stalls, y = ~factor(cuisine_group, levels = cuisine_group),
            type = "bar", orientation = "h", marker = list(color = cols),
            text = ~stalls, textposition = "outside", cliponaxis = FALSE,
            textfont = list(color = "#404041", size = 9),
            hovertemplate = "%{y}: %{x} stalls<extra></extra>") |>
      mck_plotly(xaxis = list(showticklabels = FALSE),
                 yaxis = list(tickfont = list(size = 9)),
                 font = list(size = 9),
                 margin = list(l = 5, r = 25, t = 2, b = 2))
  })

  centre_table_data <- reactive({
    fc() |>
      transmute(centre_id, Centre = centre_name, `Planning area` = pa_title,
                Stalls = stalls, `Tender listings` = n_listings,
                `Median rent (S$)` = median_amr,
                `MRT exit (m)` = round(nearest_exit_distance_m),
                `Residents (subzone)` = resident_population)
  })

  # 10 rows per page and no scroll areas: the header and the page's rows are
  # always visible together (user's request: no scrollbars)
  output$centre_table <- renderDT({
    d <- centre_table_data()
    i <- match(as.numeric(isolate(selected_centre())), d$centre_id)   # the map selection, if the filters keep it
    datatable(d, rownames = FALSE, class = "compact hover",
              selection = list(mode = "single", selected = if (is.na(i)) NULL else i),
              options = list(dom = "frtip", paging = TRUE, pageLength = 10,
                             displayStart = if (is.na(i)) 0 else ((i - 1) %/% 10) * 10,
                             language = list(search = "", searchPlaceholder = "Search hawker centre or planning area"),
                             columnDefs = list(list(visible = FALSE, targets = 0))))
  })

  # The map selection (a click on the map, a search, a chart, reset) is also the table's selected row:
  # the row is highlighted and the table goes to its page (in the table's current sort and search);
  # no row if the filters hide it. A freshly drawn table (filters changed) opens with the row selected,
  # see renderDT above.
  centre_table_proxy <- dataTableProxy("centre_table")
  # Centres whose row the server selected itself: the browser echoes each one back through
  # input$centre_table_rows_selected, and that echo must not count as a new pick. Without this, two
  # quick selections chased each other forever (A -> B -> A -> B ...).
  table_echo <- numeric(0)
  observeEvent(selected_centre(), {
    id <- as.numeric(selected_centre())
    i <- match(id, centre_table_data()$centre_id)
    if (!identical(as.integer(i), as.integer(isolate(input$centre_table_rows_selected)))) {
      if (!is.na(i)) table_echo <<- tail(c(table_echo, id), 5)
      selectRows(centre_table_proxy, if (is.na(i)) NULL else i)
    }
    if (is.na(i)) return()
    order_now <- input$centre_table_rows_all           # row numbers after the table's search and sort
    pos <- if (length(order_now)) match(i, order_now) else i
    if (!is.na(pos)) selectPage(centre_table_proxy, (pos - 1) %/% 10 + 1)
  })

  # CSV of the filtered table (button at the top right of the card)
  output$centre_csv <- downloadHandler(
    filename = function() paste0("hawker_centres_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(select(centre_table_data(), -centre_id), file, row.names = FALSE, na = "")
    }
  )

  observeEvent(input$centre_table_rows_selected, {
    i <- input$centre_table_rows_selected
    if (!length(i)) return()
    id <- centre_table_data()$centre_id[i]
    k <- match(id, table_echo)
    if (!is.na(k)) { table_echo <<- table_echo[-seq_len(k)]; return() }   # our own selectRows() coming back
    selected_centre(id)
  })

  # ===========================================================================
  # Comparison table (V11): the investor's shortlist, any number of centres
  # ===========================================================================
  shortlist <- reactiveVal(character(0))                 # centre ids, in display order
  compare_sort <- reactiveVal(list(col = NULL, desc = FALSE))

  # Notifications stay on screen 25% shorter than before (6 s -> 4.5 s, 5 s -> 3.75 s, 4 s -> 3 s)
  NOTIF_SCALE <- 0.75
  notif_secs <- function(secs) secs * NOTIF_SCALE

  name_of <- function(id) centres$centre_name[match(as.numeric(id), centres$centre_id)]
  go_link <- tags$a(href = "#sec-compare", class = "fw-semibold", "Go to comparison table ↓")

  # Add centres (no duplicates) and say so in a notification with a link down
  add_to_compare <- function(ids) {
    ids <- as.character(ids)
    new <- setdiff(ids, shortlist())
    shortlist(c(shortlist(), new))
    compare_sort(list(col = NULL, desc = FALSE))
    msg <- if (length(ids) == 1 && length(new)) {
      tagList(tags$b(name_of(ids)), " added to the comparison table below. ")
    } else if (length(ids) == 1) {
      tagList(tags$b(name_of(ids)), " is already in the comparison table. ")
    } else {
      paste(plural(length(new), "centre"), "added to the comparison table below. ")
    }
    showNotification(tagList(msg, go_link), type = "message", duration = notif_secs(6))
  }

  # The Add to comparison buttons are toggles: a second click takes the centre off the shortlist
  toggle_compare <- function(id) {
    id <- as.character(id)
    if (!length(id) || is.na(id)) return()
    if (!id %in% shortlist()) return(add_to_compare(id))
    shortlist(setdiff(shortlist(), id))
    showNotification(tagList(tags$b(name_of(id)), " removed from the comparison table. ", go_link),
                     type = "message", duration = notif_secs(6))
  }

  # Quick start for an empty shortlist: the 5 best centres among those that pass the map filters,
  # by one criterion (centres with no value for it are skipped)
  QUICK_N <- 5
  quick_add <- function(d, key, decreasing = FALSE) {
    ids <- head(d$centre_id[order(key, decreasing = decreasing, na.last = NA)], QUICK_N)
    if (length(ids)) add_to_compare(ids)
    else showNotification("No centres match these filters, or none has data for this.", type = "warning", duration = notif_secs(5))
  }
  observeEvent(input$quick_rent, { d <- fc(); quick_add(d, d$median_amr) })
  observeEvent(input$quick_vacant, {
    d <- filter(fc(), coalesce(vacant_units, 0) > 0)
    quick_add(d, d$vacant_units, decreasing = TRUE)
  })
  observeEvent(input$quick_mrt, { d <- fc(); quick_add(d, d$nearest_exit_distance_m) })
  # Busiest station (average daily tap-outs, V8) and most residents in the subzone
  observeEvent(input$quick_busy, {
    d <- fc(); quick_add(d, v8$avg_daily_tap_outs[match(d$centre_id, v8$centre_id)], decreasing = TRUE)
  })
  observeEvent(input$quick_pop, { d <- fc(); quick_add(d, d$resident_population, decreasing = TRUE) })
  # Most stalls you can bid on (as the cuisine in "Centres that sell"; "Any" = not reserved stalls)
  observeEvent(input$quick_open, {
    d <- fc()
    n <- stall_hint |> filter(allowed_trade_hint %in% open_hints(input$cuisine)) |> count(centre_id)
    quick_add(d, n$n[match(d$centre_id, n$centre_id)], decreasing = TRUE)
  })
  # Best tender record (V9): the largest share of assessed stalls that never came back
  observeEvent(input$quick_tender, {
    d <- fc(); v <- v9_all[match(d$centre_id, v9_all$centre_id), ]
    quick_add(d, v$share_taken + v$stalls_assessed / 1e4, decreasing = TRUE)   # more assessed stalls break ties
  })
  # Biggest engagement - supply gap for the cuisine in "Centres that sell" (V6, V7)
  observeEvent(input$quick_gap, {
    d <- fc(); req(input$cuisine != "All")
    rv <- review_share_by_area(input$cuisine)
    sup <- cuisine_by_centre |> filter(cuisine_group == input$cuisine)
    gap <- rv$reviews[match(d$planning_area_name, rv$planning_area_name)] -
      100 * ifelse(d$has_cuisine, coalesce(sup$share[match(d$centre_id, sup$centre_id)], 0), NA)
    quick_add(d, gap, decreasing = TRUE)
  })

  observeEvent(input$compare_remove, shortlist(setdiff(shortlist(), as.character(input$compare_remove))))
  observeEvent(input$compare_clear, shortlist(character(0)))
  # Rows dragged into a new order (sortable sends the data-id of each row)
  observeEvent(input$compare_order, {
    o <- as.character(input$compare_order)
    if (setequal(o, shortlist())) {
      shortlist(o)
      compare_sort(list(col = NULL, desc = FALSE))
    }
  })

  # The cuisine columns (stalls and engagement-supply gap) follow "Centres that sell" and only
  # appear once a cuisine is chosen
  cuisine_label <- reactive(paste(input$cuisine, "stalls"))
  cuisine_cell <- function(d) d$cuisine_stalls

  compare_data <- reactive({
    ids <- as.numeric(shortlist())
    if (!length(ids)) return(NULL)
    d <- centres[match(ids, centres$centre_id), ]
    d$cuisine_stalls <- NA_real_; d$supply_pct <- NA_real_; d$eng_pct <- NA_real_; d$gap_pts <- NA_real_
    if (input$cuisine != "All") {
      comp <- cuisine_by_centre |>
        filter(cuisine_group == input$cuisine) |>
        select(centre_id, cuisine_stalls = stalls, cuisine_share = share)
      d <- d |>
        select(-cuisine_stalls, -supply_pct) |>
        left_join(comp, by = "centre_id") |>
        mutate(cuisine_stalls = ifelse(has_cuisine, coalesce(cuisine_stalls, 0), NA),
               supply_pct = ifelse(has_cuisine, 100 * coalesce(cuisine_share, 0), NA))
      rv <- review_share_by_area(input$cuisine)   # NA where the area has too few classified places
      d$eng_pct <- rv$reviews[match(d$planning_area_name, rv$planning_area_name)]
      d$gap_pts <- d$eng_pct - d$supply_pct
    }
    # Open to you?: stalls advertised Oct 2025 - Sep 2026 that this cuisine may bid on
    oh <- stall_hint |>
      group_by(centre_id) |>
      summarise(open_total = n(), open_n = sum(allowed_trade_hint %in% open_hints(input$cuisine)), .groups = "drop")
    d <- left_join(d, oh, by = "centre_id")
    d$open_share <- d$open_n / d$open_total
    d
  })

  # What "Open to you?" is counting, for the column tooltip and the note under the table
  open_basis <- reactive(switch(input$cuisine,
    "All" = "any trade that is not reserved for Indian or halal food",
    "Indian / South Asian" = "Indian food (stalls reserved for Indian, or for halal or Indian, are included)",
    "Malay / Indonesian" = "Malay / Indonesian food, treated as halal (stalls reserved for halal, or for halal or Indian, are included)",
    paste0(input$cuisine, " food (stalls reserved for Indian or halal food are excluded)")))

  # Click a column header to sort (again = reverse); the sorted order becomes the
  # shortlist order, so dragging afterwards starts from it
  observeEvent(input$compare_sort, {
    d <- compare_data()
    col <- input$compare_sort$col
    if (is.null(d) || is.null(col)) return()
    s <- compare_sort()
    desc <- identical(s$col, col) && !s$desc
    key <- switch(col,
      centre = d$centre_name, rent = d$median_amr, mrt = d$nearest_exit_distance_m,
      residents = d$resident_population, cuisine = cuisine_cell(d), open = d$open_share,
      vacant = d$vacant_units, gap = d$gap_pts, listing = as.numeric(d$latest_month))
    shortlist(as.character(d$centre_id[order(key, decreasing = desc, na.last = TRUE)]))
    compare_sort(list(col = col, desc = desc))
  })

  output$compare_count <- renderText(plural(length(shortlist()), "centre"))

  output$compare_ui <- renderUI({
    d <- compare_data()
    if (is.null(d)) {
      # Empty state: how to add centres, plus three quick-start suggestions
      return(div(class = "compare-empty",
        div(class = "compare-empty-icon", icon("scale-balanced")),
        div(class = "compare-empty-title", "Your shortlist is empty"),
        p(class = "compare-empty-text",
          "Add centres to compare them side by side: click ", tags$b("Add to comparison"),
          " on the centre card, or double-click a centre on the map."),
        div(class = "compare-empty-quick",
            p(class = "small text-muted qs-label", paste0("Or start with ", QUICK_N, " centres that match your filters, picked by:")),
            div(class = "qs-grid",
              actionButton("quick_rent", "Lowest rent", icon = icon("tag"), class = "btn-sm btn-outline-success"),
              actionButton("quick_open", "Most stalls open to you", icon = icon("door-open"), class = "btn-sm btn-outline-success"),
              actionButton("quick_vacant", "Most vacant stalls", icon = icon("store"), class = "btn-sm btn-outline-success"),
              actionButton("quick_tender", "Best tender record", icon = icon("handshake"), class = "btn-sm btn-outline-success"),
              actionButton("quick_mrt", "Nearest to MRT", icon = icon("train-subway"), class = "btn-sm btn-outline-success"),
              actionButton("quick_busy", "Busiest station", icon = icon("people-arrows"), class = "btn-sm btn-outline-success"),
              actionButton("quick_pop", "Most residents", icon = icon("people-roof"), class = "btn-sm btn-outline-success"),
              if (input$cuisine != "All")
                actionButton("quick_gap", paste("Biggest", input$cuisine, "gap"), icon = icon("chart-line"),
                             class = "btn-sm btn-outline-success")))))
    }
    s <- compare_sort()
    th <- function(key, label, sub = NULL, hint = "Sort") {
      arrow <- if (identical(s$col, key)) if (isTRUE(s$desc)) " ▼" else " ▲" else ""
      tags$th(class = "sortable-col", title = hint,
              onclick = sprintf("Shiny.setInputValue('compare_sort', {col: '%s', nonce: Math.random()})", key),
              paste0(label, arrow), if (!is.null(sub)) tags$small(sub))
    }
    has_cu <- input$cuisine != "All"
    # Open to you?: a bar of the stalls this cuisine may bid on, then "x of y stalls"
    open_cell <- function(r) {
      if (is.na(r$open_share)) return(tags$td(class = "text-muted", "No listing"))
      cls <- if (r$open_share >= 2/3) "ok" else if (r$open_share >= 1/3) "some" else "none"
      tags$td(div(class = "open-cell",
                  title = sprintf("%d of %d stalls advertised Oct 2025 – Sep 2026 are open to %s",
                                  r$open_n, r$open_total, open_basis()),
                  span(class = "open-bar", span(class = paste("open-fill", cls),
                                                style = sprintf("width: %d%%;", round(100 * r$open_share)))),
                  span(class = "open-text", sprintf("%d of %d stalls", r$open_n, r$open_total))))
    }
    vacant_cell <- function(r) {
      if (is.na(r$observed_units)) return(tags$td(class = "text-muted", title = "No Hawker Guide observations", "no data"))
      tags$td(title = "Vacant units among those observed by Hawker Guide",
              span(class = paste("vac-dot", if (r$vacant_units > 0) "vac-yes")),
              sprintf("%d of %d", r$vacant_units, r$observed_units))
    }
    gap_cell <- function(r) {
      if (is.na(r$gap_pts))
        return(tags$td(class = "num text-muted",
                       title = if (!isTRUE(r$has_cuisine)) "No Hawker Guide stalls for this centre"
                               else "Fewer than 30 classified Google Maps places in this planning area", "n/a"))
      tags$td(class = paste("num", if (r$gap_pts > 0) "gap-pos" else "gap-neg"),
              title = sprintf("Reviews %.1f%% vs stalls %.1f%% (positive = more review attention than stalls)",
                              r$eng_pct, r$supply_pct),
              sprintf("%+.1f pts", r$gap_pts))
    }
    sel <- isolate(selected_centre())   # later selections only move the highlight (compare_selected below)
    rows <- lapply(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      tags$tr(
        `data-id` = r$centre_id,
        class = if (!is.null(sel) && r$centre_id == as.numeric(sel)) "is-selected",
        title = "Double-click to show this centre on the map",
        # Double-click: show the centre on the map and scroll up to it
        ondblclick = sprintf(paste0("Shiny.setInputValue('compare_focus', %s, {priority: 'event'});",
                                    "document.getElementById('map').scrollIntoView({behavior: 'smooth', block: 'center'});"),
                             r$centre_id),
        tags$td(class = "drag-handle", title = "Drag to reorder", "⋮⋮"),
        tags$td(div(class = "fw-semibold", r$centre_name), div(class = "small text-muted", r$pa_title)),
        tags$td(class = "num", fmt_sgd(r$median_amr)),
        open_cell(r),
        vacant_cell(r),
        tags$td(if (is.na(r$nearest_exit_distance_m)) "n/a" else
                  paste0(fmt_num(r$nearest_exit_distance_m), " m · ", r$nearest_mrt_station)),
        tags$td(class = "num", fmt_num(r$resident_population)),
        if (has_cu) tags$td(class = "num", if (is.na(r$cuisine_stalls)) "no data" else r$cuisine_stalls),
        if (has_cu) gap_cell(r),
        tags$td(if (is.na(r$latest_listing)) "No listing" else r$latest_listing),
        tags$td(tags$button(class = "btn-remove", title = "Remove", `aria-label` = "Remove",
                            onclick = sprintf("Shiny.setInputValue('compare_remove', '%s', {priority: 'event'})",
                                              r$centre_id),
                            "✕")))
    })
    tagList(
      div(class = "compare-scroll",
        tags$table(class = "table compare-table",
          tags$thead(tags$tr(
            tags$th(""), th("centre", "Centre"), th("rent", "Median rent"),
            th("open", "Open to you?", sub = if (has_cu) paste("as", input$cuisine) else "any trade not reserved",
               hint = paste("Stalls advertised Oct 2025 – Sep 2026 that you may bid on:", open_basis(), "- click to sort")),
            th("vacant", "Vacant stalls", sub = "of observed"),
            th("mrt", "MRT distance (station)"), th("residents", "Residents (subzone)"),
            if (has_cu) th("cuisine", cuisine_label()),
            if (has_cu) th("gap", "Engagement – supply", sub = "gap, points",
                           hint = "Share of Google Maps reviews in the planning area minus share of stalls at the centre - click to sort"),
            th("listing", "Latest listing"), tags$th(""))),
          tags$tbody(id = "compare_rows", rows))),
      sortable_js("compare_rows", options = sortable_options(
        handle = ".drag-handle", onSort = sortable_js_capture_input("compare_order"))))
  })

  # The map selection is highlighted in the comparison table by the page script, so picking a centre
  # does not rebuild the table
  observeEvent(selected_centre(), session$sendCustomMessage("compare_selected", list(id = sel_id())),
               ignoreInit = TRUE)

  # Everything the database holds about the given centres, one row each, for the CSV: the centre record
  # (address, coordinates, status, subzone, MRT access, residents), tender and vacancy figures, land use,
  # V8 station activity, V9 tender record, stalls per cuisine group and the review share of each cuisine
  # in the centre's planning area. Columns keep their database / app names.
  centre_full_data <- function(ids) {
    wide <- function(d, names_from, values_from, prefix) {
      d <- d |> mutate(key = paste0(prefix, gsub("[^A-Za-z0-9]+", "_", .data[[names_from]])))
      tidyr::pivot_wider(d[, c(setdiff(names(d), c(names_from, values_from, "key")), "key", values_from)],
                         names_from = key, values_from = all_of(values_from))
    }
    base <- raw$centres[match(ids, raw$centres$centre_id), ]
    base <- left_join(base, centres |> select(centre_id, n_listings, n_tender_stalls, median_amr, latest_listing,
                                              starts_with("residential_share"), starts_with("commercial_share")),
                      by = "centre_id")
    if (!is.null(raw$vacancy)) base <- left_join(base, raw$vacancy, by = "centre_id")
    base <- left_join(base, v9_status |> select(centre_id, tender_stalls_advertised = n_stalls,
                                                tender_stalls_assessed = n_assessed, tender_stalls_not_returned = taken_first,
                                                tender_not_returned_share = share_taken, tender_record_status = status),
                      by = "centre_id")
    base <- left_join(base, raw$mrt_access |> select(centre_id, station_avg_daily_tap_outs = avg_daily_tap_outs,
                                                     station_avg_daily_movements = avg_daily_movements,
                                                     mrt_match_status = match_status), by = "centre_id")
    stalls_w <- wide(cuisine_by_centre |> select(centre_id, cuisine_group, stalls), "cuisine_group", "stalls", "stalls_")
    base <- left_join(base, stalls_w, by = "centre_id")
    eng_w <- engagement |> select(planning_area_name, cuisine_group, engagement_pct, classified_establishments) |>
      wide("cuisine_group", "engagement_pct", "review_share_pct_")
    left_join(base, eng_w, by = "planning_area_name") |> as.data.frame()
  }

  # Legend under the comparison table: what the columns mean and how the table works. It follows the
  # cuisine in "Centres that sell", because that changes what "Open to you?" counts.
  output$compare_help <- renderUI({
    has_cu <- input$cuisine != "All"
    div(class = "compare-help",
      div(class = "compare-tips",
          span(tags$b("Double-click"), " a row to show the centre on the map"),
          span(tags$b("Drag ⋮⋮"), " to reorder"),
          span(tags$b("Click"), " a column header to sort"),
          span(tags$b("✕"), " removes a centre")),
      p(tags$b("Open to you?"),
        paste0(" counts the stalls advertised Oct 2025 – Sep 2026 that you may bid on: ", open_basis(),
               ". Other limits are in NEA's PDF notes.")),
      p(tags$b("Cuisine columns:"),
        if (has_cu) paste0(" the stalls of ", input$cuisine, " at each centre and its engagement – supply gap (share of Google Maps reviews minus share of stalls).")
        else " choose a cuisine in \"Centres that sell\" on the map and the table adds that cuisine's stalls and its engagement – supply gap."),
      p(tags$b("Export CSV"), " for the full data: besides these columns it holds everything we have on each centre (address, coordinates, status, subzone, MRT access, tender and vacancy figures, land use, station activity, stalls per cuisine and review shares)."))
  })

  output$compare_csv <- downloadHandler(
    filename = function() paste0("hawker_shortlist_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- compare_data()
      out <- if (is.null(d)) data.frame(Centre = character(0)) else data.frame(
        Centre = d$centre_name, `Planning area` = d$pa_title,
        `Median rent (S$)` = d$median_amr,
        `Open to you (stalls)` = ifelse(is.na(d$open_total), NA, paste0(d$open_n, " of ", d$open_total)),
        `Vacant stalls` = ifelse(is.na(d$observed_units), NA, paste0(d$vacant_units, " of ", d$observed_units)),
        `MRT distance (m)` = round(d$nearest_exit_distance_m), `Nearest MRT station` = d$nearest_mrt_station,
        `Residents (subzone)` = d$resident_population,
        `Cuisine stalls` = d$cuisine_stalls, `Engagement-supply gap (pts)` = round(d$gap_pts, 1),
        `Latest listing` = d$latest_listing, check.names = FALSE)
      if (input$cuisine == "All") out <- out[, !names(out) %in% c("Cuisine stalls", "Engagement-supply gap (pts)")]
      else names(out)[names(out) == "Cuisine stalls"] <- cuisine_label()
      # then every other column held about each centre (see centre_full_data), after the ones shown
      if (!is.null(d)) {
        full <- centre_full_data(d$centre_id)
        out <- cbind(out, full[, setdiff(names(full), names(out)), drop = FALSE])
      }
      write.csv(out, file, row.names = FALSE, na = "")
    }
  )

  # ===========================================================================
  # Map search box
  # ===========================================================================

  # Map search box ("cg_centre"): picking a centre from the search results
  # zooms the map to it and opens its centre card.
  observeEvent(input$cg_centre, {
    id <- suppressWarnings(as.numeric(input$cg_centre))
    if (is.na(id)) return()
    c <- centres[centres$centre_id == id, ]
    if (nrow(c) && !is.na(c$latitude)) leafletProxy("map") |> flyTo(c$longitude, c$latitude, zoom = 15)
    selected_centre(id)
  }, ignoreInit = TRUE)

  # ===========================================================================
  # Data visualization: V3, V4, V6, V7 (Ridhwan), V8 (Lucien), V9 (Casper), drawn live from
  # the data prepared in global.R; the original scripts are in charts/.
  # ===========================================================================
  # V3: the search box above the chart picks the centre; a centre picked on the map also lands here
  v3_id <- reactiveVal(V3_DEFAULT_ID)
  # A centre picked on the map that has no cuisine supply data: V3 says so instead of
  # leaving the previous centre on screen (NA = the chart shows v3_id())
  v3_miss <- reactiveVal(NA_real_)
  # V3's own search box: show that centre, and make it the map selection too
  observeEvent(input$v3_centre, {
    id <- suppressWarnings(as.numeric(input$v3_centre))
    if (!is.na(id) && id %in% V3_IDS) { v3_id(id); v3_miss(NA_real_) }
    pick_on_map(id)
  }, ignoreInit = TRUE)
  v3_shown <- reactive(if (is.na(v3_miss())) v3_id() else v3_miss())
  v3_name <- reactive(sub(" \\(.*$", "", centres$centre_name[centres$centre_id == v3_shown()][1]))
  v3_data <- reactive(v3_supply_for(v3_id()))
  v3_reason <- reactive(if (is.na(v3_miss())) NULL else v3_missing_reason(centres[centres$centre_id == v3_miss(), ]))

  output$v3_headline <- renderUI(paste("Cuisine supply at", v3_name()))
  output$v3_lead <- renderUI({
    if (!is.null(v3_reason())) return(v3_reason()$lead)
    d <- v3_data()
    sprintf("%s accounts for %d of %d classified occupied main-meal stalls (%s) at %s, making it the most represented cuisine.",
            d$cuisine_group[1], d$stall_count[1], sum(d$stall_count),
            percent(d$supply_share[1], accuracy = 0.1), v3_name())
  })
  output$v3_body <- renderUI({
    if (!is.null(v3_reason())) return(v3_reason()$body)
    d <- v3_data()
    nxt <- if (d$stall_count[2] > 0)
      sprintf("%s follows with %d stalls (%s), while the remaining cuisines have a smaller presence. ",
              d$cuisine_group[2], d$stall_count[2], percent(d$supply_share[2], accuracy = 0.1))
    else "No other cuisine has an occupied stall here. "
    # Own paragraph under the bold lead (Ridhwan's V3 note)
    span(class = "viz-para viz-para-gap",
         paste0(nxt, "This suggests that prospective hawkers entering the ", d$cuisine_group[1],
                " segment would face more existing direct competition, whereas less-represented cuisines have fewer competing stalls. ",
                "Figures reflect observed classified occupied main-meal stalls, not an official census."))
  })

  output$v3_plot <- renderPlotly({
    if (!is.null(v3_reason())) return(message_plot(v3_reason()$chart, "No cuisine supply to show"))
    d <- v3_data() |> filter(stall_count > 0)   # cuisines with no occupied stalls are not listed
    d$cuisine_group <- factor(d$cuisine_group, levels = rev(d$cuisine_group))
    d$label <- paste0(d$stall_count, " (", percent(d$supply_share, accuracy = 0.1), ")")
    d$fill <- ifelse(seq_len(nrow(d)) == 1, BRAND$green_dark, "#BAC4CA")
    plot_ly(d, x = ~stall_count, y = ~cuisine_group, type = "bar", orientation = "h",
            marker = list(color = ~fill), text = ~label, textposition = "outside", cliponaxis = FALSE,
            textfont = list(color = "#404041"), hoverinfo = "text",
            hovertext = ~paste0(cuisine_group, "<br>", label)) |>
      mck_plotly(margin = list(l = 10, r = 40, t = 10, b = 10)) |>
      layout(xaxis = list(visible = FALSE, range = c(0, max(d$stall_count) * 1.3)),
             yaxis = list(automargin = TRUE, ticks = "outside", ticklen = 10, tickcolor = "rgba(0,0,0,0)"), bargap = 0.45)
  })

  # Ring and name on the map selection in a plotly scatter (V7, V8); d needs centre_id. The ring is always
  # the chart's last trace (with no points when the selection is not on the chart), so a new selection
  # only swaps that trace (swap_sel_ring) instead of redrawing the chart
  # The selected centre is an orange dot (the app's orange) with a dark outline, as in Ridhwan's V7.R; `size`
  # gives its diameter from the row (a bubble chart passes the bubble's own size)
  sel_ring <- function(d, x, y, id, label = TRUE, size = function(sd) rep(16, nrow(sd))) {
    sd <- d[d$centre_id %in% id, ]
    list(x = sd[[x]], y = sd[[y]], text = if (label) sub(" \\(.*$", "", sd$centre_name) else rep("", nrow(sd)),
         type = "scatter", mode = "markers+text", textposition = "top center",
         textfont = list(color = "#1E2B22", size = 12), hoverinfo = "skip",
         marker = list(size = size(sd), color = BRAND$orange, opacity = 1, line = list(color = "#1E2B22", width = 2)))
  }
  add_sel_ring <- function(p, ...) do.call(add_trace, c(list(p), sel_ring(...)))
  swap_sel_ring <- function(output_id, ...) {
    r <- sel_ring(...)
    r[c("x", "y", "text")] <- lapply(r[c("x", "y", "text")], as.list)   # JSON arrays, even for one point
    r$marker$size <- as.list(r$marker$size)
    plotlyProxy(output_id, session) |>
      plotlyProxyInvoke("deleteTraces", -1) |>
      plotlyProxyInvoke("addTraces", r)
  }

  # V4: planning areas shaded by the share of Google Maps reviews for the chosen cuisine
  output$v4_map <- renderLeaflet({
    req(engagement)
    cu <- input$v4_cuisine                 # follows the map filter, then V4's own box; "All" = leading cuisine
    r <- review_share_by_area(cu)
    a <- planning_areas |> left_join(r, by = "planning_area_name")
    nc <- a$planning_area_name %in% NO_CENTRE_AREAS      # no hawker centre: excluded from the comparison
    sc <- review_scale(a$reviews[!nc])                   # five equal bins that follow the cuisine shown
    five <- REVIEW_COLS
    pal <- colorBin(five, domain = a$reviews, bins = sc$bins, right = FALSE, na.color = FEW_PLACES_FILL)
    fills <- ifelse(nc, NO_CENTRE_FILL, pal(a$reviews))
    # "Geylang - Chinese: 4.3% engagement share, 18 classified places, 2,797 reviews"
    lab <- lapply(sprintf("<b>%s</b>%s<br>%s", a$pa_title,
                          ifelse(is.na(a$reviews), "", paste0(" - ", a$lead_cuisine)),
                          ifelse(nc, NO_CENTRE_TIP,
                          ifelse(is.na(a$reviews), FEW_PLACES_TIP,
                                 sprintf("%.1f%% engagement share<br>%s food establishments · %s reviews", a$reviews,
                                         comma(a$restaurant_count), comma(a$total_reviews))))), htmltools::HTML)
    base_map() |>
      fitBounds(103.64, 1.235, 104.03, 1.47) |>
      addPolygons(data = a, layerId = ~planning_area_name, fillColor = fills, fillOpacity = 0.9,
                  color = "#FFFFFF", weight = 1, label = lab, highlightOptions = highlightOptions(weight = 2, color = "#404041")) |>
      addLegend("bottomright", colors = c(FEW_PLACES_FILL, NO_CENTRE_FILL), opacity = 1,
                labels = c("Too few food establishments", "No hawker centres (excluded)")) |>
      addLegend("bottomright", colors = five, labels = sc$labels, opacity = 1,
                title = if (identical(cu, "All")) "Leading cuisine's<br>engagement share (%)"
                        else "Cuisine engagement<br>share (%)") |>
      add_v4_selection(a, isolate(sel_id()))   # later selections are redrawn on their own (below)
  })
  # Note under the V4 map: which areas are excluded and how they are shown
  output$v4_note <- renderUI(
    p(class = "compare-note",
      tags$b("Excluded areas:"),
      sprintf(" Planning areas without hawker centres are shown in light grey. Areas with fewer than %d food establishments whose cuisines could be identified are shown in darker grey.",
              V47_MIN_ESTABLISHMENTS)))

  # Choosing a cuisine in the map filter "Centres that sell" moves V4 and V7 to it ("Any cuisine"
  # leaves V7 as it is, because its scatter needs one cuisine); their own boxes can then differ
  observeEvent(input$cuisine, {
    updateSelectInput(session, "v4_cuisine", selected = input$cuisine)
    if (input$cuisine != "All") updateSelectInput(session, "v7_cuisine", selected = input$cuisine)
  }, ignoreInit = TRUE)

  # V4 text, worked out for the cuisine shown (areas that contain hawker centres only)
  v4_stats <- reactive({
    r <- review_share_by_area(input$v4_cuisine)
    r <- r[r$planning_area_name %in% centres$planning_area_name, ]
    r[order(-r$reviews), ]
  })
  pa_nice <- function(x) tools::toTitleCase(tolower(x))
  output$v4_headline <- renderUI(
    if (input$v4_cuisine == "All") "Which cuisine draws the most online engagement, area by area?"
    else paste0("Where does ", input$v4_cuisine, " attract the most online engagement?"))
  # No bold lead line for V4 (it repeated the story); the story is in separate paragraphs below
  output$v4_lead <- renderUI({
    if (!nrow(v4_stats())) "No area has enough food establishments to compare." else NULL
  })
  output$v4_body <- renderUI({
    r <- v4_stats(); if (!nrow(r)) return(NULL)
    para <- function(...) span(class = "viz-para", ...)
    note <- para(tags$b("Note:"), " Results are based on restaurants whose cuisine could be identified (approximately half of the dataset). Areas with insufficient data are excluded. Review shares indicate online engagement, not actual purchases.")
    if (input$v4_cuisine == "All") {
      tb <- sort(table(r$lead_cuisine), decreasing = TRUE)
      others <- if (length(tb) < 2) "" else {
        rest <- sprintf("%s cuisine leads in %d", names(tb)[-1], as.integer(tb[-1]))
        rest[1] <- paste0(rest[1], if (tb[[2]] == 1) " area" else " areas")
        paste0(" ", if (length(rest) == 1) rest else paste0(paste(head(rest, -1), collapse = ", "), ", while ", tail(rest, 1)), ".")
      }
      tagList(
        para(tags$strong(sprintf("%s cuisine accounts for the largest share of Google Maps restaurant reviews in %d of the %d planning areas with hawker centres.",
                     names(tb)[1], tb[[1]], nrow(r))), others,
             sprintf(" Across these areas, the leading cuisine's engagement share ranges from %.0f%% to %.0f%%, with a median of %.0f%%, highlighting differences in online engagement across planning areas.",
                     min(r$reviews), max(r$reviews), median(r$reviews))),
        para("Each area is shaded according to its leading cuisine's engagement share. Hover over an area to identify the leading cuisine and its percentage."),
        note)
    } else {
      top <- head(pa_nice(r$planning_area_name), 3)
      top <- if (length(top) < 2) top else paste(paste(head(top, -1), collapse = ", "), "and", tail(top, 1))
      cu <- input$v4_cuisine
      tagList(
        para(sprintf("%s cuisine records its highest Google Maps review engagement in %s, accounting for %.1f%% of reviews from food establishments whose cuisines could be identified.",
                     cu, pa_nice(r$planning_area_name[1]), r$reviews[1]),
             sprintf(" Across the %d planning areas with hawker centres, %s's engagement share ranges from %.1f%% to %.1f%%, with a median of %.1f%%, highlighting differences in online engagement across planning areas.",
                     nrow(r), cu, min(r$reviews), max(r$reviews), median(r$reviews))),
        para(sprintf("%s cuisine shows relatively stronger engagement in %s.", cu, top)),
        note)
    }
  })
  # The map selection on V4: its planning area outlined, the centre as a dot. They sit in their own
  # group, so a new selection redraws just them and V4 is not rebuilt.
  add_v4_selection <- function(m, a, id) {
    sc <- centres[centres$centre_id %in% id, ]
    if (!nrow(sc) || is.na(sc$latitude[1])) return(m)
    m |>
      addPolygons(data = a[a$planning_area_name %in% sc$planning_area_name, ], group = "v4_sel", fill = FALSE,
                  color = "#1E2B22", weight = 3, opacity = 1) |>
      addCircleMarkers(lng = sc$longitude, lat = sc$latitude, group = "v4_sel", radius = 7, color = "#FFFFFF", weight = 2,
                       fillColor = BRAND$orange, fillOpacity = 1, label = sc$centre_name)
  }
  v4_ready <- map_ready("v4_map")
  observeEvent(sel_id(), {
    req(v4_ready())
    leafletProxy("v4_map") |> clearGroup("v4_sel") |> add_v4_selection(planning_areas, sel_id())
  }, ignoreInit = TRUE)

  # V7 text for the cuisine shown (same rule as the green dots in the chart)
  v7_d <- reactive({
    d <- centre_gap |> filter(cuisine_group == input$v7_cuisine)
    d$opp <- d$gap_pts > 0 & d$median_amr <= median(d$median_amr)
    d
  })
  # Title, lead and paragraphs follow Ridhwan's V7 note (V7 paragraph.docx) and V7.R
  output$v7_headline <- renderUI(paste("Cuisine Engagement-Supply Gap vs Rent:", input$v7_cuisine))
  output$v7_lead <- renderUI({
    d <- v7_d(); if (!nrow(d)) return("No centres have rent and cuisine data for this cuisine.")
    sprintf("For %s, %d out of %d hawker centres have relatively stronger online engagement compared to existing cuisine supply, while also having below-median rental costs.",
            input$v7_cuisine, sum(d$opp), nrow(d))
  })
  output$v7_body <- renderUI({
    d <- v7_d(); if (!nrow(d)) return("")
    sel <- d[d$centre_id %in% sel_id(), ]
    para <- function(...) span(class = "viz-para", ...)
    # Say which of the two criteria is met, not just "both" or "not both": a positive gap, and a rent below the median
    pick <- if (nrow(sel)) {
      med <- median(d$median_amr)
      gap_ok <- sel$gap_pts[1] > 0; rent_ok <- sel$median_amr[1] <= med
      # Wording from Ridhwan's V7 note; a gap of exactly 0 is "non-positive", not "negative"
      neg <- if (sel$gap_pts[1] < 0) "a negative" else "a non-positive"
      verdict <- if (gap_ok && rent_ok) sprintf("It meets both criteria, with a positive engagement–supply gap and an assessed market rent at or below the median of S$%s.", comma(round(med)))
        else if (gap_ok) sprintf("It meets only the engagement criterion, with a positive engagement–supply gap and an assessed market rent above the median of S$%s.", comma(round(med)))
        else if (rent_ok) sprintf("It meets only the rent criterion, with %s engagement–supply gap and an assessed market rent at or below the median of S$%s.", neg, comma(round(med)))
        else sprintf("It meets neither criterion, with %s engagement–supply gap and an assessed market rent above the median of S$%s.", neg, comma(round(med)))
      paste(sprintf("%s has an engagement\u2013supply gap of %+.1f percentage points and an assessed market rent of S$%s.",
                    sel$short_name[1], sel$gap_pts[1], comma(round(sel$median_amr[1]))), verdict)
    }
    else "Select a hawker centre on the map to see how its cuisine engagement compares with existing stall supply and rental costs."
    tagList(
      para("These centres may be worth exploring because the selected cuisine attracts a higher share of online engagement in the surrounding planning area than its share of existing hawker stalls. They also have relatively lower assessed market rents. However, higher online engagement does not necessarily indicate unmet customer demand."),
      para(pick))
  })

  # V7's fixed note (the green dots carry no names: Ridhwan found them messy)
  V7_OPP_NOTE <- list(x = 0.99, y = 0.01, xref = "paper", yref = "paper", showarrow = FALSE,
                      xanchor = "right", yanchor = "bottom", text = "<b>POSITIVE GAP + BELOW-MEDIAN RENT</b>",
                      font = list(color = BRAND$green_dark, size = 12))

  # When the centre picked on the map has no dot in V7, say why on the chart itself
  v7_missing_note <- function(d, id) {
    if (!length(id) || is.na(id) || id %in% d$centre_id) return(list())
    cen <- centres[centres$centre_id == id, ]
    if (!nrow(cen)) return(list())
    why <- if (is.na(cen$median_amr)) "it has no tender listing, so no rent to plot"
           else if (!isTRUE(cen$has_cuisine)) "Hawker Guide has no stalls for it, so no supply share"
           else "its planning area has too few classified Google Maps places"
    list(list(x = 0.5, y = 0.97, xref = "paper", yref = "paper", showarrow = FALSE,
              xanchor = "center", yanchor = "top", bgcolor = "rgba(255,255,255,0.92)", borderpad = 6,
              text = paste0("<b>", sub(" \\(.*$", "", cen$centre_name), "</b> is not on this chart: ", why, "."),
              font = list(size = 12, color = "#58595B")))
  }

  # V7: engagement-supply gap against median rent, one dot per centre
  output$v7_plot <- renderPlotly({
    req(centre_gap)
    d <- v7_d()
    if (!nrow(d)) return(empty_plot("No centres for this cuisine"))
    med <- median(d$median_amr)
    d$hov <- sprintf("<b>%s</b><br>Planning area: %s<br><br>Cuisine engagement share: %.1f%%<br>Cuisine supply share: %.1f%%<br>Engagement-supply gap: %+.1f pp<br><br>Median assessed market rent: S$%s",
                     d$short_name, tools::toTitleCase(tolower(d$planning_area_name)),
                     d$engagement_pct, d$supply_pct, d$gap_pts, comma(round(d$median_amr)))
    sel <- isolate(sel_id())   # later selections only swap the ring and the note (below)
    xr <- range(c(0, d$gap_pts)) + c(-2, 12)
    plot_ly(source = "v7") |>
      add_markers(data = filter(d, !opp), x = ~gap_pts, y = ~median_amr, text = ~hov, hoverinfo = "text",
                  customdata = ~centre_id, marker = list(color = "#C7D0D4", size = 9, opacity = 0.75)) |>
      add_trace(data = filter(d, opp), x = ~gap_pts, y = ~median_amr, type = "scatter", mode = "markers",
                text = ~hov, hoverinfo = "text", customdata = ~centre_id,
                marker = list(color = "#087A3E", size = 10, opacity = 0.85)) |>
      mck_plotly(margin = list(l = 10, r = 10, t = 10, b = 10)) |>
      layout(xaxis = list(title = "Cuisine engagement-supply gap (percentage points)", range = xr,
                          ticksuffix = " pp", showline = FALSE, ticks = "", zeroline = FALSE,
                          showgrid = TRUE, gridcolor = "#E6EAEC", gridwidth = 1),
             yaxis = list(title = "Median assessed market rent (S$)", tickprefix = "S$", tickformat = ",",
                          dtick = 500, automargin = TRUE, zeroline = FALSE,
                          showgrid = TRUE, gridcolor = "#E6EAEC", gridwidth = 1),
             shapes = list(
               list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper",
                    line = list(color = "#A0AAB0", dash = "dash", width = 1)),
               list(type = "line", y0 = med, y1 = med, x0 = 0, x1 = 1, xref = "paper",
                    line = list(color = "#A0AAB0", dash = "dash", width = 1))),
             annotations = c(list(V7_OPP_NOTE), v7_missing_note(d, sel))) |>
      add_sel_ring(d, "gap_pts", "median_amr", sel, label = FALSE) |>   # the Map selection chip names it
      event_register("plotly_click")
  })
  observeEvent(sel_id(), {
    d <- v7_d()
    if (!nrow(d)) return()
    id <- sel_id()
    swap_sel_ring("v7_plot", d, "gap_pts", "median_amr", id, label = FALSE) |>
      plotlyProxyInvoke("relayout", list(annotations = c(list(V7_OPP_NOTE), v7_missing_note(d, id))))
  }, ignoreInit = TRUE)

  # V6: the search box above the chart picks the centre (and the map selection); a centre picked
  # on the map also lands here
  v6_id <- reactiveVal(V6_DEFAULT_ID)
  observeEvent(input$v6_centre, {
    id <- suppressWarnings(as.numeric(input$v6_centre))
    if (!is.na(id) && id %in% V6_IDS) { v6_id(id); v6_miss(NA_real_) }
    pick_on_map(id)
  }, ignoreInit = TRUE)
  # A centre picked on the map that V6 cannot show: V6 says why (NA = the chart shows v6_id())
  v6_miss <- reactiveVal(NA_real_)
  v6_shown <- reactive(if (is.na(v6_miss())) v6_id() else v6_miss())
  v6_name <- reactive(sub(" \\(.*$", "", centres$centre_name[centres$centre_id == v6_shown()][1]))
  v6_area <- reactive(centres$pa_title[centres$centre_id == v6_id()][1])
  v6_data <- reactive(v6_gap_for(v6_id()))
  v6_reason <- reactive({
    if (is.na(v6_miss())) return(NULL)
    cen <- centres[centres$centre_id == v6_miss(), ]
    if (!isTRUE(cen$has_cuisine)) return(v3_missing_reason(cen))
    list(chart = "Too few Google Maps places",
         lead = paste0(cen$pa_title, " has fewer than ", V47_MIN_ESTABLISHMENTS,
                       " classified Google Maps places, so its review shares are not compared."),
         body = "With so few places, one restaurant can swing a cuisine's share, so the gap would not be reliable.")
  })

  # Titles as in Ridhwan's V6.R: "Cuisine Engagement-Supply Gap at <centre>" and
  # "<PLANNING AREA>'s cuisine engagement share vs <centre>'s cuisine supply share"
  output$v6_headline <- renderUI(paste("Cuisine Engagement-Supply Gap at", v6_name()))
  output$v6_subtitle <- renderUI({
    area <- toupper(centres$planning_area_name[centres$centre_id == v6_shown()][1])
    paste0(area, "'s cuisine engagement share vs ", v6_name(), "'s cuisine supply share")
  })
  output$v6_lead <- renderUI({
    if (!is.null(v6_reason())) return(v6_reason()$lead)
    d <- v6_data(); top <- d[which.max(d$gap_pts), ]
    if (top$gap_pts <= 0) return(sprintf("No cuisine records a positive engagement–supply gap at %s.", v6_name()))
    sprintf("%s records the largest positive engagement–supply gap at %s (+%.1f percentage points).",
            top$cuisine_group, v6_name(), top$gap_pts)
  })
  output$v6_body <- renderUI({
    if (!is.null(v6_reason())) return(v6_reason()$body)
    d <- v6_data(); top <- d[which.max(d$gap_pts), ]; low <- d[which.min(d$gap_pts), ]
    pos <- if (top$gap_pts > 0)
      paste0("This indicates that its share of cuisine engagement in ", v6_area(),
             " exceeds its share of occupied stalls at the selected hawker centre. ") else ""
    neg <- if (low$gap_pts < 0)
      sprintf("Conversely, %s records the largest negative gap (−%.1f percentage points), indicating a greater presence in existing hawker supply relative to online engagement. ",
              low$cuisine_group, abs(low$gap_pts)) else ""
    paste0(pos, neg, "These differences help prospective hawkers identify cuisines worth investigating further, ",
           "although they do not necessarily indicate unmet consumer demand.")
  })
  output$v6_plot <- renderPlotly({
    if (!is.null(v6_reason())) return(message_plot(v6_reason()$chart, "No engagement–supply gap to show"))
    d <- v6_data()
    d$cuisine_group <- factor(d$cuisine_group, levels = d$cuisine_group)
    d$col <- ifelse(d$gap_pts >= 0, BRAND$green_dark, BRAND$orange)
    d$label <- ifelse(abs(d$gap_pts) < 0.05, "≈0 pp", sprintf("%+.1f pp", d$gap_pts))
    lim <- max(abs(d$gap_pts)) * 1.35   # symmetric around zero, as in V6.R
    plot_ly(d, x = ~gap_pts, y = ~cuisine_group, type = "bar", orientation = "h",
            marker = list(color = ~col), text = ~label, textposition = "outside", cliponaxis = FALSE,
            textfont = list(color = "#404041"), hoverinfo = "text",
            hovertext = ~sprintf("%s<br>Reviews %.1f%% vs stalls %.1f%%<br>Gap %+.1f pts",
                                 cuisine_group, engagement_pct, supply_pct, gap_pts)) |>
      mck_plotly(margin = list(l = 10, r = 30, t = 28, b = 30)) |>
      layout(xaxis = list(visible = TRUE, range = c(-lim, lim), zeroline = FALSE, showgrid = TRUE, gridcolor = "#E6EAEC", gridwidth = 1, showline = FALSE,
                          ticksuffix = " pp", nticks = 7, tickfont = list(size = 11, color = "#58595B")),
             yaxis = list(automargin = TRUE, ticks = "outside", ticklen = 10, tickcolor = "rgba(0,0,0,0)"),
             bargap = 0.45,
             shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = "#58595B", width = 1))),
             annotations = list(   # direction labels, centred over each half of the axis as in V6.R
               list(x = 0.55 * lim, y = 1.02, xref = "x", yref = "paper", xanchor = "center", yanchor = "bottom",
                    showarrow = FALSE, text = "Higher cuisine engagement share", font = list(size = 12, color = BRAND$green_dark)),
               list(x = -0.55 * lim, y = 1.02, xref = "x", yref = "paper", xanchor = "center", yanchor = "bottom",
                    showarrow = FALSE, text = "Higher cuisine supply share", font = list(size = 12, color = "#C27C0E"))))
  })

  # ---------------------------------------------------------------------------
  # Map selection -> search boxes and charts. Picking a centre (map click, search box, table
  # row, a chart, or reset) sets the map search box, moves V3 and V6 to it (or shows why they
  # cannot), and highlights it on V4, V7, V8 and V9.
  # ---------------------------------------------------------------------------
  observeEvent(selected_centre(), {
    id <- suppressWarnings(as.numeric(selected_centre()))
    if (!length(id) || is.na(id)) return()
    name <- centres$centre_name[centres$centre_id == id][1]
    if (is.na(name)) return()
    session$sendCustomMessage("set_search_centre", list(id = id, name = name))
    if (id %in% V3_IDS) { v3_id(id); v3_miss(NA_real_) } else v3_miss(id)
    if (id %in% V6_IDS) { v6_id(id); v6_miss(NA_real_) } else v6_miss(id)
    session$sendCustomMessage("set_v3_search", list(name = name))
    session$sendCustomMessage("set_v6_search", list(name = name))
  })
  sel_id <- reactive(suppressWarnings(as.numeric(selected_centre())))

  # Chip in a chart's toolbar naming the map selection (greyed when the chart cannot show it)
  sel_chip <- function(in_chart, why) {
    renderUI({
      id <- sel_id()
      if (!length(id) || is.na(id)) return(NULL)
      nm <- sub(" \\(.*$", "", centres$centre_name[centres$centre_id == id][1])
      if (in_chart(id)) span(class = "viz-chip", span(class = "viz-chip-dot"), "Map selection:", tags$b(nm))
      else span(class = "viz-chip viz-chip-off", "Map selection:", tags$b(nm), paste0("(", why, ")"))
    })
  }
  output$v4_sel <- sel_chip(function(id) !is.na(centres$planning_area_name[centres$centre_id == id][1]),
                            "no planning area")
  output$v7_sel <- sel_chip(function(id) id %in% centre_gap$centre_id[centre_gap$cuisine_group == input$v7_cuisine],
                            "no rent or cuisine data")
  output$v8_sel <- sel_chip(function(id) !is.null(v8) && id %in% v8$centre_id,
                            "no population data")

  # V9: tender performance of the centre picked on the map (Casper; wording from "Explanition for
  # Tender performance graph.docx"). The share is the median-based gauge: stalls that never
  # returned to tender after their first advertisement. Centres that cannot be assessed get a
  # grey message, never a 0%: 0% is a real result (every stall came back).
  v9_row <- reactive({
    id <- sel_id()
    if (!length(id) || is.na(id)) id <- DEFAULT_CENTRE
    v9_status[v9_status$centre_id == id, ]
  })
  v9_name <- reactive(sub(" \\(.*$", "", centres$centre_name[centres$centre_id == v9_row()$centre_id][1]))
  v9_counts <- v9_status |> count(status) |> (\(x) setNames(x$n, x$status))()
  v9_unassessed <- sprintf("Of the %d hawker centres examined, %d could not be reliably assessed due to insufficient eligible stall observations.",
                           nrow(v9_status), sum(v9_counts[c("none", "few", "late")], na.rm = TRUE))
  v9_why <- function(r) {
    switch(r$status,
      none = "Not enough tender activity to assess — this centre advertised no stalls in the past year.",
      few  = sprintf("Not enough tender activity to assess — only %d stall%s advertised, too few for a reliable rate.",
                     r$n_stalls, if (r$n_stalls == 1) " was" else "s were"),
      late = sprintf("Not enough time to assess — its stalls were advertised in the last %d months, too recently to see whether anyone took them.",
                     V9_SETTLE_MONTHS))
  }
  # Wording from Casper's "V9 paragraph.docx"; the comparison with the median and the band follow the data
  v9_band <- function(s) if (s < V9_Q1) "a lower non-return rate than most centres"
                         else if (s >= V9_Q3) "a higher non-return rate than most centres"
                         else "a typical non-return rate relative to other centres"
  V9_SMALL_SAMPLE <- 5   # at or below this many assessed stalls, the story adds a small-sample caution

  output$v9_headline <- renderUI(paste("Stall Tender Non-Return Rate at", v9_name()))
  output$v9_lead <- renderUI({
    r <- v9_row()
    if (r$status != "ok") return(v9_why(r))
    vs <- if (r$share_taken > V9_MEDIAN) "above" else if (r$share_taken < V9_MEDIAN) "below" else "matching"
    sprintf("%d of %d assessed stalls (%s) at %s were not re-advertised, %s the median rate across assessed hawker centres.",
            r$taken_first, r$stalls_assessed, percent(r$share_taken, accuracy = 1), v9_name(), vs)
  })
  output$v9_body <- renderUI({
    r <- v9_row()
    para <- function(...) span(class = "viz-para", ...)
    if (r$status != "ok")
      return(tagList(span(class = "viz-para viz-para-gap", v9_unassessed),
                     para("Non-return is a proxy for stall take-up, not confirmation of a successful award.")))
    tagList(
      span(class = "viz-para viz-para-gap", sprintf("This suggests %s. A higher non-return rate may indicate stronger stall take-up but does not confirm successful tender awards.",
                   v9_band(r$share_taken))),
      para(v9_unassessed),
      if (r$stalls_assessed <= V9_SMALL_SAMPLE)
        div(class = "viz-caution", icon("triangle-exclamation"),
            span(tags$b("Small sample:"), sprintf(" only %d assessed stalls, so this comparison is less reliable.", r$stalls_assessed))))
  })
  output$v9_sel <- sel_chip(function(id) id %in% v9_all$centre_id, "too little tender activity to assess")

  # Note under the gauge: which stalls and centres are left out, with the months worked out from the data
  output$v9_note <- renderUI({
    last <- max(tenders$exercise_month)
    first_out <- seq(last, by = "-1 month", length.out = V9_SETTLE_MONTHS)[V9_SETTLE_MONTHS]
    mon <- function(d) paste(month.name[as.integer(format(d, "%m"))], format(d, "%Y"))
    div(class = "ex-note",
      p(style = "margin: 0;", tags$b("Methodology:"),
        sprintf(" A stall is counted as non-returned if it was advertised once and did not reappear in subsequent tenders. Stalls first advertised from %s to %s are excluded to allow sufficient follow-up time. Centres require at least %s eligible stalls for assessment.",
                month.name[as.integer(format(first_out, "%m"))], mon(last),
                c("one", "two", "three", "four", "five")[V9_MIN_STALLS])),
      p(style = "margin: 0.5em 0 0;",
        "Thresholds are based on the 25th and 75th percentiles of non-return rates across assessed hawker centres."))
  })

  output$v9_plot <- renderPlotly({
    r <- v9_row()
    if (r$status != "ok") return(message_plot("Not assessed", v9_why(r)))
    # the gauge (and its number, drawn at the base of the arc) sits above the colour key, never on it
    plot_ly(type = "indicator", mode = "gauge+number", value = round(100 * r$share_taken, 1),
            domain = list(x = c(0, 1), y = c(0.16, 1)),
            number = list(suffix = "%", font = list(size = 44, color = "#1E2B22")),
            title = list(text = sprintf("%d of %d assessed stalls were not re-advertised", r$taken_first, r$stalls_assessed),
                         font = list(size = 14, color = "#58595B")),
            gauge = list(axis = list(range = c(0, 100), ticksuffix = "%", tickcolor = "#C9D0D4"),
                         bar = list(color = BRAND$green_dark, thickness = 0.28), bgcolor = "white", borderwidth = 0,
                         steps = list(list(range = c(0, 100 * V9_Q1), color = "#F6D9A5"),
                                      list(range = c(100 * V9_Q1, 100 * V9_Q3), color = "#E3E8EB"),
                                      list(range = c(100 * V9_Q3, 100), color = "#BBD980")),
                         threshold = list(line = list(color = "#404041", width = 3), thickness = 0.9,
                                          value = 100 * V9_MEDIAN))) |>
      mck_plotly(margin = list(l = 30, r = 30, t = 60, b = 20)) |>
      layout(annotations = list(list(x = 0.5, y = 0, xref = "paper", yref = "paper", yanchor = "bottom", showarrow = FALSE,
                                     text = sprintf("Below %d%%: Lower non-return rate · %d–%d%%: Typical range · %d%%+: Higher non-return rate",
                                                    round(100 * V9_Q1), round(100 * V9_Q1), round(100 * V9_Q3), round(100 * V9_Q3)),
                                     font = list(size = 11, color = "#58595B"))))
  })

  # V8 bubbles: area proportional to the subzone's residents, the biggest drawn 34 px across; the selected
  # centre's orange dot is the same size as its bubble (a little larger so the grey or green shows around it)
  V8_REF <- 2 * max(v8$subzone_population_2026) / (34^2)
  v8_sel_size <- function(sd) pmax(sqrt(2 * sd$subzone_population_2026 / V8_REF) + 6, 14)

  output$v8_plot <- renderPlotly({
    req(v8)
    d <- v8
    mid_x <- median(d$straight_line_distance_m)
    mid_y <- median(d$avg_daily_tap_outs)
    d$zone <- ifelse(d$straight_line_distance_m <= mid_x & d$avg_daily_tap_outs >= mid_y, "close", "other")
    d$hov <- sprintf("%s<br>%s %s · %s m to nearest exit<br>%s daily tap-outs · %s subzone residents",
                     d$centre_name, d$station, ifelse(grepl("LRT", d$nearest_station_name), "LRT", "MRT"),
                     comma(round(d$straight_line_distance_m)),
                     comma(round(d$avg_daily_tap_outs)), comma(d$subzone_population_2026))
    ref <- V8_REF   # bubble area scale (sizemode = "area")
    bubbles <- function(x, col, alpha) list(color = col, opacity = alpha, line = list(width = 0),
                                            size = x$subzone_population_2026, sizemode = "area",
                                            sizeref = ref, sizemin = 4)
    far <- subset(d, zone == "other"); near <- subset(d, zone == "close")
    xmax <- 1600
    plot_ly(source = "v8") |>
      add_markers(data = far, x = ~straight_line_distance_m, y = ~avg_daily_tap_outs, customdata = ~centre_id,
                  marker = bubbles(far, "#BAC4CA", 0.55), text = ~hov, hoverinfo = "text") |>
      add_markers(data = near, x = ~straight_line_distance_m, y = ~avg_daily_tap_outs, customdata = ~centre_id,
                  marker = bubbles(near, BRAND$green, 0.8), text = ~hov, hoverinfo = "text") |>
      mck_plotly(margin = list(l = 10, r = 10, t = 10, b = 10)) |>
      layout(xaxis = list(title = "Distance to nearest MRT/LRT exit (m)", range = c(0, xmax),
                          tickformat = ",", showline = TRUE, linecolor = "#C9D0D4", ticks = "outside"),
             yaxis = list(title = "Average daily station tap-outs", tickformat = "~s", automargin = TRUE),
             shapes = list(
               # stops below the CLOSE + HIGH STATION ACTIVITY label so it never runs through the text
               list(type = "line", x0 = mid_x, x1 = mid_x, y0 = 0, y1 = 0.9, yref = "paper",
                    line = list(color = "#8B96A5", dash = "dash", width = 1)),
               list(type = "line", y0 = mid_y, y1 = mid_y, x0 = 0, x1 = 1, xref = "paper",
                    line = list(color = "#8B96A5", dash = "dash", width = 1))),
             annotations = list(list(x = 0.01, y = 0.99, xref = "paper", yref = "paper", showarrow = FALSE,
                                     xanchor = "left", yanchor = "top", text = "<b>CLOSE + HIGH STATION ACTIVITY</b>",
                                     font = list(color = BRAND$green_dark, size = 13)))) |>
      add_sel_ring(d, "straight_line_distance_m", "avg_daily_tap_outs", isolate(sel_id()), label = FALSE, size = v8_sel_size) |>
      event_register("plotly_click")
  })
  # no name on the orange dot: the "Map selection" chip above the chart already names it
  observeEvent(sel_id(), swap_sel_ring("v8_plot", v8, "straight_line_distance_m", "avg_daily_tap_outs", sel_id(),
                                       label = FALSE, size = v8_sel_size),
               ignoreInit = TRUE)

  # Facts about the centre picked on the map that V8 is about: exit and station, station activity,
  # subzone residents, how each compares with the median centre, and where it sits in the chart
  output$v8_detail <- renderUI({
    req(v8)
    id <- sel_id(); if (!length(id) || is.na(id)) id <- DEFAULT_CENTRE
    cen <- centres[centres$centre_id == id, ]
    if (!nrow(cen)) return(NULL)
    nm <- sub(" \\(.*$", "", cen$centre_name)
    # one table row: measure | this centre (with a small note) | the median centre
    fact <- function(label, value, sub = NULL, median = NULL)
      tags$tr(tags$th(scope = "row", label),
              tags$td(class = "v", value, if (!is.null(sub)) tags$small(sub)),
              if (!is.null(median)) tags$td(class = "m", median))
    fact_table <- function(..., with_median = TRUE)
      tags$table(class = "viz-table",
                 tags$thead(tags$tr(tags$th("Measure"), tags$th("This centre"), if (with_median) tags$th("Median centre"))),
                 tags$tbody(...))
    r <- v8[v8$centre_id == id, ]
    if (!nrow(r)) {
      return(div(class = "viz-detail",
        div(class = "viz-detail-title", nm, span(class = "viz-tag viz-tag-off", "Excluded from this chart")),
        fact_table(with_median = FALSE,
          if (!is.na(cen$nearest_exit_distance_m))
            fact("Nearest exit", paste0(fmt_num(cen$nearest_exit_distance_m), " m"), cen$nearest_mrt_station),
          fact("Subzone population (2026)", if (is.na(cen$resident_population)) "N/A" else fmt_num(cen$resident_population),
               if (is.na(cen$resident_population)) "no published figure" else NULL)),
        p(class = "viz-detail-note",
          "This centre is excluded from the chart due to unavailable subzone population data.")))
    }
    med_d <- median(v8$straight_line_distance_m); med_t <- median(v8$avg_daily_tap_outs)
    med_p <- median(v8$subzone_population_2026)
    near <- r$straight_line_distance_m <= med_d; busy <- r$avg_daily_tap_outs >= med_t
    zone <- if (near && busy) "Close + high station activity" else if (near) "Close + low station activity"
            else if (busy) "Far + high station activity" else "Far + low station activity"
    ordinal <- function(n) paste0(n, if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
    pct_busier <- round(100 * mean(v8$avg_daily_tap_outs < r$avg_daily_tap_outs))
    pct_pop <- round(100 * mean(v8$subzone_population_2026 < r$subzone_population_2026))
    is_lrt <- grepl("LRT", r$nearest_station_name)
    div(class = "viz-detail",
      div(class = "viz-detail-title", nm, span(class = paste("viz-tag", if (!(near && busy)) "viz-tag-off"), zone)),
      fact_table(
        fact("Nearest exit", paste0(fmt_num(r$straight_line_distance_m), " m"),
             paste0(r$station, if (is_lrt) " LRT" else " MRT"), paste0(fmt_num(med_d), " m")),
        fact("Station activity", paste0(fmt_num(r$avg_daily_tap_outs), " daily tap-outs"),
             sprintf("%s percentile among centres", ordinal(pct_busier)), paste0(fmt_num(med_t), " daily tap-outs")),
        fact("Subzone population (2026)", fmt_num(r$subzone_population_2026),
             sprintf("%s · higher than %d%% of centres", tools::toTitleCase(tolower(cen$subzone_name)), pct_pop),
             fmt_num(med_p)),
        if (!is.na(cen$exits_within_500m))
          fact("MRT/LRT exits within 500 m", paste0(plural(cen$exits_within_500m, "exit"), ", ", plural(cen$stations_within_500m, "station")),
               NULL, "")),
      if (is_lrt)
        p(class = "viz-detail-note", "The nearest station is on the LRT, which carries far fewer riders than MRT stations, so it can never show high station activity."),
      if (!identical(cen$nea_status, "Existing"))
        p(class = "viz-detail-note", paste0("Status: ", cen$nea_status, ".")),
      p(class = "viz-detail-note", "Distances are measured in straight lines. Station tap-outs do not represent hawker-centre visitors. \"Close + high station activity\" indicates a shorter-than-median distance to the nearest MRT/LRT exit and above-median daily station tap-outs."))
  })

  # Lucien's V8 note: the lead, and what the two dotted lines mean (medians from the data)
  output$v8_lead <- renderUI({
    req(v8)
    near <- v8$straight_line_distance_m <= median(v8$straight_line_distance_m) &
      v8$avg_daily_tap_outs >= median(v8$avg_daily_tap_outs)
    sprintf("%d of %d hawker centres (%s) are both closer than the median MRT/LRT exit distance and associated with stations recording above-median daily tap-outs.",
            sum(near), nrow(v8), percent(mean(near), accuracy = 1))
  })
  output$v8_lines <- renderUI({
    req(v8)
    div(class = "ex-note",
        tags$b("Vertical line:"), sprintf(" Median distance to MRT/LRT exit: %s m", fmt_num(median(v8$straight_line_distance_m))),
        tags$br(),
        tags$b("Horizontal line:"), sprintf(" Median daily station tap-outs: %s", fmt_num(median(v8$avg_daily_tap_outs))))
  })

  # ===========================================================================
  # Charts -> map (reverse sync) and the "Add to comparison" button on every chart
  # ===========================================================================
  # A centre picked inside a chart (its search box, or a click on a dot or bubble) becomes the
  # map selection: the card opens, the map flies there and every other chart follows.
  pick_on_map <- function(id) {
    id <- suppressWarnings(as.numeric(id))
    if (!length(id) || is.na(id) || identical(id, sel_id())) return()
    c <- centres[centres$centre_id == id, ]
    if (nrow(c) && !is.na(c$latitude) && isTRUE(t1_map_ready()))
      leafletProxy("map") |> flyTo(c$longitude, c$latitude, zoom = 15)
    selected_centre(id)
  }
  # observeEvent (not observe): pick_on_map() reads the selection, so a plain observe would re-run on every
  # later map click or Reset and put the old chart click back (event_data keeps the last click)
  observeEvent(event_data("plotly_click", source = "v7"), {
    e <- event_data("plotly_click", source = "v7")
    if (!is.null(e$customdata)) pick_on_map(e$customdata[[1]])
  })
  observeEvent(event_data("plotly_click", source = "v8"), {
    e <- event_data("plotly_click", source = "v8")
    if (!is.null(e$customdata)) pick_on_map(e$customdata[[1]])
  })

  # A cuisine picked in V4 or V7 becomes the map filter "Centres that sell". The list there only
  # holds cuisines sold in the chosen area, so the picked one is added to it when missing.
  set_map_cuisine <- function(v) {
    if (is.null(v) || identical(v, input$cuisine)) return()
    b <- area_centres(input$region, input$pa)
    cu <- intersect(CUISINE_GROUPS,
                    cuisine_by_centre$cuisine_group[cuisine_by_centre$centre_id %in% b$centre_id])
    if (v != "All") cu <- intersect(CUISINE_GROUPS, union(cu, v))
    updateSelectInput(session, "cuisine", choices = cuisine_choices(cu), selected = v)
  }
  observeEvent(input$v4_cuisine, set_map_cuisine(input$v4_cuisine), ignoreInit = TRUE)
  observeEvent(input$v7_cuisine, set_map_cuisine(input$v7_cuisine), ignoreInit = TRUE)

  # Clicking a planning area on V4 sets the map's region and planning area filters
  observeEvent(input$v4_map_shape_click, {
    pa <- input$v4_map_shape_click$id
    if (is.null(pa)) return()
    if (!pa %in% PAS) {
      showNotification(paste0("No hawker centres in ", tools::toTitleCase(tolower(pa)), "."), type = "warning", duration = notif_secs(4))
      return()
    }
    r <- unique(centres$region_name[centres$planning_area_name == pa])[1]
    pas <- sort(unique(centres$planning_area_name[centres$region_name == r]))
    updateSelectInput(session, "region", selected = r)
    updateSelectInput(session, "pa", choices = c("All planning areas" = "All", named_choices(pas)), selected = pa)
  })

  # One "Add to comparison" button per chart and on the centre card: adds that centre to the comparison table below.
  # Once the centre is on the shortlist it reads "In comparison"; hovering shows "Remove", and a click takes it off.
  add_button <- function(input_id, id_fn) {
    renderUI({
      id <- suppressWarnings(as.numeric(id_fn()))
      if (!length(id) || is.na(id)) return(NULL)
      nm <- sub(" \\(.*$", "", name_of(id))
      if (as.character(id) %in% shortlist())
        actionButton(input_id, tagList(span(class = "va-in", icon("check"), "In comparison"),
                                       span(class = "va-rm", icon("xmark"), "Remove")),
                     class = "viz-add viz-add-done", title = paste0("Remove ", nm, " from the comparison table"))
      else
        actionButton(input_id, tagList(icon("plus"), "Add to comparison"), class = "viz-add",
                     title = paste0("Add ", nm, " to the comparison table"))
    })
  }
  output$cc_add <- add_button("add_compare", sel_id)   # the centre card's button (observer above)
  output$v3_add <- add_button("add_v3", v3_shown)
  output$v4_add <- add_button("add_v4", sel_id)
  output$v6_add <- add_button("add_v6", v6_id)
  output$v7_add <- add_button("add_v7", sel_id)
  output$v8_add <- add_button("add_v8", sel_id)
  output$v9_add <- add_button("add_v9", sel_id)
  observeEvent(input$add_v3, toggle_compare(v3_shown()))
  observeEvent(input$add_v4, toggle_compare(sel_id()))
  observeEvent(input$add_v6, toggle_compare(v6_id()))
  observeEvent(input$add_v7, toggle_compare(sel_id()))
  observeEvent(input$add_v8, toggle_compare(sel_id()))
  observeEvent(input$add_v9, toggle_compare(sel_id()))

  # ===========================================================================
  # About us: star rating and subscribe. Nothing is stored or sent anywhere yet
  # (no database or mailing service is connected); the visitor just gets a reply.
  # ===========================================================================
  observeEvent(input$rating, {
    showNotification(sprintf("Thanks for rating HawkerRadar %d out of 5.", as.integer(input$rating)),
                     type = "message", duration = notif_secs(4))
  })

  observeEvent(input$sub_submit, {
    email <- trimws(if (is.null(input$sub_email)) "" else input$sub_email)
    if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) {
      showNotification("Please enter a valid email address.", type = "warning", duration = notif_secs(4))
      return()
    }
    showNotification("Thanks for subscribing! (Preview: email addresses are not stored yet.)",
                     type = "message", duration = notif_secs(6))
    updateTextInput(session, "sub_email", value = "")
  })
}
