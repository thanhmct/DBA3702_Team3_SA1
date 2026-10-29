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

  # Keep the other filters consistent with the area: only cuisines sold there,
  # and rent / MRT sliders that span what exists there.
  observeEvent(list(input$region, input$pa), {
    b <- area_centres(input$region, input$pa)
    cu <- intersect(CUISINE_GROUPS,
                    cuisine_by_centre$cuisine_group[cuisine_by_centre$centre_id %in% b$centre_id])
    cur <- isolate(input$cuisine)
    updateSelectInput(session, "cuisine", choices = c("Any cuisine" = "All", cu),
                      selected = if (cur %in% cu) cur else "All")
    rent <- b$median_amr[!is.na(b$median_amr)]
    r <- if (length(rent)) c(floor(min(rent) / 50) * 50, ceiling(max(rent) / 50) * 50) else AMR_RANGE
    if (r[1] == r[2]) r[2] <- r[1] + 50
    updateSliderInput(session, "max_amr", min = r[1], max = r[2], value = r)
    mrt <- b$nearest_exit_distance_m[!is.na(b$nearest_exit_distance_m)]
    m <- if (length(mrt)) ceiling(max(mrt) / 100) * 100 else MRT_MAX
    updateSliderInput(session, "max_mrt", min = 0, max = m, value = c(0, m))
    vmax <- max(c(1, b$vacant_units), na.rm = TRUE)
    updateSliderInput(session, "min_vacant", min = 0, max = vmax, value = 0)
  }, ignoreInit = TRUE)

  observeEvent(input$reset_t1, {
    updateSelectInput(session, "region", selected = "All")
    updateSelectInput(session, "pa", choices = c("All planning areas" = "All", named_choices(PAS)),
                      selected = "All")
    updateSelectInput(session, "cuisine", choices = c("Any cuisine" = "All", CUISINE_GROUPS),
                      selected = "All")
    updateSliderInput(session, "max_amr", min = AMR_RANGE[1], max = AMR_RANGE[2], value = AMR_RANGE)
    updateSliderInput(session, "max_mrt", min = 0, max = MRT_MAX, value = c(0, MRT_MAX))
    updateCheckboxInput(session, "only_tenders", value = FALSE)
    updateCheckboxInput(session, "only_vacant", value = FALSE)
    updateSliderInput(session, "min_vacant", min = 0, max = VAC_MAX, value = 0)
    updateSelectInput(session, "map_metric", selected = "n")
    updateCheckboxInput(session, "map_exits", value = FALSE)
    selected_centre(DEFAULT_CENTRE)  # the map search box follows; no zoom on reset
  })

  # Centres that pass the map filters
  fc <- reactive({
    d <- area_centres(input$region, input$pa)
    if (input$cuisine != "All") {
      ids <- cuisine_by_centre$centre_id[cuisine_by_centre$cuisine_group == input$cuisine]
      d <- filter(d, centre_id %in% ids)
    }
    if (length(input$max_amr) == 2) {
      if (input$max_amr[1] > AMR_RANGE[1]) {
        d <- filter(d, !is.na(median_amr) & median_amr >= input$max_amr[1] & median_amr <= input$max_amr[2])
      } else {
        d <- filter(d, is.na(median_amr) | (median_amr >= input$max_amr[1] & median_amr <= input$max_amr[2]))
      }
    } else if (length(input$max_amr) == 1) {
      d <- filter(d, is.na(median_amr) | median_amr <= input$max_amr)
    }

    if (length(input$max_mrt) == 2) {
      if (input$max_mrt[1] > 0) {
        d <- filter(d, !is.na(nearest_exit_distance_m) & nearest_exit_distance_m >= input$max_mrt[1] & nearest_exit_distance_m <= input$max_mrt[2])
      } else {
        d <- filter(d, is.na(nearest_exit_distance_m) | (nearest_exit_distance_m >= input$max_mrt[1] & nearest_exit_distance_m <= input$max_mrt[2]))
      }
    } else if (length(input$max_mrt) == 1) {
      d <- filter(d, is.na(nearest_exit_distance_m) | nearest_exit_distance_m <= input$max_mrt)
    }
    if (isTRUE(input$only_tenders)) d <- filter(d, n_listings > 0)
    # Only centres where Hawker Guide recorded at least one vacant unit (no data counts as none)
    if (isTRUE(input$only_vacant)) d <- filter(d, coalesce(vacant_units, 0) > 0)
    # V2 constraint: vacant stalls (Hawker Guide); centres without data count as 0
    d <- filter(d, coalesce(vacant_units, 0) >= input$min_vacant)
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
      r <- review_share_by_area(input$cuisine)   # "Any cuisine" = each area's leading cuisine
      i <- match(a$planning_area_name, r$planning_area_name)
      a$reviews <- r$reviews[i]
      a$lead_cuisine <- r$lead_cuisine[i]
    }
    a
  })

  output$map <- renderLeaflet(base_map(pad_right = 270))  # the page script refits Singapore to the real map size

  # Bins, colours and labels for the planning-area shading: one definition used
  # by the map and by the legend in the info panel beside it
  map_scale <- reactive({
    five <- BRAND_SEQ5
    if (identical(input$map_metric, "reviews") && identical(input$cuisine, "All")) {
      # darker = the leading cuisine takes a bigger share of the area's reviews
      return(list(bins = REVIEW_SCALE$lead$bins, cols = five, labels = REVIEW_SCALE$lead$labels,
                  title = "Leading cuisine's share<br>of Google Maps reviews"))
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
      reviews = list(bins = REVIEW_SCALE$one$bins, cols = five, labels = REVIEW_SCALE$one$labels,
                     title = paste0(input$cuisine, "<br>share of Google Maps reviews")))
  })
  lead_mode <- reactive(identical(input$map_metric, "reviews") && identical(input$cuisine, "All"))

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
    if (input$cuisine == "All")
      return(ifelse(is.na(v), "too few classified Google Maps places",
                    paste0("of reviews go to ", area_stats()$lead_cuisine, ", the leading cuisine")))
    ifelse(is.na(v), "too few classified Google Maps places",
           paste0("of classified Google Maps reviews · ", input$cuisine))
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
        div(class = "legend-row", span(class = "legend-swatch", style = "background:#E3E8EB;"),
            "Too few classified places"),
      div(class = "legend-row", span(class = "legend-dot"), "Hawker centre (size = cooked-food stalls)"),
      if (isTRUE(input$map_exits))
        div(class = "legend-row", mrt_icon_img(),
            paste0("MRT/LRT exit (", comma(nrow(mrt_exits)), ")"))
    )
  })

  t1_map_ready <- map_ready("map")

  # Zoom to the chosen planning area or region whenever those filters change;
  # "All" (or Reset) zooms back out to Singapore. Padding keeps the area clear
  # of the search box (top left) and the floating centre card (top right).
  observeEvent(list(input$region, input$pa), {
    req(t1_map_ready())
    a <- planning_areas
    pa_ok <- input$pa != "All" &&
      (input$region == "All" || input$pa %in% a$planning_area_name[a$region_name %in% input$region])
    sel <- if (pa_ok) a[a$planning_area_name == input$pa, ]
           else if (input$region != "All") a[a$region_name %in% input$region, ]
           else NULL
    all_sg <- is.null(sel) || !nrow(sel)
    # a chosen area keeps clear of the search box and the centre card; "All" is the default view
    pad <- if (all_sg) list(paddingTopLeft = c(4, 4), paddingBottomRight = c(270, 4))   # clear of the centre card
           else list(paddingTopLeft = c(20, 60), paddingBottomRight = c(265, 20))
    bb <- if (all_sg) c(103.62, 1.235, 104.03, 1.47) else as.numeric(sf::st_bbox(sel))
    leafletProxy("map") |> flyToBounds(bb[1], bb[2], bb[3], bb[4], options = pad)
  }, ignoreInit = TRUE)

  observe({
    req(t1_map_ready())
    a <- area_stats()
    d <- fc()
    metric <- input$map_metric
    vals <- a[[metric]]
    sc <- map_scale()
    pal <- colorBin(if (is.null(sc$pal_cols)) sc$cols else sc$pal_cols, domain = vals, bins = sc$bins,
                    right = FALSE, na.color = "#E3E8EB")
    fills <- pal(vals)
    # McKinsey-style tooltips: name, the shaded metric as a big serif number, details
    big <- fmt_metric(metric, vals)
    area_labels <- sprintf(
      "<div class='tt-title'>%s</div><div class='tt-big'>%s</div><div class='tt-sub'>%s</div><div class='tt-meta'>%d centres · %s stalls · %s residents</div>",
      a$pa_title, big, metric_unit(metric, vals),
      a$n, comma(a$stalls), comma(a$residents)) |>
      lapply(htmltools::HTML)
    # Centre tooltip (V1+V2): cuisine mix, vacant stalls and rent of the centre
    centre_labels <- sprintf(
      "<div class='tt-title'>%s</div><div class='tt-big'>%s</div><div class='tt-sub'>cooked-food stalls · %s</div><div class='tt-meta'>%s<br>Vacant stalls: %s of %s observed · %d tender listings%s</div>",
      d$centre_name, fmt_num(d$stalls), d$pa_title,
      ifelse(is.na(d$top3), "No cuisine data", d$top3),
      fmt_num(d$vacant_units), fmt_num(d$observed_units), d$n_listings,
      ifelse(is.na(d$median_amr), "", paste0("<br>Median rent ", fmt_sgd(d$median_amr)))) |>
      lapply(htmltools::HTML)
    tt <- labelOptions(className = "mck-tt", direction = "right", offset = c(18, 0))

    p <- leafletProxy("map") |>
      clearShapes() |> clearMarkers() |> clearControls() |>
      add_sg_border() |>
      addPolygons(data = a, fillColor = fills, fillOpacity = 0.75,
                  color = "#FFFFFF", weight = 1, label = area_labels, labelOptions = tt,
                  highlightOptions = highlightOptions(weight = 2.5, color = "#1E2B22",
                                                      fillOpacity = 0.95, bringToFront = FALSE))
    if (nrow(d)) {
      p <- p |>
        addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude,
                         layerId = ~centre_id,
                         radius = ~3 + sqrt(coalesce(stalls, 0)) / 2,
                         color = "#FFFFFF", weight = 1.5,
                         fillColor = "#FAA619", fillOpacity = 0.9,
                         label = centre_labels, labelOptions = tt)
    }
    # MRT/LRT exits (merged from the old Location tab)
    if (isTRUE(input$map_exits)) {
      leafletProxy("map") |>
        addMarkers(data = mrt_exits, lng = ~longitude, lat = ~latitude, group = "exits",
                   icon = mrt_marker_icon, label = ~station_name,
                   options = markerOptions(riseOnHover = TRUE))
    }
    draw_selected(isolate(selected_centre()))  # redraw the highlight after clearMarkers()
  })

  # Selected centre: drawn on top in dark green with a pulsing ring (group
  # "selected"). It ignores the mouse, so hover and clicks reach the circle below.
  draw_selected <- function(id) {
    p <- leafletProxy("map") |> clearGroup("selected")
    if (is.null(id)) return(invisible())
    c <- centres[centres$centre_id == as.numeric(id), ]
    if (!nrow(c) || is.na(c$latitude)) return(invisible())
    r <- 3 + sqrt(coalesce(c$stalls, 0)) / 2
    p |>
      addCircleMarkers(lng = c$longitude, lat = c$latitude, group = "selected", radius = r + 8,
                       color = BRAND$green_dark, weight = 2.5, fill = FALSE,
                       options = pathOptions(interactive = FALSE, className = "sel-ring")) |>
      addCircleMarkers(lng = c$longitude, lat = c$latitude, group = "selected", radius = r + 1.5,
                       color = "#FFFFFF", weight = 2, fillColor = BRAND$green_dark, fillOpacity = 1,
                       options = pathOptions(interactive = FALSE, className = "sel-dot"))
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

  # Click = show the centre card (and pick the centre for the charts).
  # Two clicks on the same centre within 0.6 s = double-click = add it to the
  # comparison table (the map's own double-click zoom is off, see base_map()).
  last_click <- reactiveVal(list(id = NULL, t = 0))
  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    if (is.null(id)) return()  # MRT/LRT exit dots have no id
    selected_centre(id)
    c <- centres[centres$centre_id == id, ]
    if (nrow(c) && !is.na(c$latitude)) {
      leafletProxy("map") |> flyTo(c$longitude, c$latitude, zoom = 15)
    }
    now <- as.numeric(Sys.time())
    lc <- last_click()
    if (identical(lc$id, id) && now - lc$t < 0.6) {
      add_to_compare(id)
      last_click(list(id = NULL, t = 0))
    } else {
      last_click(list(id = id, t = now))
    }
  })

  # Lets the UI show the cuisine chart and Compare button only once a centre is chosen
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
    if (!is.null(id)) add_to_compare(id)
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
    datatable(centre_table_data(), rownames = FALSE, selection = "single", class = "compact hover",
              options = list(dom = "frtip", paging = TRUE, pageLength = 10,
                             columnDefs = list(list(visible = FALSE, targets = 0))))
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
    if (length(i)) selected_centre(centre_table_data()$centre_id[i])
  })

  # ===========================================================================
  # Comparison table (V11): the investor's shortlist, any number of centres
  # ===========================================================================
  shortlist <- reactiveVal(character(0))                 # centre ids, in display order
  compare_sort <- reactiveVal(list(col = NULL, desc = FALSE))

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
    showNotification(tagList(msg, go_link), type = "message", duration = 6)
  }

  # Quick start for an empty shortlist: 3 matching centres by one criterion
  quick_add <- function(d, key, decreasing = FALSE) {
    ids <- head(d$centre_id[order(key, decreasing = decreasing, na.last = NA)], 3)
    if (length(ids)) add_to_compare(ids)
    else showNotification("No centres match these filters.", type = "warning")
  }
  observeEvent(input$quick_rent, { d <- fc(); quick_add(d, d$median_amr) })
  observeEvent(input$quick_vacant, {
    d <- filter(fc(), coalesce(vacant_units, 0) > 0)
    quick_add(d, d$vacant_units, decreasing = TRUE)
  })
  observeEvent(input$quick_mrt, { d <- fc(); quick_add(d, d$nearest_exit_distance_m) })

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
          "Add centres to compare them side by side: click ", tags$b("Compare"),
          " on the centre card, or double-click a centre on the map."),
        div(class = "compare-empty-quick",
            span(class = "small text-muted", "Or start with 3 centres that match your filters:"),
            actionButton("quick_rent", "Lowest rent", icon = icon("tag"), class = "btn-sm btn-outline-success"),
            actionButton("quick_vacant", "Most vacant stalls", icon = icon("store"), class = "btn-sm btn-outline-success"),
            actionButton("quick_mrt", "Nearest to MRT", icon = icon("train-subway"), class = "btn-sm btn-outline-success"))))
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
    sel <- selected_centre()
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
      p(class = "compare-note",
        tags$b("Open to you?"),
        paste0(" counts the stalls advertised Oct 2025 – Sep 2026 that you may bid on: ", open_basis(),
               ". Other limits are in NEA's PDF notes. ",
               if (!has_cu) "Choose a cuisine in \"Centres that sell\" on the map to add its stalls and the engagement – supply gap." else "")),
      sortable_js("compare_rows", options = sortable_options(
        handle = ".drag-handle", onSort = sortable_js_capture_input("compare_order"))))
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
    paste0(nxt, "This suggests that prospective hawkers entering the ", d$cuisine_group[1],
           " segment would face more existing direct competition, whereas less-represented cuisines have fewer competing stalls. ",
           "Figures reflect observed classified occupied main-meal stalls, not an official census.")
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

  # Ring and name on the map selection in a plotly scatter (V7, V8); d needs centre_id
  add_sel_ring <- function(p, d, x, y) {
    sd <- d[d$centre_id %in% sel_id(), ]
    if (!nrow(sd)) return(p)
    p |>
      add_trace(x = sd[[x]], y = sd[[y]], type = "scatter", mode = "markers+text",
                text = sub(" \\(.*$", "", sd$centre_name), textposition = "top center",
                textfont = list(color = "#1E2B22", size = 12), hoverinfo = "skip",
                marker = list(size = 22, color = "rgba(0,0,0,0)", line = list(color = "#1E2B22", width = 2.5)))
  }

  # V4: planning areas shaded by the share of Google Maps reviews for the chosen cuisine
  output$v4_map <- renderLeaflet({
    req(engagement)
    cu <- input$v4_cuisine                 # follows the map filter, then V4's own box; "All" = leading cuisine
    r <- review_share_by_area(cu)
    a <- planning_areas |> left_join(r, by = "planning_area_name")
    sc <- REVIEW_SCALE[[if (identical(cu, "All")) "lead" else "one"]]
    five <- BRAND_SEQ5
    pal <- colorBin(five, domain = a$reviews, bins = sc$bins, right = FALSE, na.color = "#E3E8EB")
    # "Geylang - Chinese: 4.3% engagement share, 18 classified places, 2,797 reviews"
    lab <- lapply(sprintf("<b>%s</b>%s<br>%s", a$pa_title,
                          ifelse(is.na(a$reviews), "", paste0(" - ", a$lead_cuisine)),
                          ifelse(is.na(a$reviews), "Too few classified places to compare",
                                 sprintf("%.1f%% engagement share<br>%s classified places · %s reviews", a$reviews,
                                         comma(a$restaurant_count), comma(a$total_reviews)))), htmltools::HTML)
    base_map() |>
      fitBounds(103.64, 1.235, 104.03, 1.47) |>
      addPolygons(data = a, layerId = ~planning_area_name, fillColor = ~pal(reviews), fillOpacity = 0.9,
                  color = "#FFFFFF", weight = 1, label = lab, highlightOptions = highlightOptions(weight = 2, color = "#404041")) |>
      addLegend("bottomright", colors = five, labels = sc$labels, opacity = 1,
                title = if (identical(cu, "All")) "Leading cuisine's<br>engagement share (%)"
                        else "Cuisine engagement<br>share (%)") |>
      addLegend("bottomright", colors = "#E3E8EB", labels = "Too few classified places", opacity = 1) |>
      add_v4_selection(a)
  })
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
  output$v4_lead <- renderUI({
    r <- v4_stats(); if (!nrow(r)) return("No area has enough classified places to compare.")
    if (input$v4_cuisine == "All") {
      tb <- sort(table(r$lead_cuisine), decreasing = TRUE)
      sprintf("%s leads Google Maps review share in %d of the %d areas with hawker centres.", names(tb)[1], tb[[1]], nrow(r))
    } else
      sprintf("%s draws its largest share of reviews in %s (%.1f%%).", input$v4_cuisine, pa_nice(r$planning_area_name[1]), r$reviews[1])
  })
  output$v4_body <- renderUI({
    r <- v4_stats(); if (!nrow(r)) return("")
    note <- " Areas with fewer than 30 classified places are excluded, only about half of the places could be classified, and review share shows online attention, not actual cuisine purchases."
    if (input$v4_cuisine == "All") {
      tb <- sort(table(r$lead_cuisine), decreasing = TRUE)
      paste0("Each planning area is shaded by the share of reviews that go to its leading cuisine, which ranges from ",
             sprintf("%.0f%% to %.0f%%", min(r$reviews), max(r$reviews)), " (median ", sprintf("%.0f%%", median(r$reviews)),
             "). ", paste0(names(tb), " leads in ", tb, collapse = "; "), ". Hover an area to see its leading cuisine.", note)
    } else {
      top <- paste(pa_nice(head(r$planning_area_name, 3)), collapse = ", ")
      paste0(input$v4_cuisine, " records its highest Google Maps review engagement in ", pa_nice(r$planning_area_name[1]),
             ", accounting for ", sprintf("%.1f%%", r$reviews[1]), " of classified restaurant reviews. Across the ", nrow(r),
             " planning areas containing hawker centres, engagement ranges from ", sprintf("%.1f%%", min(r$reviews)), " to ",
             sprintf("%.1f%%", max(r$reviews)), ", with a median of ", sprintf("%.1f%%", median(r$reviews)), ". ",
             input$v4_cuisine, " shows relatively stronger engagement in ", top,
             ", suggesting geographical differences in recorded online interest.", note)
    }
  })
  # The map selection on V4: its planning area outlined, the centre as a dot
  add_v4_selection <- function(m, a) {
    sc <- centres[centres$centre_id %in% sel_id(), ]
    if (!nrow(sc) || is.na(sc$latitude[1])) return(m)
    m |>
      addPolygons(data = a[a$planning_area_name %in% sc$planning_area_name, ], fill = FALSE,
                  color = "#1E2B22", weight = 3, opacity = 1) |>
      addCircleMarkers(lng = sc$longitude, lat = sc$latitude, radius = 7, color = "#FFFFFF", weight = 2,
                       fillColor = BRAND$orange, fillOpacity = 1, label = sc$centre_name)
  }

  # V7 text for the cuisine shown (same rule as the green dots in the chart)
  v7_d <- reactive({
    d <- centre_gap |> filter(cuisine_group == input$v7_cuisine)
    d$opp <- d$gap_pts > 0 & d$median_amr <= median(d$median_amr)
    d
  })
  output$v7_lead <- renderUI({
    d <- v7_d(); if (!nrow(d)) return("No centres have rent and cuisine data for this cuisine.")
    sprintf("For %s, %d of %d centres pair an opening with a below-median rent.", input$v7_cuisine, sum(d$opp), nrow(d))
  })
  output$v7_body <- renderUI({
    d <- v7_d(); if (!nrow(d)) return("")
    sel <- d[d$centre_id %in% sel_id(), ]
    pick <- if (nrow(sel))
      sprintf(" %s has an engagement–supply gap of %+.1f points and a median assessed market rent of S$%s, so it falls %s this group.",
              sel$short_name[1], sel$gap_pts[1], comma(round(sel$median_amr[1])), if (sel$opp[1]) "within" else "outside")
    else " Select a hawker centre on the map to see its gap and rent, and whether it meets both screening criteria."
    paste0("Each dot is a centre with tender listings: across, the cuisine's share of reviews in its planning area minus its share of stalls at the centre; up, its median assessed market rent. Green dots are under-supplied and below the median rent (S$",
           comma(round(median(d$median_amr))), " for ", input$v7_cuisine, "). ", input$v7_cuisine, " is ",
           sprintf("%.0f%%", median(d$supply_pct)), " of stalls at the median centre and absent from ", sum(d$stalls == 0),
           " centres.", pick, " The gap is an exploratory signal, not unmet demand, and the rent is NEA's reference value, not the winning bid.")
  })

  # When the centre picked on the map has no dot in V7, say why on the chart itself
  v7_missing_note <- function(d) {
    id <- sel_id()
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
    d <- centre_gap |> filter(cuisine_group == input$v7_cuisine)
    if (!nrow(d)) return(empty_plot("No centres for this cuisine"))
    med <- median(d$median_amr)
    d$opp <- d$gap_pts > 0 & d$median_amr <= med
    d$hov <- sprintf("%s<br>%s<br>Gap %+.1f pts (reviews %.1f%% vs stalls %.1f%%)<br>Median rent S$%s",
                     d$centre_name, tools::toTitleCase(tolower(d$planning_area_name)), d$gap_pts,
                     d$engagement_pct, d$supply_pct, comma(round(d$median_amr)))
    best <- d |> filter(opp, !centre_id %in% sel_id()) |> arrange(desc(gap_pts)) |> head(4)  # the selection gets its own label
    xr <- range(c(0, d$gap_pts)) + c(-2, 12)
    plot_ly(source = "v7") |>
      add_markers(data = filter(d, !opp), x = ~gap_pts, y = ~median_amr, text = ~hov, hoverinfo = "text",
                  customdata = ~centre_id, marker = list(color = "#BAC4CA", size = 9, opacity = 0.8)) |>
      add_trace(data = filter(d, opp), x = ~gap_pts, y = ~median_amr, type = "scatter", mode = "markers",
                text = ~hov, hoverinfo = "text", customdata = ~centre_id,
                marker = list(color = BRAND$green_dark, size = 10, opacity = 0.9)) |>
      add_text(data = best, x = ~gap_pts, y = ~median_amr, text = ~short_name, textposition = "top left",
               textfont = list(size = 11, color = "#1E2B22"), hoverinfo = "skip") |>
      mck_plotly(margin = list(l = 10, r = 10, t = 10, b = 10)) |>
      layout(xaxis = list(title = "Engagement \u2212 supply gap (percentage points)", range = xr,
                          ticksuffix = " pts", showline = TRUE, linecolor = "#C9D0D4", ticks = "outside"),
             yaxis = list(title = "Median assessed market rent (S$)", tickprefix = "S$", tickformat = ",",
                          automargin = TRUE),
             shapes = list(
               list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper",
                    line = list(color = "#8B96A5", dash = "dash", width = 1)),
               list(type = "line", y0 = med, y1 = med, x0 = 0, x1 = 1, xref = "paper",
                    line = list(color = "#8B96A5", dash = "dash", width = 1))),
             annotations = c(list(list(x = 0.99, y = 0.01, xref = "paper", yref = "paper", showarrow = FALSE,
                                       xanchor = "right", yanchor = "bottom",
                                       text = "<b>UNDER-SUPPLIED + BELOW-MEDIAN RENT</b>",
                                       font = list(color = BRAND$green_dark, size = 12))),
                             v7_missing_note(d))) |>
      add_sel_ring(d, x = "gap_pts", y = "median_amr") |>
      event_register("plotly_click")
  })

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

  output$v6_headline <- renderUI(paste("Review share vs stall share at", v6_name()))
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
    lim <- max(abs(d$gap_pts)) * 1.3
    plot_ly(d, x = ~gap_pts, y = ~cuisine_group, type = "bar", orientation = "h",
            marker = list(color = ~col), text = ~label, textposition = "outside", cliponaxis = FALSE,
            textfont = list(color = "#404041"), hoverinfo = "text",
            hovertext = ~sprintf("%s<br>Reviews %.1f%% vs stalls %.1f%%<br>Gap %+.1f pts",
                                 cuisine_group, engagement_pct, supply_pct, gap_pts)) |>
      mck_plotly(margin = list(l = 10, r = 30, t = 28, b = 10)) |>
      layout(xaxis = list(visible = FALSE, range = c(-lim, lim), zeroline = FALSE),
             yaxis = list(automargin = TRUE, ticks = "outside", ticklen = 10, tickcolor = "rgba(0,0,0,0)"),
             bargap = 0.45,
             shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = "#58595B", width = 1))),
             annotations = list(
               list(x = 0.99, y = 1.02, xref = "paper", yref = "paper", xanchor = "right", yanchor = "bottom",
                    showarrow = FALSE, text = "more reviews than stalls \u2192", font = list(size = 11, color = BRAND$green_dark)),
               list(x = 0.01, y = 1.02, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom",
                    showarrow = FALSE, text = "\u2190 more stalls than reviews", font = list(size = 11, color = "#C27C0E"))))
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
                            "not in this chart: no rent or cuisine data")
  output$v8_sel <- sel_chip(function(id) !is.null(v8) && id %in% v8$centre_id,
                            "not in this chart: no population data")

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
  v9_unassessed <- sprintf("%d of %d centres either had no stalls on offer, too few to be reliable, or none early enough to judge.",
                           sum(v9_counts[c("none", "few", "late")], na.rm = TRUE), nrow(v9_status))
  v9_why <- function(r) {
    switch(r$status,
      none = "Not enough tender activity to assess — this centre advertised no stalls in the past year.",
      few  = sprintf("Not enough tender activity to assess — only %d stall%s advertised, too few for a reliable rate.",
                     r$n_stalls, if (r$n_stalls == 1) " was" else "s were"),
      late = sprintf("Not enough time to assess — its stalls were advertised in the last %d months, too recently to see whether anyone took them.",
                     V9_SETTLE_MONTHS))
  }
  v9_band <- function(s) if (s < V9_Q1) "worse than most centres" else if (s >= V9_Q3) "better than most centres" else "typical"

  output$v9_headline <- renderUI(paste("Where stalls get taken at", v9_name()))
  output$v9_lead <- renderUI({
    r <- v9_row()
    if (r$status != "ok") return(v9_why(r))
    sprintf("%d of %d assessed stalls (%s) did not return to tender at %s, %s.", r$taken_first, r$stalls_assessed,
            percent(r$share_taken, accuracy = 1), v9_name(), v9_band(r$share_taken))
  })
  output$v9_body <- renderUI({
    r <- v9_row()
    bench <- sprintf("The median centre scores %s (below %s is worse than most, %s and above is better than most): half of centres do better, half do worse.",
                     percent(V9_MEDIAN, accuracy = 1), percent(V9_Q1, accuracy = 1), percent(V9_Q3, accuracy = 1))
    if (r$status != "ok")
      return(paste0(v9_unassessed, " ", bench, " Non-return is a proxy for a stall being taken, not proof of an award."))
    read <- if (r$share_taken > V9_MEDIAN)
      "Above the median — stalls here get taken and stay taken. Good sign, but expect competition and expect to pay."
    else if (r$share_taken < V9_MEDIAN)
      "Below the median — stalls keep returning to tender. Either nobody bids, or tenants don’t last. Cheap rent here is a warning, not a bargain."
    else "At the median — as many centres do better as do worse."
    paste0(read, " Always read the stall count: 2 of 4 stalls and 14 of 27 both show 50%, but one is nearly a coin flip and the other is real evidence. ",
           bench, " ", v9_unassessed)
  })
  output$v9_sel <- sel_chip(function(id) id %in% v9_all$centre_id, "too little tender activity to assess")

  output$v9_plot <- renderPlotly({
    r <- v9_row()
    if (r$status != "ok") return(message_plot("Not assessed", v9_why(r)))
    # the gauge (and its number, drawn at the base of the arc) sits above the colour key, never on it
    plot_ly(type = "indicator", mode = "gauge+number", value = round(100 * r$share_taken, 1),
            domain = list(x = c(0, 1), y = c(0.16, 1)),
            number = list(suffix = "%", font = list(size = 44, color = "#1E2B22")),
            title = list(text = sprintf("%d of %d assessed stalls did not return", r$taken_first, r$stalls_assessed),
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
                                     text = sprintf("Orange: below %s · grey: typical · green: %s and above · line: median %s",
                                                    percent(V9_Q1, accuracy = 1), percent(V9_Q3, accuracy = 1),
                                                    percent(V9_MEDIAN, accuracy = 1)),
                                     font = list(size = 11, color = "#58595B"))))
  })

  output$v8_plot <- renderPlotly({
    req(v8)
    d <- v8
    mid_x <- median(d$straight_line_distance_m)
    mid_y <- median(d$avg_daily_tap_outs)
    d$zone <- ifelse(d$straight_line_distance_m <= mid_x & d$avg_daily_tap_outs >= mid_y, "close", "other")
    d$hov <- sprintf("%s<br>%s · %s m from the exit<br>%s exits a day · %s residents",
                     d$centre_name, d$station, comma(round(d$straight_line_distance_m)),
                     comma(round(d$avg_daily_tap_outs)), comma(d$subzone_population_2026))
    ref <- 2 * max(d$subzone_population_2026) / (34^2)   # bubble area scale (sizemode = "area")
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
             yaxis = list(title = "Average daily station exits", tickformat = "~s", automargin = TRUE),
             shapes = list(
               list(type = "line", x0 = mid_x, x1 = mid_x, y0 = 0, y1 = 1, yref = "paper",
                    line = list(color = "#8B96A5", dash = "dash", width = 1)),
               list(type = "line", y0 = mid_y, y1 = mid_y, x0 = 0, x1 = 1, xref = "paper",
                    line = list(color = "#8B96A5", dash = "dash", width = 1))),
             annotations = list(list(x = 0.01, y = 0.99, xref = "paper", yref = "paper", showarrow = FALSE,
                                     xanchor = "left", yanchor = "top", text = "<b>CLOSE + BUSY</b>",
                                     font = list(color = BRAND$green_dark, size = 13)))) |>
      add_sel_ring(d, x = "straight_line_distance_m", y = "avg_daily_tap_outs") |>
      event_register("plotly_click")
  })

  # Facts about the centre picked on the map that V8 is about: exit and station, station activity,
  # subzone residents, how each compares with the median centre, and where it sits in the chart
  output$v8_detail <- renderUI({
    req(v8)
    id <- sel_id(); if (!length(id) || is.na(id)) id <- DEFAULT_CENTRE
    cen <- centres[centres$centre_id == id, ]
    if (!nrow(cen)) return(NULL)
    nm <- sub(" \\(.*$", "", cen$centre_name)
    fact <- function(label, value, sub = NULL)
      div(class = "viz-fact", span(class = "viz-fact-label", label),
          span(class = "viz-fact-value", value, if (!is.null(sub)) tags$small(sub)))
    r <- v8[v8$centre_id == id, ]
    if (!nrow(r)) {
      return(div(class = "viz-detail",
        div(class = "viz-detail-title", nm, span(class = "viz-tag viz-tag-off", "Not in this chart")),
        if (!is.na(cen$nearest_exit_distance_m))
          fact("Nearest MRT/LRT exit", paste0(fmt_num(cen$nearest_exit_distance_m), " m"), cen$nearest_mrt_station),
        fact("Residents of its subzone (2026)", fmt_num(cen$resident_population),
             if (is.na(cen$resident_population)) "no published figure" else NULL),
        p(class = "viz-detail-note",
          "V8 needs a matched station with Sep 2026 tap-outs and a subzone population, so this centre is left out of the chart.")))
    }
    med_d <- median(v8$straight_line_distance_m); med_t <- median(v8$avg_daily_tap_outs)
    med_p <- median(v8$subzone_population_2026)
    near <- r$straight_line_distance_m <= med_d; busy <- r$avg_daily_tap_outs >= med_t
    zone <- if (near && busy) "Close + busy" else if (near) "Close, quieter station"
            else if (busy) "Far, busier station" else "Far, quieter station"
    pct_busier <- round(100 * mean(v8$avg_daily_tap_outs < r$avg_daily_tap_outs))
    pct_pop <- round(100 * mean(v8$subzone_population_2026 < r$subzone_population_2026))
    is_lrt <- grepl("LRT", r$nearest_station_name)
    div(class = "viz-detail",
      div(class = "viz-detail-title", nm, span(class = paste("viz-tag", if (!(near && busy)) "viz-tag-off"), zone)),
      fact("Nearest exit", paste0(fmt_num(r$straight_line_distance_m), " m"),
           sprintf("%s%s · median centre %s m", r$station, if (is_lrt) " LRT" else " MRT", fmt_num(med_d))),
      fact("Station activity", paste0(fmt_num(r$avg_daily_tap_outs), " exits a day"),
           sprintf("busier than %d%% of centres · median %s", pct_busier, fmt_num(med_t))),
      fact("Residents of its subzone (2026)", fmt_num(r$subzone_population_2026),
           sprintf("%s · more than %d%% of centres · median %s", tools::toTitleCase(tolower(cen$subzone_name)),
                   pct_pop, fmt_num(med_p))),
      if (!is.na(cen$exits_within_500m))
        fact("Within 500 m", paste0(plural(cen$exits_within_500m, "exit"), ", ", plural(cen$stations_within_500m, "station"))),
      if (is_lrt)
        p(class = "viz-detail-note", "The nearest station is on the LRT, which carries far fewer riders than MRT stations, so it can never read as busy."),
      if (!identical(cen$nea_status, "Existing"))
        p(class = "viz-detail-note", paste0("Status: ", cen$nea_status, ".")),
      p(class = "viz-detail-note", "Straight-line distance; station exits are not hawker visitors. Close + busy means nearer than the median exit distance and above the median station activity."))
  })

  output$v8_lead <- renderUI({
    req(v8)
    near <- v8$straight_line_distance_m <= median(v8$straight_line_distance_m) &
      v8$avg_daily_tap_outs >= median(v8$avg_daily_tap_outs)
    sprintf("%d of %d centres are both closer than the median exit distance and busier than the median station.",
            sum(near), nrow(v8))
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
  observe({
    e <- event_data("plotly_click", source = "v7")
    if (!is.null(e$customdata)) pick_on_map(e$customdata[[1]])
  })
  observe({
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
    updateSelectInput(session, "cuisine", choices = c("Any cuisine" = "All", cu), selected = v)
  }
  observeEvent(input$v4_cuisine, set_map_cuisine(input$v4_cuisine), ignoreInit = TRUE)
  observeEvent(input$v7_cuisine, set_map_cuisine(input$v7_cuisine), ignoreInit = TRUE)

  # Clicking a planning area on V4 sets the map's region and planning area filters
  observeEvent(input$v4_map_shape_click, {
    pa <- input$v4_map_shape_click$id
    if (is.null(pa)) return()
    if (!pa %in% PAS) {
      showNotification(paste0("No hawker centres in ", tools::toTitleCase(tolower(pa)), "."), type = "warning", duration = 4)
      return()
    }
    r <- unique(centres$region_name[centres$planning_area_name == pa])[1]
    pas <- sort(unique(centres$planning_area_name[centres$region_name == r]))
    updateSelectInput(session, "region", selected = r)
    updateSelectInput(session, "pa", choices = c("All planning areas" = "All", named_choices(pas)), selected = pa)
  })

  # One "Add to comparison" button per chart: adds the chart's centre to the comparison table below.
  # It turns into a quiet "In comparison" tag once the centre is on the shortlist.
  add_button <- function(input_id, id_fn) {
    renderUI({
      id <- suppressWarnings(as.numeric(id_fn()))
      if (!length(id) || is.na(id)) return(NULL)
      nm <- sub(" \\(.*$", "", name_of(id))
      if (as.character(id) %in% shortlist())
        tags$button(type = "button", class = "viz-add viz-add-done", disabled = NA,
                    title = paste0(nm, " is in the comparison table"), icon("check"), "In comparison")
      else
        actionButton(input_id, tagList(icon("plus"), "Add to comparison"), class = "viz-add",
                     title = paste0("Add ", nm, " to the comparison table"))
    })
  }
  output$v3_add <- add_button("add_v3", v3_shown)
  output$v4_add <- add_button("add_v4", sel_id)
  output$v6_add <- add_button("add_v6", v6_id)
  output$v7_add <- add_button("add_v7", sel_id)
  output$v8_add <- add_button("add_v8", sel_id)
  output$v9_add <- add_button("add_v9", sel_id)
  observeEvent(input$add_v3, add_to_compare(v3_shown()))
  observeEvent(input$add_v4, add_to_compare(sel_id()))
  observeEvent(input$add_v6, add_to_compare(v6_id()))
  observeEvent(input$add_v7, add_to_compare(sel_id()))
  observeEvent(input$add_v8, add_to_compare(sel_id()))
  observeEvent(input$add_v9, add_to_compare(sel_id()))

  # ===========================================================================
  # About us: star rating and subscribe. Nothing is stored or sent anywhere yet
  # (no database or mailing service is connected); the visitor just gets a reply.
  # ===========================================================================
  observeEvent(input$rating, {
    showNotification(sprintf("Thanks for rating Hawker Site Finder %d out of 5.", as.integer(input$rating)),
                     type = "message", duration = 4)
  })

  observeEvent(input$sub_submit, {
    email <- trimws(if (is.null(input$sub_email)) "" else input$sub_email)
    if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) {
      showNotification("Please enter a valid email address.", type = "warning", duration = 4)
      return()
    }
    showNotification("Thanks for subscribing! (Preview: email addresses are not stored yet.)",
                     type = "message", duration = 6)
    updateTextInput(session, "sub_email", value = "")
  })
}
