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
  # Tab 1 filters (this tab only)
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
    updateSliderInput(session, "min_vacant", min = 0, max = VAC_MAX, value = 0)
    updateSelectInput(session, "map_metric", selected = "n")
    updateCheckboxInput(session, "map_exits", value = FALSE)
    selected_centre(DEFAULT_CENTRE)  # the map search box follows; no zoom on reset
  })

  # Centres that pass the Tab 1 filters
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
    # V2 constraint: vacant stalls (Hawker Guide); centres without data count as 0
    d <- filter(d, coalesce(vacant_units, 0) >= input$min_vacant)
    d
  })

  selected_centre <- reactiveVal(DEFAULT_CENTRE)  # card opens on Ang Mo Kio 628 Market

  # ===========================================================================
  # Tab 1: Explore
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
    planning_areas |>
      left_join(s, by = "planning_area_name") |>
      mutate(n = coalesce(n, 0L), stalls = coalesce(stalls, 0),
             density = n / area_km2 * 10,
             residents = coalesce(resident_population, 0))
  })

  output$map <- renderLeaflet(base_map(pad_right = 265))  # room for the floating centre card

  # Bins, colours and labels for the planning-area shading: one definition used
  # by the map and by the legend in the info panel beside it
  map_scale <- reactive({
    five <- c("#F6F8F9", "#DDECC1", "#BBD980", "#40AD49", "#0D743B")
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
                       title = "Residents per planning area (2026)"))
  })

  # Units shown under the big number in the tooltips
  METRIC_UNITS <- list(n = "hawker centres", density = "centres per 10 km²",
                       stalls = "cooked-food stalls", residents = "residents (June 2026)")
  fmt_metric <- function(m, v) if (m == "density") sprintf("%.1f", v) else comma(round(v))

  output$map_legend <- renderUI({
    sc <- map_scale()
    tagList(
      div(class = "info-title", sc$title),
      lapply(seq_along(sc$labels), function(i)
        div(class = "legend-row",
            span(class = "legend-swatch", style = paste0("background:", sc$cols[i], ";")),
            sc$labels[i])),
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
    pad <- list(paddingTopLeft = c(20, 60), paddingBottomRight = c(265, 20))
    bb <- if (is.null(sel) || !nrow(sel)) c(103.60, 1.205, 104.05, 1.475) else as.numeric(sf::st_bbox(sel))
    leafletProxy("map") |> flyToBounds(bb[1], bb[2], bb[3], bb[4], options = pad)
  }, ignoreInit = TRUE)

  observe({
    req(t1_map_ready())
    a <- area_stats()
    d <- fc()
    metric <- input$map_metric
    vals <- a[[metric]]
    sc <- map_scale()
    pal <- colorBin(sc$cols, domain = vals, bins = sc$bins, right = FALSE)
    # McKinsey-style tooltips: name, the shaded metric as a big serif number, details
    area_labels <- sprintf(
      "<div class='tt-title'>%s</div><div class='tt-big'>%s</div><div class='tt-sub'>%s</div><div class='tt-meta'>%d centres · %s stalls · %s residents</div>",
      a$pa_title, fmt_metric(metric, vals), METRIC_UNITS[[metric]],
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
      addPolygons(data = a, fillColor = pal(vals), fillOpacity = 0.75,
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

  # The cuisine column follows "Centres that sell": stalls of that cuisine, or
  # each centre's main cuisine when the filter is "Any cuisine"
  cuisine_label <- reactive(if (input$cuisine == "All") "Main cuisine" else paste(input$cuisine, "stalls"))
  cuisine_cell <- function(d) if (input$cuisine == "All") d$main_cuisine else d$cuisine_stalls

  compare_data <- reactive({
    ids <- as.numeric(shortlist())
    if (!length(ids)) return(NULL)
    d <- centres[match(ids, centres$centre_id), ]
    d$cuisine_stalls <- NA_real_
    if (input$cuisine != "All") {
      comp <- cuisine_by_centre |>
        filter(cuisine_group == input$cuisine) |>
        select(centre_id, cuisine_stalls = stalls)
      d <- d |>
        select(-cuisine_stalls) |>
        left_join(comp, by = "centre_id") |>
        mutate(cuisine_stalls = ifelse(has_cuisine, coalesce(cuisine_stalls, 0), NA))
    }
    d
  })

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
      residents = d$resident_population, cuisine = cuisine_cell(d),
      listing = as.numeric(d$latest_month))
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
    th <- function(key, label) {
      arrow <- if (identical(s$col, key)) if (isTRUE(s$desc)) " ▼" else " ▲" else ""
      tags$th(class = "sortable-col", title = "Sort",
              onclick = sprintf("Shiny.setInputValue('compare_sort', {col: '%s', nonce: Math.random()})", key),
              paste0(label, arrow))
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
        tags$td(if (is.na(r$nearest_exit_distance_m)) "n/a" else
                  paste0(fmt_num(r$nearest_exit_distance_m), " m · ", r$nearest_mrt_station)),
        tags$td(class = "num", fmt_num(r$resident_population)),
        if (input$cuisine == "All")
          tags$td(if (is.na(r$main_cuisine)) "no data" else r$main_cuisine)
        else
          tags$td(class = "num", if (is.na(r$cuisine_stalls)) "no data" else r$cuisine_stalls),
        tags$td(if (is.na(r$latest_listing)) "No listing" else r$latest_listing),
        tags$td(tags$button(class = "btn-remove", title = "Remove", `aria-label` = "Remove",
                            onclick = sprintf("Shiny.setInputValue('compare_remove', '%s', {priority: 'event'})",
                                              r$centre_id),
                            "✕")))
    })
    tagList(
      tags$table(class = "table compare-table",
        tags$thead(tags$tr(
          tags$th(""), th("centre", "Centre"), th("rent", "Median rent"),
          th("mrt", "MRT distance (station)"), th("residents", "Residents (subzone)"),
          th("cuisine", cuisine_label()), th("listing", "Latest listing"), tags$th(""))),
        tags$tbody(id = "compare_rows", rows)),
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
        `MRT distance (m)` = round(d$nearest_exit_distance_m), `Nearest MRT station` = d$nearest_mrt_station,
        `Residents (subzone)` = d$resident_population,
        setNames(list(cuisine_cell(d)), cuisine_label()),
        `Latest listing` = d$latest_listing, check.names = FALSE)
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

  # Sync the map search box text whenever a centre is chosen (map click, table row, search, or reset)
  observeEvent(selected_centre(), {
    id <- selected_centre()
    if (!is.null(id)) {
      c <- centres[centres$centre_id == as.numeric(id), ]
      if (nrow(c)) {
        session$sendCustomMessage("set_search_centre", list(id = c$centre_id, name = c$centre_name))
      }
    }
  })


  # ===========================================================================
  # Data visualization: V3 (Ridhwan) and V8 (Lucien) drawn live from the data.
  # Data prepared in global.R; the original scripts are in charts/.
  # ===========================================================================
  # V3: the search box above the chart picks the centre; a centre picked on the map also lands here
  v3_id <- reactiveVal(V3_DEFAULT_ID)
  observeEvent(input$v3_centre, {
    id <- suppressWarnings(as.numeric(input$v3_centre))
    if (!is.na(id) && id %in% V3_IDS) v3_id(id)
  }, ignoreInit = TRUE)
  v3_name <- reactive(sub(" \\(.*$", "", centres$centre_name[centres$centre_id == v3_id()][1]))
  v3_data <- reactive(v3_supply_for(v3_id()))

  output$v3_headline <- renderUI(paste("Cuisine supply at", v3_name()))
  output$v3_lead <- renderUI({
    d <- v3_data()
    sprintf("%s stalls make up %d of %d classified stalls at %s.", d$cuisine_group[1], d$stall_count[1],
            sum(d$stall_count), v3_name())
  })
  output$v3_body <- renderUI({
    d <- v3_data()
    nxt <- if (d$stall_count[2] > 0)
      sprintf("; %s stalls are next at %s", d$cuisine_group[2], percent(d$supply_share[2], accuracy = 0.1)) else ""
    paste0("The chart counts occupied main-meal stalls in ",
           centres$centre_name[centres$centre_id == v3_id()][1], ". ", d$cuisine_group[1],
           " stalls make up ", percent(d$supply_share[1], accuracy = 0.1), nxt,
           ". These are Hawker Guide observations, not an official census of every stall.")
  })

  output$v3_plot <- renderPlotly({
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
    d <- engagement |>
      filter(cuisine_group == input$v4_cuisine) |>
      select(planning_area_name, engagement_pct, total_reviews, restaurant_count, classified_establishments)
    a <- planning_areas |> left_join(d, by = "planning_area_name")
    a$val <- ifelse(is.na(a$engagement_pct) | a$classified_establishments < V47_MIN_ESTABLISHMENTS,
                    NA, a$engagement_pct)
    pal <- colorNumeric(BRAND_SEQ, domain = a$val, na.color = "#E3E8EB")
    lab <- sprintf("%s: %s", a$pa_title,
                   ifelse(is.na(a$val), "too few classified places",
                          sprintf("%.1f%% of reviews (%s places, %s reviews)", a$engagement_pct,
                                  comma(a$restaurant_count), comma(a$total_reviews))))
    base_map() |>
      fitBounds(103.64, 1.235, 104.03, 1.47) |>
      addPolygons(data = a, fillColor = ~pal(val), fillOpacity = 0.9, color = "#FFFFFF", weight = 1,
                  label = lab, highlightOptions = highlightOptions(weight = 2, color = "#404041")) |>
      addLegend("bottomright", pal = pal, values = a$val[!is.na(a$val)],
                title = paste0(input$v4_cuisine, "<br>share of reviews"),
                labFormat = labelFormat(suffix = "%", digits = 0), opacity = 1) |>
      addLegend("bottomright", colors = "#E3E8EB", labels = "Too few classified places", opacity = 1) |>
      add_v4_selection(a)
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
    plot_ly() |>
      add_markers(data = filter(d, !opp), x = ~gap_pts, y = ~median_amr, text = ~hov, hoverinfo = "text",
                  marker = list(color = "#BAC4CA", size = 9, opacity = 0.8)) |>
      add_trace(data = filter(d, opp), x = ~gap_pts, y = ~median_amr, type = "scatter", mode = "markers",
                text = ~hov, hoverinfo = "text",
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
             annotations = list(list(x = 0.99, y = 0.01, xref = "paper", yref = "paper", showarrow = FALSE,
                                     xanchor = "right", yanchor = "bottom",
                                     text = "<b>UNDER-SUPPLIED + BELOW-MEDIAN RENT</b>",
                                     font = list(color = BRAND$green_dark, size = 12)))) |>
      add_sel_ring(d, x = "gap_pts", y = "median_amr")
  })

  # V6: the search box above the chart picks the centre; a centre picked on the map also lands here
  v6_id <- reactiveVal(V6_DEFAULT_ID)
  observeEvent(input$v6_centre, {
    id <- suppressWarnings(as.numeric(input$v6_centre))
    if (!is.na(id) && id %in% V6_IDS) v6_id(id)
  }, ignoreInit = TRUE)
  v6_name <- reactive(sub(" \\(.*$", "", centres$centre_name[centres$centre_id == v6_id()][1]))
  v6_area <- reactive(tools::toTitleCase(tolower(centres$planning_area_name[centres$centre_id == v6_id()][1])))
  v6_data <- reactive(v6_gap_for(v6_id()))

  output$v6_headline <- renderUI(paste("Review share vs stall share at", v6_name()))
  output$v6_lead <- renderUI({
    d <- v6_data(); top <- d[which.max(d$gap_pts), ]
    if (top$gap_pts <= 0) return(sprintf("No cuisine has more review share than stall share at %s.", v6_name()))
    sprintf("%s food has the widest opening at %s: %.1f points more review share than stall share.",
            top$cuisine_group, v6_name(), top$gap_pts)
  })
  output$v6_body <- renderUI({
    d <- v6_data(); low <- d[which.min(d$gap_pts), ]
    paste0("Each bar subtracts a cuisine's share of occupied main-meal stalls at the centre from its share of classified Google Maps reviews in ",
           v6_area(), ". Green bars mark cuisines with more review attention than stalls. Orange bars mark cuisines that already hold more stalls than attention; the largest is ",
           low$cuisine_group, " at ", sprintf("%.1f", low$gap_pts), " points. Google Maps covers restaurants as well as hawker stalls, so the gap is an exploratory signal, not unmet demand.")
  })
  output$v6_plot <- renderPlotly({
    d <- v6_data()
    d$cuisine_group <- factor(d$cuisine_group, levels = d$cuisine_group)
    d$col <- ifelse(d$gap_pts >= 0, BRAND$green_dark, BRAND$orange)
    d$label <- sprintf("%+.1f", d$gap_pts)
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
  # Map selection -> charts. Picking a centre on the map (click, search box, table
  # row) moves V3 and V6 to it and highlights it on V4, V7 and V8. V9 ranks centres
  # and V5/V10 have no chart yet, so they stay as they are.
  # ---------------------------------------------------------------------------
  observeEvent(selected_centre(), {
    id <- suppressWarnings(as.numeric(selected_centre()))
    if (!length(id) || is.na(id)) return()
    name <- centres$centre_name[centres$centre_id == id][1]
    if (id %in% V3_IDS) { v3_id(id); session$sendCustomMessage("set_v3_search", list(name = name)) }
    if (id %in% V6_IDS) { v6_id(id); session$sendCustomMessage("set_v6_search", list(name = name)) }
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

  # V9: share of assessed stalls that did not return to tender, by centre (Casper)
  v9_d <- reactive({
    d <- if (identical(input$v9_half, "worst")) filter(v9_all, share_taken < V9_OVERALL)
         else filter(v9_all, share_taken >= V9_OVERALL)
    arrange(d, desc(share_taken), desc(stalls_assessed))
  })
  output$v9_lead <- renderUI({
    n <- nrow(v9_d()); pct <- percent(V9_OVERALL, accuracy = 1)
    if (identical(input$v9_half, "worst"))
      sprintf("%d of the %d eligible centres fall below the overall %s rate.", n, nrow(v9_all), pct)
    else
      sprintf("Among the eligible centres, %d have rates at or above the overall %s.", n, pct)
  })
  output$v9_body <- renderUI({
    d <- v9_d(); pct <- percent(V9_OVERALL, accuracy = 1)
    first <- d[which.max(d$stalls_assessed), ]   # the centre with the largest sample
    if (identical(input$v9_half, "worst"))
      paste0("These centres re-advertise more of their stalls than average, so a stall there is more likely to come back to tender. ",
             first$short_name, " has ", first$taken_first, " of ", first$stalls_assessed,
             " assessed stalls not returning; the overall rate is ", pct, ". Treat low rates as a tenancy-risk signal. ",
             "Non-return is a proxy for a stall being taken, not proof of an award, and small centres swing easily.")
    else
      paste0("The chart ranks centres where assessed stalls did not return to tender after their first advertisement. ",
             first$short_name, " has ", first$taken_first, " of ", first$stalls_assessed,
             " such stalls; the overall rate is ", pct, ". Non-return is a proxy for a stall being taken, not proof of an award. ",
             "Stalls first advertised in the last ", V9_SETTLE_MONTHS, " months and centres with fewer than ",
             V9_MIN_STALLS, " assessed stalls are excluded.")
  })
  output$v9_plot <- renderPlotly({
    d <- v9_d()
    d$short_name <- factor(d$short_name, levels = rev(unique(d$short_name)))
    d$label <- sprintf("%s (%d of %d)", percent(d$share_taken, accuracy = 1), d$taken_first, d$stalls_assessed)
    col <- if (identical(input$v9_half, "worst")) BRAND$orange else BRAND$green_dark
    plot_ly(d, x = ~share_taken, y = ~short_name, type = "bar", orientation = "h",
            marker = list(color = col), text = ~label, textposition = "outside", cliponaxis = FALSE,
            textfont = list(color = "#404041"), hoverinfo = "text",
            hovertext = ~sprintf("%s<br>%d of %d assessed stalls did not return to tender (%s)",
                                 centre_name, taken_first, stalls_assessed, percent(share_taken, accuracy = 1))) |>
      mck_plotly(margin = list(l = 10, r = 40, t = 26, b = 10)) |>
      layout(xaxis = list(visible = FALSE, range = c(0, 1.3)),
             yaxis = list(automargin = TRUE, ticks = "outside", ticklen = 10, tickcolor = "rgba(0,0,0,0)"), bargap = 0.45,
             shapes = list(list(type = "line", layer = "below", x0 = V9_OVERALL, x1 = V9_OVERALL, y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = "#8B96A5", dash = "dash", width = 1))),
             annotations = list(list(x = V9_OVERALL, y = 1, yref = "paper", xanchor = "left", yanchor = "bottom",
                                     showarrow = FALSE, text = paste0(" overall ", percent(V9_OVERALL, accuracy = 1)),
                                     font = list(size = 11, color = "#58595B"))))
  })

  output$v8_plot <- renderPlotly({
    req(v8)
    d <- v8
    mid_x <- median(d$straight_line_distance_m)
    mid_y <- median(d$avg_daily_tap_outs)
    d$zone <- ifelse(d$straight_line_distance_m <= mid_x & d$avg_daily_tap_outs >= mid_y, "close", "other")
    d$hov <- sprintf("%s<br>%s · %s m from the exit<br>%s exits a day · %s residents<br>Score %.1f (rank %d)",
                     d$centre_name, d$station, comma(round(d$straight_line_distance_m)),
                     comma(round(d$avg_daily_tap_outs)), comma(d$subzone_population_2026),
                     d$combined_score, d$overall_rank)
    top5 <- head(d, 5)
    ref <- 2 * max(d$subzone_population_2026) / (34^2)   # bubble area scale (sizemode = "area")
    bubbles <- function(x, col, alpha) list(color = col, opacity = alpha, line = list(width = 0),
                                            size = x$subzone_population_2026, sizemode = "area",
                                            sizeref = ref, sizemin = 4)
    far <- subset(d, zone == "other"); near <- subset(d, zone == "close")
    xmax <- max(1600, max(top5$straight_line_distance_m) + 100)
    plot_ly() |>
      add_markers(data = far, x = ~straight_line_distance_m, y = ~avg_daily_tap_outs,
                  marker = bubbles(far, "#BAC4CA", 0.55), text = ~hov, hoverinfo = "text") |>
      add_markers(data = near, x = ~straight_line_distance_m, y = ~avg_daily_tap_outs,
                  marker = bubbles(near, BRAND$green, 0.8), text = ~hov, hoverinfo = "text") |>
      add_trace(data = top5, x = ~straight_line_distance_m, y = ~avg_daily_tap_outs, type = "scatter",
                mode = "markers+text", text = ~overall_rank, textposition = "middle center",
                textfont = list(color = "#FFFFFF", size = 12), hovertext = ~hov, hoverinfo = "text",
                marker = list(size = 24, color = BRAND$green_dark, line = list(color = "#FFFFFF", width = 1.5))) |>
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
      add_sel_ring(d, x = "straight_line_distance_m", y = "avg_daily_tap_outs")
  })

  output$v8_top5 <- renderUI({
    req(v8)
    top5 <- head(v8, 5)
    tags$ol(class = "viz-top5",
            lapply(seq_len(nrow(top5)), function(i) {
              r <- top5[i, ]
              tags$li(tags$b(r$centre_name), tags$span(sprintf(
                " · %s · %s m · %s exits/day · score %.1f", r$station,
                comma(round(r$straight_line_distance_m)), comma(round(r$avg_daily_tap_outs)),
                r$combined_score)))
            }))
  })

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
