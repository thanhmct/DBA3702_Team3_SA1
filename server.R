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
    updateSliderInput(session, "max_amr", min = r[1], max = r[2], value = r[2])
    mrt <- b$nearest_exit_distance_m[!is.na(b$nearest_exit_distance_m)]
    m <- if (length(mrt)) ceiling(max(mrt) / 100) * 100 else MRT_MAX
    updateSliderInput(session, "max_mrt", min = 0, max = m, value = m)
    vmax <- max(c(1, b$vacant_units), na.rm = TRUE)
    updateSliderInput(session, "min_vacant", min = 0, max = vmax, value = 0)
  }, ignoreInit = TRUE)

  observeEvent(input$reset_t1, {
    updateSelectInput(session, "region", selected = "All")
    updateSelectInput(session, "pa", choices = c("All planning areas" = "All", named_choices(PAS)),
                      selected = "All")
    updateSelectInput(session, "cuisine", choices = c("Any cuisine" = "All", CUISINE_GROUPS),
                      selected = "All")
    updateSliderInput(session, "max_amr", min = AMR_RANGE[1], max = AMR_RANGE[2], value = AMR_RANGE[2])
    updateSliderInput(session, "max_mrt", min = 0, max = MRT_MAX, value = MRT_MAX)
    updateCheckboxInput(session, "only_tenders", value = FALSE)
    updateSliderInput(session, "min_vacant", min = 0, max = VAC_MAX, value = 0)
    updateSelectInput(session, "map_metric", selected = "n")
    updateCheckboxInput(session, "map_exits", value = FALSE)
    updateSelectInput(session, "cg_cuisine", selected = "Western")
    updateSelectInput(session, "cg_centre", selected = head(arrange(cg_centres(), centre_name)$centre_id, 1))
  })

  # Centres that pass the Tab 1 filters
  fc <- reactive({
    d <- area_centres(input$region, input$pa)
    if (input$cuisine != "All") {
      ids <- cuisine_by_centre$centre_id[cuisine_by_centre$cuisine_group == input$cuisine]
      d <- filter(d, centre_id %in% ids)
    }
    d <- filter(d, is.na(median_amr) | median_amr <= input$max_amr)
    d <- filter(d, is.na(nearest_exit_distance_m) | nearest_exit_distance_m <= input$max_mrt)
    if (isTRUE(input$only_tenders)) d <- filter(d, n_listings > 0)
    # V2 constraint: vacant stalls (Hawker Guide); centres without data count as 0
    d <- filter(d, coalesce(vacant_units, 0) >= input$min_vacant)
    d
  })

  selected_centre <- reactiveVal(NULL)

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

  output$map <- renderLeaflet(base_map())

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

  # Units shown under the big number in tooltips and in the callout
  METRIC_UNITS <- list(n = "hawker centres", density = "centres per 10 km²",
                       stalls = "cooked-food stalls", residents = "residents (June 2026)")
  fmt_metric <- function(m, v) if (m == "density") sprintf("%.1f", v) else comma(round(v))

  # Planning area with the highest value of the shaded metric (NULL if none)
  map_top <- reactive({
    a <- sf::st_drop_geometry(area_stats())
    v <- a[[input$map_metric]]
    if (!length(v) || max(v, na.rm = TRUE) <= 0) return(NULL)
    i <- which.max(v)
    list(pa = a$pa_title[i], value = v[i], row = i)
  })

  # McKinsey-style exhibit text: the headline states the takeaway, the subtitle
  # names the metric (bold) and its unit
  output$map_headline <- renderText({
    t <- map_top()
    m <- input$map_metric
    if (is.null(t)) return("No hawker centres match these filters.")
    v <- fmt_metric(m, t$value)
    switch(m,
      n = sprintf("%s has the most hawker centres of any planning area (%s).", t$pa, v),
      density = sprintf("%s has the densest hawker network, with %s centres per 10 km².", t$pa, v),
      stalls = sprintf("%s has the most cooked-food stalls of any planning area (%s).", t$pa, v),
      residents = sprintf("%s is the most populous planning area, with %s residents.", t$pa, v))
  })
  output$map_subtitle <- renderUI(HTML(switch(input$map_metric,
    n = "<b>Hawker centres per planning area</b>, number",
    density = "<b>Hawker centres per 10 km²</b>, by planning area",
    stalls = "<b>Cooked-food stalls per planning area</b>, number",
    residents = "<b>Residents per planning area</b>, June 2026")))

  # Opening insight sentence for the page (follows the filters)
  output$page_insight <- renderText({
    d <- fc()
    if (!nrow(d)) return("No hawker centres match these filters. Loosen a filter or press Reset filters.")
    sprintf("%s of %s hawker centres match your filters, across %s planning areas. %s have stalls on offer in NEA tenders, and %s have vacant stalls in the Hawker Guide.",
            comma(nrow(d)), comma(nrow(centres)), comma(n_distinct(d$planning_area_name)),
            comma(sum(d$n_listings > 0)), comma(sum(coalesce(d$vacant_units, 0) > 0)))
  })

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
        div(class = "legend-row", span(class = "legend-exit"),
            paste0("MRT/LRT exit (", comma(nrow(mrt_exits)), ")"))
    )
  })

  t1_map_ready <- map_ready("map")

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
    tt <- labelOptions(className = "mck-tt", direction = "auto", offset = c(12, 0))

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
    # McKinsey-style callout: the top planning area's value as a big serif number
    top <- map_top()
    if (!is.null(top)) {
      pt <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(a)[top$row])))
      p |> addLabelOnlyMarkers(
        lng = pt[1, "X"], lat = pt[1, "Y"],
        label = htmltools::HTML(sprintf("<div class='annot-big'>%s</div><div class='annot-sub'>%s</div>",
                                        fmt_metric(metric, top$value), top$pa)),
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                    className = "mck-annot"))
    }
    # MRT/LRT exits (merged from the old Location tab)
    if (isTRUE(input$map_exits)) {
      leafletProxy("map") |>
        addCircleMarkers(data = mrt_exits, lng = ~longitude, lat = ~latitude, group = "exits",
                         radius = 2, color = "#4C83C2", stroke = FALSE, fillOpacity = 0.7,
                         label = ~station_name)
    }
  })

  # Click = show the centre card (and pick the centre for the charts).
  # Two clicks on the same centre within 0.6 s = double-click = add it to the
  # comparison table (the map's own double-click zoom is off, see base_map()).
  last_click <- reactiveVal(list(id = NULL, t = 0))
  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    if (is.null(id)) return()  # MRT/LRT exit dots have no id
    selected_centre(id)
    if (id %in% cg_centres()$centre_id) updateSelectInput(session, "cg_centre", selected = id)
    now <- as.numeric(Sys.time())
    lc <- last_click()
    if (identical(lc$id, id) && now - lc$t < 0.6) {
      add_to_compare(id)
      last_click(list(id = NULL, t = 0))
    } else {
      last_click(list(id = id, t = now))
    }
  })

  output$centre_card <- renderUI({
    id <- selected_centre()
    if (is.null(id)) return(tag_note("Click a circle on the map or a row in the table."))
    c <- centres[centres$centre_id == id, ]
    tags$div(
      tags$h5(c$centre_name, class = "mb-1"),
      tags$p(class = "mb-1", paste0(c$nea_status, " · ", fmt_num(c$stalls), " cooked-food stalls")),
      tags$p(class = "mb-1", paste0(c$pa_title, " · ", c$region_title)),
      tags$p(class = "mb-1", paste0("Nearest MRT exit: ", c$nearest_mrt_station, " (",
                                    fmt_num(c$nearest_exit_distance_m), " m)")),
      tags$p(class = "mb-1", paste0("Residents in subzone: ", fmt_num(c$resident_population))),
      tags$p(class = "mb-0", paste0("Tender listings: ", c$n_listings,
                                    " · median rent ", fmt_sgd(c$median_amr))),
      actionButton("add_compare", "Compare", icon = icon("plus"),
                   class = "btn-success btn-sm mt-2 w-100")
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
    if (nrow(d) == 0) return(empty_plot("No cuisine data for this centre"))
    # McKinsey style: direct labels, accent colour on the main cuisine, grey for the rest
    cols <- ifelse(d$stalls == max(d$stalls), BRAND$green_dark, "#C8CDD2")
    plot_ly(d, x = ~stalls, y = ~factor(cuisine_group, levels = cuisine_group),
            type = "bar", orientation = "h", marker = list(color = cols),
            text = ~stalls, textposition = "outside", cliponaxis = FALSE,
            textfont = list(color = "#404041"),
            hovertemplate = "%{y}: %{x} stalls<extra></extra>") |>
      mck_plotly(xaxis = list(showticklabels = FALSE), margin = list(l = 10, r = 30, t = 4, b = 4))
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

  compare_data <- reactive({
    ids <- as.numeric(shortlist())
    if (!length(ids)) return(NULL)
    comp <- cuisine_by_centre |>
      filter(cuisine_group == input$cg_cuisine) |>
      select(centre_id, cuisine_stalls = stalls)
    centres[match(ids, centres$centre_id), ] |>
      left_join(comp, by = "centre_id") |>
      mutate(cuisine_stalls = ifelse(has_cuisine, coalesce(cuisine_stalls, 0), NA))
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
      residents = d$resident_population, cuisine = d$cuisine_stalls,
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
    rows <- lapply(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      tags$tr(
        `data-id` = r$centre_id,
        tags$td(class = "drag-handle", title = "Drag to reorder", "⋮⋮"),
        tags$td(div(class = "fw-semibold", r$centre_name), div(class = "small text-muted", r$pa_title)),
        tags$td(class = "num", fmt_sgd(r$median_amr)),
        tags$td(if (is.na(r$nearest_exit_distance_m)) "n/a" else
                  paste0(fmt_num(r$nearest_exit_distance_m), " m · ", r$nearest_mrt_station)),
        tags$td(class = "num", fmt_num(r$resident_population)),
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
          th("cuisine", paste(input$cg_cuisine, "stalls")), th("listing", "Latest listing"), tags$th(""))),
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
        setNames(list(d$cuisine_stalls), paste(input$cg_cuisine, "stalls")),
        `Latest listing` = d$latest_listing, check.names = FALSE)
      write.csv(out, file, row.names = FALSE, na = "")
    }
  )

  # ===========================================================================
  # Tab 2: Cuisine Gap — V1-V6 (visualisation plan)
  # ===========================================================================

  output$survey_status <- renderUI({
    if (HAS_SURVEY) {
      tag_note(paste0("Survey loaded: ", comma(sum(survey$n_people)), " respondents."))
    } else {
      div(class = "alert alert-warning small p-2 mb-0",
          "Synthetic survey not found. Add data/cuisine_survey.csv (planning_area, age_group, ",
          "cuisine_group, n_people) to switch on the demand charts (V4-V7). The supply chart (V3) works now.")
    }
  })

  # Demand share per planning area x cuisine (survey); NULL without survey
  demand_pa <- reactive({
    if (!HAS_SURVEY) return(NULL)
    survey |>
      group_by(planning_area_name = planning_area, cuisine_group) |>
      summarise(people = sum(n_people), .groups = "drop") |>
      group_by(planning_area_name) |>
      mutate(demand = people / sum(people)) |>
      ungroup()
  })

  # Centres with cuisine data (Tab 2 has its own filters, independent of Tab 1)
  cg_centres <- reactive({
    centres |> filter(has_cuisine)
  })

  # Centre choices: centres with cuisine data; keep the current choice if possible
  observe({
    d <- cg_centres() |> arrange(centre_name)
    cur <- isolate(input$cg_centre)
    sel <- if (!is.null(cur) && cur %in% d$centre_id) cur else head(d$centre_id, 1)
    updateSelectInput(session, "cg_centre", choices = setNames(d$centre_id, d$centre_name),
                      selected = sel)
  })
  sel_cg_centre <- reactive({
    req(input$cg_centre)
    centres |> filter(centre_id == as.numeric(input$cg_centre))
  })

  # (V1+V2 cuisine composition with budget and vacancy constraints now lives on
  # the Explore map in Tab 1.)

  # ---------------------------------------------------------------------------
  # NOT DISPLAYED: Tab 2 now shows empty placeholder cards V3-V10 (see ui.R).
  # The chart code below (supply bar, demand heatmap, age bars, diverging bar,
  # gap vs rent) is kept for teammates to reuse: put its output back into the
  # matching placeholder card, e.g. plotlyOutput("cg_supply_centre") in V3.
  # ---------------------------------------------------------------------------

  # ---- V2: supply stacked bar ---------------------------------------------------
  output$cg_supply_centre <- renderPlotly({
    c <- sel_cg_centre()
    share_of <- function(d, label) {
      d |> group_by(cuisine_group) |> summarise(stalls = sum(stalls), .groups = "drop") |>
        mutate(share = stalls / sum(stalls), level = label)
    }
    d <- bind_rows(
      share_of(filter(cuisine_by_centre, centre_id == c$centre_id), "This centre"),
      share_of(filter(cuisine_by_centre, planning_area_name == c$planning_area_name),
               tools::toTitleCase(tolower(c$planning_area_name))),
      share_of(cuisine_by_centre, "All of Singapore")
    )
    if (nrow(d) == 0) return(empty_plot("No cuisine data for this centre"))
    d$level <- factor(d$level, levels = rev(unique(d$level)))
    d$cuisine_group <- factor(d$cuisine_group, levels = names(CUISINE_COLOURS))
    plot_ly(d, x = ~share, y = ~level, color = ~cuisine_group, colors = CUISINE_COLOURS,
            type = "bar", orientation = "h", customdata = ~stalls,
            hovertemplate = "%{y}<br>%{fullData.name}: %{x:.0%} (%{customdata} stalls)<extra></extra>") |>
      layout(barmode = "stack", title = list(text = c$centre_name, font = list(size = 12)),
             xaxis = list(title = "Share of stalls", tickformat = ".0%"), yaxis = list(title = ""),
             legend = list(orientation = "h", y = -0.25, font = list(size = 10))) |>
      config(displayModeBar = FALSE)
  })

  # ---- V3: demand heatmap (choropleth) -----------------------------------------
  output$cg_demand_map <- renderLeaflet({
    m <- base_map(zoom = 10.5)
    dm <- demand_pa()
    if (is.null(dm)) {
      return(m |> addPolygons(data = planning_areas, fillColor = "#EDF0F2", fillOpacity = 0.6,
                              color = "#FFFFFF", weight = 1, label = ~pa_title) |>
               addControl(HTML("<b>Waiting for the synthetic survey</b><br>Add data/cuisine_survey.csv"),
                          position = "topright"))
    }
    v <- dm |> filter(cuisine_group == input$cg_cuisine) |> select(planning_area_name, demand)
    a <- planning_areas |> left_join(v, by = "planning_area_name")
    pal <- colorNumeric(BRAND_SEQ, domain = a$demand, na.color = "#EDF0F2")
    m |>
      addPolygons(data = a, fillColor = ~pal(demand), fillOpacity = 0.85, color = "#FFFFFF",
                  weight = 1, label = sprintf("%s: %s prefer %s", a$pa_title,
                                              ifelse(is.na(a$demand), "no survey data",
                                                     percent(a$demand, 1)), input$cg_cuisine)) |>
      addLegend("bottomright", pal = pal, values = a$demand, title = "Estimated demand",
                labFormat = labelFormat(transform = function(x) 100 * x, suffix = "%"),
                opacity = 1)
  })

  # ---- V4: age-grouped preference ----------------------------------------------
  output$cg_age_plot <- renderPlotly({
    if (!HAS_SURVEY) return(empty_plot("Needs the synthetic survey (age group × cuisine)"))
    d <- survey
    if (input$cg_pa != "All") d <- filter(d, planning_area == input$cg_pa)
    d <- d |>
      group_by(age_group, cuisine_group) |>
      summarise(people = sum(n_people), .groups = "drop") |>
      group_by(age_group) |>
      mutate(share = people / sum(people)) |>
      ungroup()
    if (nrow(d) == 0) return(empty_plot("No survey rows for this planning area"))
    ages <- sort(unique(d$age_group))
    age_cols <- setNames(colorRampPalette(c("#BBD980", "#40AD49", "#0D743B"))(length(ages)), ages)
    d$cuisine_group <- factor(d$cuisine_group, levels = CUISINE_GROUPS)
    plot_ly(d, x = ~cuisine_group, y = ~share, color = ~age_group, colors = age_cols, type = "bar",
            hovertemplate = "%{x}<br>Age %{fullData.name}: %{y:.1%}<extra></extra>") |>
      layout(barmode = "group", yaxis = list(title = "Share of age group", tickformat = ".0%"),
             xaxis = list(title = "", tickangle = -30),
             legend = list(title = list(text = "Age group"), orientation = "h", y = -0.35)) |>
      config(displayModeBar = FALSE)
  })

  # ---- V5: diverging bar at the selected centre ----------------------------------
  output$cg_diverging <- renderPlotly({
    c <- sel_cg_centre()
    dm <- demand_pa()
    if (is.null(dm)) return(empty_plot("Needs the synthetic survey for demand share"))
    sup <- cuisine_by_centre |> filter(centre_id == c$centre_id) |>
      select(cuisine_group, supply = share)
    dem <- dm |> filter(planning_area_name == c$planning_area_name) |> select(cuisine_group, demand)
    d <- tibble(cuisine_group = CUISINE_GROUPS) |>
      left_join(dem, by = "cuisine_group") |> left_join(sup, by = "cuisine_group") |>
      mutate(across(c(demand, supply), ~ coalesce(.x, 0)), gap = demand - supply,
             side = ifelse(gap >= 0, "Under-supplied", "Over-supplied"),
             hov = sprintf("%s<br>Demand %s · supply %s<br>Gap %+.0f pts", cuisine_group,
                           percent(demand, 1), percent(supply, 1), 100 * gap)) |>
      arrange(gap)
    if (all(d$demand == 0)) return(empty_plot("No survey rows for this centre's planning area"))
    d$cuisine_group <- factor(d$cuisine_group, levels = d$cuisine_group)
    plot_ly(d, x = ~gap, y = ~cuisine_group, color = ~side,
            colors = c("Under-supplied" = "#0D743B", "Over-supplied" = "#FAA619"),
            type = "bar", orientation = "h", text = ~hov, textposition = "none",
            hovertemplate = "%{text}<extra></extra>") |>
      layout(title = list(text = c$centre_name, font = list(size = 12)),
             xaxis = list(title = "Demand share − supply share", tickformat = "+.0%",
                          zeroline = TRUE, zerolinecolor = "#404041"),
             yaxis = list(title = ""), legend = list(orientation = "h", y = -0.2)) |>
      config(displayModeBar = FALSE)
  })

  # ---- V6: gap vs rent scatter ---------------------------------------------------
  output$cg_gap_rent <- renderPlotly({
    sup <- cuisine_by_centre |> filter(cuisine_group == input$cg_cuisine) |>
      select(centre_id, supply = share)
    d <- cg_centres() |> filter(!is.na(median_amr)) |>
      left_join(sup, by = "centre_id") |> mutate(supply = coalesce(supply, 0))
    dm <- demand_pa()
    if (!is.null(dm)) {
      d <- d |> left_join(dm |> filter(cuisine_group == input$cg_cuisine) |>
                            select(planning_area_name, demand), by = "planning_area_name") |>
        filter(!is.na(demand)) |> mutate(x = demand - supply)
      xt <- paste(input$cg_cuisine, "gap: demand share − supply share")
    } else {
      d <- d |> mutate(x = -supply)
      xt <- paste("Supply only (survey missing): −", input$cg_cuisine, "share of stalls")
    }
    if (nrow(d) == 0) return(empty_plot("No centres with rent data match the filters"))
    sel <- as.numeric(input$cg_centre)
    d <- d |> mutate(grp = ifelse(centre_id %in% sel, "Selected centre", "Hawker centre"))
    plot_ly(d, x = ~x, y = ~median_amr, color = ~grp,
            colors = c("Hawker centre" = "#40AD49", "Selected centre" = "#FAA619"),
            size = ~stalls, sizes = c(8, 30), type = "scatter", mode = "markers",
            customdata = ~centre_id, text = ~centre_name, source = "cg_v6",
            marker = list(opacity = 0.8),
            hovertemplate = "%{text}<br>Gap %{x:+.0%} · rent S$%{y:,.0f}<extra></extra>") |>
      layout(xaxis = list(title = xt, tickformat = "+.0%", zeroline = TRUE,
                          zerolinecolor = "#808285"),
             yaxis = list(title = "Median centre rent (AMR, S$)"),
             legend = list(orientation = "h", y = -0.25)) |>
      event_register("plotly_click") |>
      config(displayModeBar = FALSE)
  })

  # When the gap vs rent chart is back on the page, re-add its click handler:
  # observeEvent(event_data("plotly_click", source = "cg_v6"), {
  #   id <- event_data("plotly_click", source = "cg_v6")$customdata
  #   if (!is.null(id)) updateSelectInput(session, "cg_centre", selected = id)
  # })
}
