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
               title = "Hawker centres per planning area", top = "Most centres"),
      density = list(bins = c(0, 0.001, 2, 4, 7, Inf), cols = five,
                     labels = c("0", "Under 2", "2–4", "4–7", "7 or more"),
                     title = "Centres per 10 km²", top = "Highest density"),
      stalls = list(bins = c(0, 1, 101, 251, 451, Inf), cols = five,
                    labels = c("0", "1–100", "101–250", "251–450", "451 or more"),
                    title = "Cooked-food stalls per planning area", top = "Most cooked-food stalls"),
      residents = list(bins = c(0, 1, 10000, 50000, 100000, 200000, Inf), cols = BRAND_SEQ,
                       labels = c("0", "Under 10,000", "10,000–49,999", "50,000–99,999",
                                  "100,000–199,999", "200,000 or more"),
                       title = "Residents per planning area (2026)", top = "Most residents"))
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

  # Top 5 planning areas for the chosen shading
  output$map_top <- renderUI({
    sc <- map_scale()
    a <- sf::st_drop_geometry(area_stats())
    v <- a[[input$map_metric]]
    o <- head(order(v, decreasing = TRUE), 5)
    o <- o[v[o] > 0]
    fmt <- if (input$map_metric == "density") function(x) sprintf("%.1f", x) else comma
    tagList(
      div(class = "info-title", sc$top),
      if (!length(o)) tag_note("No planning areas match the filters.")
      else lapply(o, function(i)
        div(class = "top-row", span(a$pa_title[i]), span(class = "num", fmt(v[i]))))
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
    area_labels <- sprintf("<strong>%s</strong><br>%d centres · %s stalls<br>%.1f centres per 10 km²<br>%s residents",
                           a$pa_title, a$n, comma(a$stalls), a$density, comma(a$residents)) |>
      lapply(htmltools::HTML)
    # Tooltip (V1+V2): cuisine mix, vacant stalls and rent of the centre
    centre_labels <- paste0(
      sprintf("<strong>%s</strong><br>%s · %s stalls · %d tender listings<br>%s<br>Vacant stalls: %s of %s observed",
              d$centre_name, d$pa_title, fmt_num(d$stalls), d$n_listings,
              ifelse(is.na(d$top3), "No cuisine data", d$top3),
              fmt_num(d$vacant_units), fmt_num(d$observed_units)),
      ifelse(is.na(d$median_amr), "", paste0("<br>Median rent ", fmt_sgd(d$median_amr)))) |>
      lapply(htmltools::HTML)

    p <- leafletProxy("map") |>
      clearShapes() |> clearMarkers() |> clearControls() |>
      add_sg_border() |>
      addPolygons(data = a, fillColor = pal(vals), fillOpacity = 0.75,
                  color = "#FFFFFF", weight = 1, label = area_labels,
                  highlightOptions = highlightOptions(weight = 2.5, color = "#404041",
                                                      bringToFront = FALSE))
    if (nrow(d)) {
      p |>
        addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude,
                         layerId = ~centre_id,
                         radius = ~3 + sqrt(coalesce(stalls, 0)) / 2,
                         color = "#FFFFFF", weight = 1.5,
                         fillColor = "#FAA619", fillOpacity = 0.9,
                         label = centre_labels)
    }
    # MRT/LRT exits (merged from the old Location tab)
    if (isTRUE(input$map_exits)) {
      leafletProxy("map") |>
        addCircleMarkers(data = mrt_exits, lng = ~longitude, lat = ~latitude, group = "exits",
                         radius = 2, color = "#4C83C2", stroke = FALSE, fillOpacity = 0.7,
                         label = ~station_name)
    }
  })

  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    selected_centre(id)
    # Also select it for the Cuisine Gap charts (if it has cuisine data)
    if (id %in% cg_centres()$centre_id) updateSelectInput(session, "cg_centre", selected = id)
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
                                    " · median rent ", fmt_sgd(c$median_amr)))
    )
  })

  output$centre_cuisine <- renderPlotly({
    id <- selected_centre()
    if (is.null(id)) return(empty_plot("Cuisine mix appears here"))
    d <- filter(cuisine_by_centre, centre_id == id) |> arrange(stalls)
    if (nrow(d) == 0) return(empty_plot("No cuisine data for this centre"))
    plot_ly(d, x = ~stalls, y = ~factor(cuisine_group, levels = cuisine_group),
            type = "bar", orientation = "h", marker = list(color = "#0D743B"),
            hovertemplate = "%{y}: %{x} stalls<extra></extra>") |>
      layout(xaxis = list(title = "Stalls"), yaxis = list(title = ""),
             margin = list(l = 10, r = 10, t = 10, b = 30)) |>
      config(displayModeBar = FALSE)
  })

  centre_table_data <- reactive({
    fc() |>
      transmute(centre_id, Centre = centre_name, `Planning area` = pa_title,
                Stalls = stalls, `Tender listings` = n_listings,
                `Median rent (S$)` = median_amr,
                `MRT exit (m)` = round(nearest_exit_distance_m),
                `Residents (subzone)` = resident_population)
  })

  output$centre_table <- renderDT({
    datatable(centre_table_data(), rownames = FALSE, selection = "single",
              extensions = "Buttons",
              options = list(pageLength = 8, scrollX = TRUE, dom = "Bfrtip",
                             buttons = c("csv"),
                             columnDefs = list(list(visible = FALSE, targets = 0))))
  })

  observeEvent(input$centre_table_rows_selected, {
    i <- input$centre_table_rows_selected
    if (length(i)) selected_centre(centre_table_data()$centre_id[i])
  })

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

  observeEvent(input$reset_t2, {
    updateSelectInput(session, "cg_cuisine", selected = "Western")
    updateSelectInput(session, "cg_centre", selected = head(arrange(cg_centres(), centre_name)$centre_id, 1))
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

  # ===========================================================================
  # Tab 3: Compare Centres
  # ===========================================================================

  # Keep the sliders consistent with the chosen planning area(s), cuisine and
  # MRT station(s): each spans what exists among those centres with a tender listing.
  fit_s_sliders <- function(pas, cuisine, stations) {
    b <- centres |> filter(n_listings > 0)
    if (length(pas)) b <- b |> filter(planning_area_name %in% pas)
    if (nrow(b) == 0) return()
    up  <- function(x, step) ceiling(max(x, na.rm = TRUE) / step) * step
    low <- function(x, step) floor(min(x, na.rm = TRUE) / step) * step
    mrt <- dist_to_stations(b, stations)
    if (any(!is.na(mrt))) {
      updateSliderInput(session, "s_mrt_max", min = 0, max = up(mrt, 50), value = up(mrt, 50))
    }
    if (any(!is.na(b$median_amr))) {
      ar <- c(low(b$median_amr, 50), up(b$median_amr, 50))
      if (ar[1] == ar[2]) ar[2] <- ar[1] + 50
      updateSliderInput(session, "s_max_amr", min = ar[1], max = ar[2], value = ar[2])
    }
    if (any(!is.na(b$resident_population))) {
      pr <- c(low(b$resident_population, 1000), up(b$resident_population, 1000))
      if (pr[1] == pr[2]) pr[2] <- pr[1] + 1000
      updateSliderInput(session, "s_pop", min = pr[1], max = pr[2], value = pr)
    }
    cm <- cuisine_by_centre |>
      filter(cuisine_group == cuisine, centre_id %in% b$centre_id) |>
      pull(stalls)
    cmax <- max(c(1, cm), na.rm = TRUE)
    updateSliderInput(session, "s_comp", min = 0, max = cmax, value = c(0, cmax))
  }

  observeEvent(list(input$s_pa, input$s_cuisine, input$s_station), {
    fit_s_sliders(input$s_pa, input$s_cuisine, input$s_station)
  }, ignoreNULL = FALSE)

  observeEvent(input$reset_t3, {
    updateSelectInput(session, "s_cuisine", selected = "Western")
    updateSelectizeInput(session, "s_pa", selected = character(0))
    updateSelectizeInput(session, "s_station", selected = character(0))
    updateCheckboxInput(session, "s_keep_na", value = TRUE)
    fit_s_sliders(character(0), "Western", character(0))
  })

  # Centres with a tender listing that pass every filter (no score, no ranking)
  matched <- reactive({
    comp <- cuisine_by_centre |>
      filter(cuisine_group == input$s_cuisine) |>
      select(centre_id, comp_stalls = stalls, comp_share = share)
    d <- centres |> filter(n_listings > 0) |>
      left_join(comp, by = "centre_id") |>
      mutate(comp_stalls = ifelse(has_cuisine, coalesce(comp_stalls, 0), NA),
             comp_share  = ifelse(has_cuisine, coalesce(comp_share, 0), NA))
    d$mrt_dist <- dist_to_stations(d, input$s_station)
    pop <- input$s_pop; cmp <- input$s_comp
    d |>
      filter(
        length(input$s_pa) == 0 | planning_area_name %in% input$s_pa,
        is.na(mrt_dist) | mrt_dist <= input$s_mrt_max,
        is.na(median_amr) | median_amr <= input$s_max_amr,
        is.na(resident_population) | (resident_population >= pop[1] & resident_population <= pop[2]),
        if (isTRUE(input$s_keep_na)) is.na(comp_stalls) | (comp_stalls >= cmp[1] & comp_stalls <= cmp[2])
        else !is.na(comp_stalls) & comp_stalls >= cmp[1] & comp_stalls <= cmp[2]
      ) |>
      arrange(planning_area_name, centre_name)
  })

  # Keep the compare choices in sync with the matching centres (nothing pre-selected)
  observe({
    d <- matched()
    current <- isolate(input$s_compare)
    sel <- current[current %in% d$centre_id]
    updateSelectizeInput(session, "s_compare",
                         choices = setNames(d$centre_id, d$centre_name), selected = sel)
  })

  add_to_compare <- function(id) {
    cur <- input$s_compare
    new <- unique(c(cur, as.character(id)))
    updateSelectizeInput(session, "s_compare", selected = new)
  }


  observeEvent(input$add_compare, {
    id <- selected_centre()
    if (is.null(id)) return()
    if (!id %in% matched()$centre_id) {
      showNotification("This centre has no tender listing (or is outside your filters), so it is not in the comparison list.",
                       type = "warning")
      return()
    }
    nav_select("tabs", "3 Compare Centres")
    add_to_compare(id)
  })

  # ---- Map of matching centres: zooms to the chosen planning area(s) ----------
  output$s_map_title <- renderUI({
    total <- sum(centres$n_listings > 0)
    zoom <- if (length(input$s_pa)) paste0(" · zoomed to ", paste(tools::toTitleCase(tolower(input$s_pa)), collapse = ", ")) else ""
    div(class = "d-flex flex-wrap gap-2 align-items-center",
        tags$strong(paste0("Map of matching centres", zoom)),
        tags$span(class = "badge bg-light text-dark", paste(nrow(matched()), "of", total, "centres match")),
        tags$span(class = "badge bg-light text-dark", "Hover = details · Click = add to comparison"))
  })

  output$s_map <- renderLeaflet(base_map())

  s_map_ready <- map_ready("s_map")

  observe({
    req(s_map_ready())  # the map renders when Tab 3 is first opened
    d <- matched()
    p <- leafletProxy("s_map") |> clearMarkers() |> clearShapes() |> add_sg_border()
    pas <- input$s_pa
    if (length(pas)) {
      a <- planning_areas |> filter(planning_area_name %in% pas)
      p <- p |> addPolygons(data = a, fill = TRUE, fillColor = "#DDECC1", fillOpacity = 0.35,
                            color = "#0D743B", weight = 2.5, label = ~pa_title)
      bb <- as.numeric(sf::st_bbox(a))
    } else if (nrow(d)) {
      bb <- c(min(d$longitude), min(d$latitude), max(d$longitude), max(d$latitude))
    } else {
      bb <- NULL
    }
    if (nrow(d)) {
      lab <- sprintf(paste0("<b>%s</b><br>Median rent %s · %s m to MRT",
                            "<br>Residents in subzone %s · %s cooked-food stalls",
                            "<br>%s stalls (competitors): %s",
                            "<br><i>Click the point to add to compare</i>"),
                     d$centre_name, fmt_sgd(d$median_amr), fmt_num(d$mrt_dist),
                     fmt_num(d$resident_population), fmt_num(d$stalls),
                     input$s_cuisine,
                     ifelse(is.na(d$comp_stalls), "no data", d$comp_stalls)) |>
        lapply(HTML)
      p <- p |> addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude, layerId = ~centre_id,
                                 radius = ~4 + sqrt(coalesce(stalls, 0)) / 2,
                                 color = "#FFFFFF", weight = 1.5, fillColor = "#FAA619",
                                 fillOpacity = 0.95, label = lab)
    }
    if (!is.null(bb)) p |> fitBounds(bb[1], bb[2], bb[3], bb[4])
  })

  observeEvent(input$s_map_marker_click, {
    id <- input$s_map_marker_click$id
    if (!is.null(id)) add_to_compare(id)
  })

  # ---- Comparison table: no limit on the number of centres --------------------
  compare_rows <- reactive({
    ids <- as.numeric(input$s_compare)
    if (!length(ids)) return(NULL)
    d <- matched() |> filter(centre_id %in% ids)
    d <- d[match(ids, d$centre_id), , drop = FALSE] |> filter(!is.na(centre_id))
    latest <- tenders |>
      filter(centre_id %in% ids) |>
      group_by(centre_id) |>
      arrange(desc(exercise_month), .by_group = TRUE) |>
      slice(1) |>
      ungroup() |>
      transmute(centre_id,
                latest = paste0(stall_no_normalised, " · ", fmt_num(approximate_area_m2), " m² · ",
                                format(exercise_month, "%b %Y")))
    d |> left_join(latest, by = "centre_id")
  })

  output$s_compare_table <- renderDT({
    d <- compare_rows()
    if (is.null(d) || !nrow(d)) {
      return(datatable(data.frame(Message = "Click centres on the map to compare them."),
                       rownames = FALSE, options = list(dom = "t")))
    }
    out <- d |>
      transmute(`#` = row_number(), Centre = centre_name,
                `Median rent` = round(median_amr), `MRT distance (m)` = round(mrt_dist),
                `Residents (subzone)` = resident_population,
                !!paste(input$s_cuisine, "stalls") := comp_stalls,
                `Latest listing` = latest)
    datatable(out, rownames = FALSE, extensions = c("RowReorder", "Buttons"),
              options = list(dom = "Bt", paging = FALSE, rowReorder = TRUE, ordering = TRUE,
                             buttons = list(list(extend = "csv", text = "Download CSV")))) |>
      formatCurrency("Median rent", currency = "S$", digits = 0) |>
      formatRound(c("MRT distance (m)", "Residents (subzone)"), digits = 0)
  })

  output$s_summary <- renderUI({
    ids <- as.numeric(input$s_compare)
    if (!length(ids)) return(tag_note("Click a centre on the map to see its summary."))
    c <- matched() |> filter(centre_id == ids[1])
    if (nrow(c) == 0) return(tag_note("This centre is not in the matching list."))
    open <- tenders |>
      filter(centre_id == ids[1]) |>
      arrange(desc(exercise_month), est_monthly_cost) |>
      head(1)
    tags$div(
      tags$h5(c$centre_name),
      tags$p(class = "mb-1", paste0("Latest listing: ", open$stall_no_normalised, " · ",
                                    fmt_num(open$approximate_area_m2), " m² · ",
                                    format(open$exercise_month, "%b %Y"),
                                    " (", as.character(open$release_label), ")")),
      tags$p(class = "mb-1", paste0("Est. monthly cost: ", fmt_sgd(open$est_monthly_cost),
                                    " (rent ", fmt_sgd(open$median_centre_amr_sgd), " + fees)")),
      tags$p(class = "mb-1", paste0(if (length(input$s_station)) "Distance to chosen MRT: " else paste0("Nearest MRT: ", c$nearest_mrt_station, " · "),
                                    fmt_num(c$mrt_dist), " m")),
      tags$p(class = "mb-1", paste0("Residents in subzone: ", fmt_num(c$resident_population))),
      tags$p(class = "mb-0", paste0(input$s_cuisine, " stalls in this centre: ",
                                    ifelse(is.na(c$comp_stalls), "no cuisine data",
                                           paste0(c$comp_stalls, " (", percent(c$comp_share, accuracy = 1), " of classified stalls)")))),
      if (!is.na(open$allowed_trade_hint) && open$allowed_trade_hint != "subject_to_pdf_notes")
        tags$p(class = "small text-warning mt-2 mb-0",
               paste("Trade restriction on this listing:", open$allowed_trade_hint))
    )
  })
}
