# =============================================================================
# Old chart code for V3-V7 (supply bar, demand heatmap, age bars, diverging bar,
# gap vs rent), moved out of server.R on 2026-10-27: none of it is on the page.
# The app does NOT load this file. To reuse a chart, copy its block back inside
# server() in server.R, put its output in the matching viz_row() in ui.R, e.g.
#   viz_row("V3", ..., plotlyOutput("cg_supply_centre", height = 300))
# and end the plotly code with |> mck_plotly() (global.R) for the house style.
# Needs objects from global.R (centres, cuisine_by_centre, survey, HAS_SURVEY,
# planning_areas, CUISINE_COLOURS, BRAND_SEQ, CUISINE_GROUPS, empty_plot, base_map)
# and, inside server(), selected_centre() and input$cuisine.
# Note: cg_age_plot filtered by input$cg_pa, an input that no longer exists;
# give it a planning-area input (or drop that line) before using it.
# =============================================================================

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

  sel_cg_centre <- reactive({
    req(selected_centre())
    centres |> filter(centre_id == as.numeric(selected_centre()))
  })

  # (V1+V2 cuisine composition with budget and vacancy constraints now lives on
  # the Explore map in Tab 1.)

  # ---------------------------------------------------------------------------
  # NOT DISPLAYED: Tab 2 now shows empty placeholder cards V3-V10 (see ui.R).
  # The chart code below (supply bar, demand heatmap, age bars, diverging bar,
  # gap vs rent) is kept for teammates to reuse: put its output back into the
  # matching placeholder card, e.g. plotlyOutput("cg_supply_centre") in V3.
  # ---------------------------------------------------------------------------

  # Cuisine the old charts analyse: the "Centres that sell" choice (Western when "Any cuisine")
  cg_cuisine <- reactive(if (input$cuisine == "All") "Western" else input$cuisine)

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
    m <- base_map()
    dm <- demand_pa()
    if (is.null(dm)) {
      return(m |> addPolygons(data = planning_areas, fillColor = "#EDF0F2", fillOpacity = 0.6,
                              color = "#FFFFFF", weight = 1, label = ~pa_title) |>
               addControl(HTML("<b>Waiting for the synthetic survey</b><br>Add data/cuisine_survey.csv"),
                          position = "topright"))
    }
    v <- dm |> filter(cuisine_group == cg_cuisine()) |> select(planning_area_name, demand)
    a <- planning_areas |> left_join(v, by = "planning_area_name")
    pal <- colorNumeric(BRAND_SEQ, domain = a$demand, na.color = "#EDF0F2")
    m |>
      addPolygons(data = a, fillColor = ~pal(demand), fillOpacity = 0.85, color = "#FFFFFF",
                  weight = 1, label = sprintf("%s: %s prefer %s", a$pa_title,
                                              ifelse(is.na(a$demand), "no survey data",
                                                     percent(a$demand, 1)), cg_cuisine())) |>
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
    sup <- cuisine_by_centre |> filter(cuisine_group == cg_cuisine()) |>
      select(centre_id, supply = share)
    d <- cg_centres() |> filter(!is.na(median_amr)) |>
      left_join(sup, by = "centre_id") |> mutate(supply = coalesce(supply, 0))
    dm <- demand_pa()
    if (!is.null(dm)) {
      d <- d |> left_join(dm |> filter(cuisine_group == cg_cuisine()) |>
                            select(planning_area_name, demand), by = "planning_area_name") |>
        filter(!is.na(demand)) |> mutate(x = demand - supply)
      xt <- paste(cg_cuisine(), "gap: demand share − supply share")
    } else {
      d <- d |> mutate(x = -supply)
      xt <- paste("Supply only (survey missing): −", cg_cuisine(), "share of stalls")
    }
    if (nrow(d) == 0) return(empty_plot("No centres with rent data match the filters"))
    sel <- as.numeric(selected_centre())
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
