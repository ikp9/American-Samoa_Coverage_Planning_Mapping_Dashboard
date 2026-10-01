library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(DT)
library(leaflet)
library(plotly)
library(here)
here::i_am("app.R")

# ==============================================================================
# Project files and dashboard data
# ==============================================================================

project_dir <- here::here()
functions_file <- here::here("R", "as_functions.R")
prepare_data_file <- here::here("R", "prepare_data.R")
styles_file <- here::here("www", "styles.css")

required_project_files <- c(functions_file, prepare_data_file)
missing_project_files <- required_project_files[!file.exists(required_project_files)]

if (length(missing_project_files) > 0) {
  stop(
    paste0(
      "Required project file(s) could not be found:\n  - ",
      paste(missing_project_files, collapse = "\n  - "),
      "\n\nOpen American Samoa_Coverage_Planning_Mapping_Dashboard.Rproj and rerun the app."
    ),
    call. = FALSE
  )
}

source(functions_file, local = FALSE)

source(here::here("config.R"), local = FALSE)
source(prepare_data_file, local = FALSE)
drop_excluded_columns <- function(data) {
  data |> select(-matches("rsv|nirsevimab|clesrovimab|beyfortus|enflonsia", ignore.case=TRUE))
}
dashboard_files <- c(
  community = here::here("data", "community_summary.csv"),
  vaccine_needs = here::here("data", "community_vaccine_needs.csv"),
  product_needs = here::here("data", "community_product_needs.csv"),
  patient = here::here("data", "patient_dashboard.csv"),
  quality = here::here("data", "qa_as_geography.csv")
)

if (any(!file.exists(dashboard_files))) {
  message("Dashboard files are missing. Running R/prepare_data.R.")
  prepare_as_dashboard(project_dir=project_dir, config=AS_CONFIG)
}

missing_dashboard_files <- dashboard_files[!file.exists(dashboard_files)]
if (length(missing_dashboard_files) > 0) {
  stop(
    paste0(
      "Dashboard preparation did not create:\n  - ",
      paste(missing_dashboard_files, collapse = "\n  - "),
      "\n\nRender the Quarto analysis first, then rerun the app."
    ),
    call. = FALSE
  )
}

community <- read_csv(dashboard_files[["community"]], show_col_types=FALSE) |> drop_excluded_columns()
vax_needs <- read_csv(dashboard_files[["vaccine_needs"]], show_col_types=FALSE) |> drop_excluded_columns()
product_needs <- read_csv(dashboard_files[["product_needs"]], show_col_types=FALSE) |> drop_excluded_columns()
patient <- read_csv(dashboard_files[["patient"]], show_col_types=FALSE) |> drop_excluded_columns()
quality <- read_csv(dashboard_files[["quality"]], show_col_types=FALSE) |> drop_excluded_columns()
if (nrow(patient)==0) stop("Dashboard data have no children. Run the analysis first.")

# ==============================================================================
# Display settings and helpers
# ==============================================================================

region_choices <- c("American Samoa", sort(unique(patient$island_atoll)))

age_choices <- c(
  "2-3 months" = "2_3",
  "4-5 months" = "4_5",
  "6-11 months" = "6_11",
  "12-18 months" = "12_18",
  "19-35 months" = "19_35",
  "3 years" = "3_years",
  "4 years" = "4_years",
  "5 years" = "5_years",
  "6 years" = "6_years",
  "2-59 months" = "2_59",
  "2-83 months" = "2_83",
  "4-6 years" = "4_6_years"
)

age_ranges <- list(
  "2_3" = c(2, 3),
  "4_5" = c(4, 5),
  "6_11" = c(6, 11),
  "12_18" = c(12, 18),
  "19_35" = c(19, 35),
  "3_years" = c(36, 47),
  "4_years" = c(48, 59),
  "5_years" = c(60, 71),
  "6_years" = c(72, 83),
  "2_59" = c(2, 59),
  "2_83" = c(2, 83),
  "4_6_years" = c(48, 83)
)

age_labels <- setNames(names(age_choices), unname(age_choices))

coverage_indicator_choices <- c(
  "DTaP UTD" = "dtap_utd",
  "IPV UTD" = "ipv_utd",
  "MMR UTD" = "mmr_utd",
  "Hib UTD" = "hib_utd",
  "HepB UTD" = "hepb_utd",
  "PCV UTD" = "pcv_utd",
  "Rotavirus UTD" = "rota_utd",
  "Varicella UTD" = "var_utd",
  "HepA UTD" = "hepa_utd"
)

coverage_definition_notes <- setNames(rep(paste0(
  "Each child is assessed at the first routine age anchor. DTaP dose 4 starts at 15 months; ",
  "MMR/varicella dose 2 at 48 months. Hib and PCV use age/product-specific catch-up and ",
  "exclude healthy children aged 60+ months from eligible denominators. HepA dose 2 uses ",
  "a six-month interval. Core UTD includes DTaP, IPV, MMR, Hib, HepB, PCV, varicella, and ",
  "HepA; rotavirus is shown separately. Not UTD and eligible for a dose today differ."
),length(age_choices)),unname(age_choices))

filter_age <- function(data, age_group) {
  range <- age_ranges[[age_group]]
  data |>
    filter(age_months >= range[[1]], age_months <= range[[2]])
}

geo_inputs <- function(prefix) {
  tagList(
    selectInput(paste0(prefix,"_region"), "Island/Atoll", region_choices, selected="American Samoa"),
    selectInput(paste0(prefix,"_district"), "District", c("All Districts",sort(unique(patient$district)))),
    selectInput(paste0(prefix,"_county"), "County", c("All Counties",sort(unique(patient$county)))),
    selectInput(paste0(prefix,"_village"), "Village", c("All Villages",sort(unique(patient$village))))
  )
}
filter_geo <- function(data,prefix,input) {
  keys<-c(region="island_atoll",district="district",county="county",village="village")
  alls<-c(region="American Samoa",district="All Districts",county="All Counties",village="All Villages")
  for (level in names(keys)) {
    value<-input[[paste0(prefix,"_",level)]]
    if(!is.null(value) && value!=alls[[level]]) data<-data[data[[keys[[level]]]]==value,,drop=FALSE]
  }
  data
}

priority_levels <- c("High", "Moderate", "Low")
priority_colors <- c("#E03F4F", "#F8C463", "#81912F")
pal_priority <- colorFactor(
  palette = priority_colors,
  levels = priority_levels,
  ordered = TRUE,
  na.color = "#BDBDBD"
)

coverage_colors <- c("#E03F4F","#F8C463","#69B34C")
coverage_legend_labels <- c("<65%","65-<85%",">=85%")
coverage_color <- function(x) {
  case_when(is.na(x)~"#BDBDBD",x<65~coverage_colors[1],x<85~coverage_colors[2],TRUE~coverage_colors[3])
}

region_colors <- setNames(rep(c("#7A5195","#0056A6","#EF8354","#009688","#607D8B","#C49000"), length.out=length(region_choices)-1),region_choices[-1])

region_color <- function(x) {
  color <- unname(region_colors[as.character(x)])
  color[is.na(color)] <- "#6C757D"
  color
}

# Use an explicit, no-key basemap so maps render consistently after publishing.
# Keeping the tile URL here also avoids provider-registry changes between local
# and hosted versions of leaflet/leaflet.providers.
dashboard_tile_url <- "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
dashboard_tile_attribution <- paste0(
  "&copy; <a href='https://www.openstreetmap.org/copyright' target='_blank'>",
  "OpenStreetMap</a> contributors"
)

dashboard_leaflet <- function(data) {
  leaflet(
    data,
    options = leafletOptions(
      preferCanvas = TRUE,
      zoomControl = TRUE
    )
  ) |>
    addTiles(
      urlTemplate = dashboard_tile_url,
      attribution = dashboard_tile_attribution,
      options = tileOptions(minZoom = 2, maxZoom = 19, noWrap = TRUE)
    )
}

fit_map_to_data <- function(map, data, default_zoom = 11) {
  distinct_locations <- data |>
    distinct(longitude, latitude)

  if (nrow(distinct_locations) == 1) {
    return(setView(map, data$longitude[[1]], data$latitude[[1]], zoom = default_zoom))
  }

  fitBounds(
    map,
    lng1 = min(data$longitude, na.rm = TRUE),
    lat1 = min(data$latitude, na.rm = TRUE),
    lng2 = max(data$longitude, na.rm = TRUE),
    lat2 = max(data$latitude, na.rm = TRUE)
  )
}

planning_vaccine_order <- names(AS_VACCINE_NEED_MAP)
planning_product_order <- names(AS_PRODUCT_MAP)

# ==============================================================================
# User interface
# ==============================================================================

ui <- page_navbar(
  title = "American Samoa Coverage Planning",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#0056A6", navbar_bg = "#003E73"),
  header = tags$head(
    if (file.exists(styles_file)) tags$link(rel = "stylesheet", href = "styles.css")
  ),

  nav_panel(
    "Situation overview",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        geo_inputs("overview"),
        sliderInput("min_priority", "Minimum priority score", 0, 100, 0),
        checkboxInput("ready_only", "Only villages ready for mapping", FALSE),
        p(
          class = "small-note",
          paste0(
            "Priority score: 90% community patient risk, 1% inter-island access, ",
            "1% low-density access difficulty, 4% density-adjusted transmission ",
            "potential, and 4% child burden / census population size."
          )
        )
      ),
      layout_columns(
        value_box(
          title = "Children 2 - 83 months",
          value = textOutput("n_children"),
          showcase = icon("users"),
          class = "overview-value-box"
        ),
        value_box(
          title = "Not UTD",
          value = textOutput("n_not_utd"),
          showcase = icon("exclamation-triangle"),
          class = "overview-value-box"
        ),
        value_box(
          title = "High-priority villages",
          value = textOutput("n_high"),
          showcase = icon("map-marker"),
          class = "overview-value-box"
        ),
        value_box(
          title = "Children eligible for MMR",
          value = textOutput("n_mmr"),
          showcase = icon("shield"),
          class = "overview-value-box"
        ),
        col_widths = c(3, 3, 3, 3),
        class = "overview-metrics"
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Village priority map"),
          leafletOutput("overview_map", height = "430px")
        ),
        card(
          full_screen = TRUE,
          card_header("Vaccination coverage: up to date for age"),
          leafletOutput("overview_coverage_map", height = "430px")
        ),
        col_widths = c(6, 6)
      ),
      card(card_header("Ranked village priorities"), DTOutput("priority_table"))
    )
  ),

  nav_panel(
    "Vaccination coverage",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        geo_inputs("coverage"),
        selectInput("coverage_age_group", "Age group", choices = age_choices, selected = "2_59"),
        selectInput(
          "coverage_map_indicator",
          "Coverage map indicator",
          choices = coverage_indicator_choices,
          selected = "dtap_utd"
        )
      ),
      layout_columns(
        value_box(
          title = "Children in selected age group",
          value = textOutput("coverage_children"),
          showcase = icon("users"),
          class = "overview-value-box"
        ),
        value_box(
          title = "Percent UTD for age",
          value = textOutput("coverage_utd_percent"),
          showcase = icon("shield"),
          class = "overview-value-box"
        ),
        col_widths = c(6, 6)
      ),
      card(
        card_header("Vaccination coverage by indicator"),
        plotlyOutput("coverage_bar", height = "360px"),
        uiOutput("coverage_definition_note")
      ),
      card(
        full_screen = TRUE,
        card_header("Village vaccination coverage map"),
        p(class = "small-note map-note", "Marker size represents the eligible denominator for the selected vaccine."),
        leafletOutput("vaccination_coverage_map", height = "460px")
      )
    )
  ),

  nav_panel(
    "Vaccination planning",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        geo_inputs("planning"),
        selectInput("planning_age_group", "Age group", choices = age_choices, selected = "2_59"),
        p(
          class = "small-note",
          paste0(
            "The map and tables use the selected geography and age group. ",
            "Products reflect one eligible visit, not all remaining series doses."
          )
        )
      ),
      layout_columns(
        value_box(
          title = "Children selected",
          value = textOutput("planning_children"),
          showcase = icon("users"),
          class = "overview-value-box"
        ),
        value_box(
          title = "On reminder/recall",
          value = textOutput("planning_reminder"),
          showcase = icon("bell"),
          class = "overview-value-box"
        ),
        value_box(
          title = "Villages with reminder/recall",
          value = textOutput("planning_villages"),
          showcase = icon("map-marker"),
          class = "overview-value-box"
        ),
        col_widths = c(4, 4, 4)
      ),
      card(
        full_screen = TRUE,
        card_header("Reminder/recall by village"),
        p(class = "small-note map-note", "Marker size increases with the number of children on reminder/recall."),
        leafletOutput("planning_reminder_map", height = "460px")
      ),
      layout_columns(
        card(
          card_header("Children due by vaccine type"),
          DTOutput("planning_vaccine_table")
        ),
        card(
          card_header("Estimated vaccine products needed"),
          DTOutput("planning_product_table")
        ),
        col_widths = c(6, 6)
      )
    )
  ),

  nav_panel(
    "Community risk & access",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        selectizeInput(
          "community_id",
          "Village",
          choices = setNames(
            community$community_id,
            paste(community$island_atoll, community$district, community$county, community$village, sep = " - ")
          ),
          selected = community$community_id[[1]]
        )
      ),
      layout_columns(
        card(card_header("Village profile"), uiOutput("community_profile")),
        card(card_header("Priority-score components"), plotlyOutput("components_plot", height = "340px")),
        col_widths = c(5, 7)
      ),
      layout_columns(
        card(card_header("Children due by vaccine type"), DTOutput("community_vaccine_table")),
        card(card_header("Estimated vaccine products needed"), DTOutput("community_product_table")),
        col_widths = c(6, 6)
      )
    )
  ),

  nav_panel(
    "Data quality & methods",
    layout_columns(
      card(
        card_header("Geography QA"),
        DTOutput("quality_table")
      ),
      card(
        card_header("American Samoa analytic rules"),
        tags$ul(
          tags$li("Reminder/Recall City is linked to the official census village. Raw report County is ignored."),
          tags$li("When City or County cannot be resolved, roster clinic can identify a county; the child's village remains Unknown."),
          tags$li("Unresolved combined county/district labels remain in QA and are never split into duplicate children."),
          tags$li("DTaP dose 4 starts at 15 months; MMR and varicella dose 2 at 48 months."),
          tags$li("Hib/PCV use product- and age-specific catch-up for healthy children; those aged 60+ months are excluded from eligible denominators."),
          tags$li("HepA dose 2 is due after six calendar months. Rotavirus dose counts and age limits are product-specific."),
          tags$li("Product estimates reflect one eligible visit and locally selected stock options."),
          tags$li("Census demographics describe the reference village/county. IIS child counts are the vaccination denominators.")
        ),
        tags$p(
          tags$a(
            href = "https://downloads.aap.org/AAP/PDF/AAP-Immunization-Schedule.pdf",
            target = "_blank",
            "AAP 2026 Child and Adolescent Immunization Schedule"
          )
        )
      ),
      col_widths = c(7, 5)
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {
  for(prefix_value in c("overview","coverage","planning")) local({
    prefix<-prefix_value
    subset_for<-function(level) {
      x<-patient
      for(parent in head(c("region","district","county","village"),level-1)) {
        value<-input[[paste0(prefix,"_",parent)]]
        all<-c(region="American Samoa",district="All Districts",county="All Counties",village="All Villages")[[parent]]
        field<-if(parent=="region")"island_atoll" else parent
        if(!is.null(value) && value!=all)x<-x[x[[field]]==value,,drop=FALSE]
      }
      x
    }
    observeEvent(input[[paste0(prefix,"_region")]],{
      x<-subset_for(2)
      updateSelectInput(session,paste0(prefix,"_district"),choices=c("All Districts",sort(unique(x$district))),selected="All Districts")
    },ignoreInit=FALSE)
    observeEvent(list(input[[paste0(prefix,"_region")]],input[[paste0(prefix,"_district")]]),{
      x<-subset_for(3)
      updateSelectInput(session,paste0(prefix,"_county"),choices=c("All Counties",sort(unique(x$county))),selected="All Counties")
    },ignoreInit=FALSE)
    observeEvent(list(input[[paste0(prefix,"_region")]],input[[paste0(prefix,"_district")]],input[[paste0(prefix,"_county")]]),{
      x<-subset_for(4)
      updateSelectInput(session,paste0(prefix,"_village"),choices=c("All Villages",sort(unique(x$village))),selected="All Villages")
    },ignoreInit=FALSE)
  })

  overview_data <- reactive({
    x <- filter_geo(community,"overview",input) |> filter(priority_score >= input$min_priority)
    if (isTRUE(input$ready_only)) {
      x <- x |> filter(data_quality_flag == "Ready")
    }
    x
  })

  output$n_children <- renderText(format(sum(overview_data()$child_population, na.rm = TRUE), big.mark = ","))
  output$n_not_utd <- renderText(format(sum(overview_data()$children_not_utd, na.rm = TRUE), big.mark = ","))
  output$n_high <- renderText(n_distinct(overview_data()$village[
    overview_data()$priority_group == "High" & overview_data()$village != "Unknown"]))
  output$n_mmr <- renderText(format(sum(filter_geo(patient,"overview",input)$need_mmr[filter_geo(patient,"overview",input)$community_id %in% overview_data()$community_id], na.rm=TRUE),big.mark=","))

  output$overview_map <- renderLeaflet({
    x <- overview_data() |> filter(!is.na(latitude), !is.na(longitude))
    validate(need(nrow(x) > 0, "No mapped villages are available for this selection."))

    map <- dashboard_leaflet(x) |>
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~pmin(22, pmax(5, sqrt(child_population))),
        color = ~pal_priority(priority_group),
        fillColor = ~pal_priority(priority_group),
        fillOpacity = 0.82,
        opacity = 1,
        weight = 1,
        label = ~paste0(village, ": ", priority_group, " priority (", priority_score, ")"),
        popup = ~paste0(
          "<b>", village, "</b><br>",
          "Island/Atoll: ", island_atoll, "<br>",
          "District: ", district, "<br>County: ", county, "<br>",
          "Children: ", child_population, "<br>",
          "Not UTD: ", children_not_utd, "<br>",
          "Priority score: ", priority_score, "<br>",
          "Mapping status: ", data_quality_flag
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = priority_colors,
        labels = priority_levels,
        opacity = 0.9,
        title = "Village priority"
      )

    fit_map_to_data(map, x)
  })

  output$overview_coverage_map <- renderLeaflet({
    x <- overview_data() |> filter(!is.na(latitude), !is.na(longitude))
    validate(need(nrow(x) > 0, "No mapped villages are available for this selection."))

    map <- dashboard_leaflet(x) |>
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~pmin(22, pmax(5, sqrt(child_population))),
        color = ~coverage_color(utd_coverage),
        fillColor = ~coverage_color(utd_coverage),
        fillOpacity = 0.85,
        opacity = 1,
        weight = 1,
        label = ~paste0(village, ": ", round(utd_coverage, 1), "% UTD"),
        popup = ~paste0(
          "<b>", village, "</b><br>",
          "Island/Atoll: ", island_atoll, "<br>",
          "District: ", district, "<br>County: ", county, "<br>",
          "UTD for age: ", round(utd_coverage, 1), "%<br>",
          "Children not UTD: ", children_not_utd
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = coverage_colors,
        labels = coverage_legend_labels,
        opacity = 0.9,
        title = "UTD for age"
      )

    fit_map_to_data(map, x)
  })

  output$priority_table <- renderDT({
    datatable(
      overview_data() |>
        transmute(
          `Priority rank` = priority_rank,
          `Island/Atoll` = island_atoll,
          District = district,
          County = county,
          Village = village,
          Children = child_population,
          `Children not UTD` = children_not_utd,
          `UTD coverage (%)` = round(utd_coverage, 1),
          `MMR coverage (%)` = round(mmr_coverage, 1),
          `Varicella coverage (%)` = round(var_coverage, 1),
          `HepA coverage (%)` = round(hepa_coverage, 1),
          `Priority score` = priority_score,
          `Priority group` = priority_group,
          `Data quality` = data_quality_flag
        ),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
    )
  })

  coverage_scope <- reactive({
    patient |>
      filter_geo("coverage",input) |>
      filter_age(input$coverage_age_group)
  })

  output$coverage_children <- renderText(
    format(n_distinct(coverage_scope()$patient_id), big.mark = ",")
  )

  output$coverage_utd_percent <- renderText({
    x <- coverage_scope()
    if (nrow(x) == 0) return("-")
    paste0(round(100 * mean(x$utd == 1, na.rm = TRUE), 1), "%")
  })

  output$coverage_definition_note <- renderUI({
    tags$p(class = "small-note", coverage_definition_notes[[input$coverage_age_group]])
  })

  output$coverage_bar <- renderPlotly({
    x <- coverage_scope()

    coverage_data <- lapply(names(coverage_indicator_choices), function(label) {
      variable <- coverage_indicator_choices[[label]]
      values <- x[[variable]]
      denominator <- sum(!is.na(values))

      tibble(
        indicator = label,
        denominator = denominator,
        coverage = if (denominator > 0) 100 * sum(values == 1, na.rm = TRUE) / denominator else NA_real_
      )
    }) |>
      bind_rows() |>
      filter(denominator > 0) |>
      mutate(indicator = factor(indicator, levels = rev(indicator)))

    validate(need(nrow(coverage_data) > 0, "No eligible coverage indicators are available."))

    plot_ly(
      coverage_data,
      x = ~coverage,
      y = ~indicator,
      type = "bar",
      orientation = "h",
      marker = list(color = "#0056A6"),
      text = ~paste0(round(coverage, 1), "% (n=", denominator, ")"),
      textposition = "auto",
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(title = "Coverage (%)", range = c(0, 100)),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 135, r = 20, t = 10, b = 50),
        showlegend = FALSE
      )
  })

  coverage_map_data <- reactive({
    selected_indicator <- input$coverage_map_indicator

    coverage_scope() |>
      mutate(selected_indicator = as.numeric(.data[[selected_indicator]])) |>
      group_by(community_id, island_atoll, district, county, region, village) |>
      summarise(
        eligible_children = sum(!is.na(selected_indicator)),
        children_utd = sum(selected_indicator == 1, na.rm = TRUE),
        coverage = if_else(
          eligible_children > 0,
          100 * children_utd / eligible_children,
          NA_real_
        ),
        latitude = median(latitude, na.rm = TRUE),
        longitude = median(longitude, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        latitude = if_else(is.nan(latitude), NA_real_, latitude),
        longitude = if_else(is.nan(longitude), NA_real_, longitude)
      )
  })

  output$vaccination_coverage_map <- renderLeaflet({
    x <- coverage_map_data() |>
      filter(eligible_children > 0, !is.na(latitude), !is.na(longitude))

    validate(need(nrow(x) > 0, "No geocoded coverage data are available for this selection."))

    indicator_label <- names(coverage_indicator_choices)[
      match(input$coverage_map_indicator, coverage_indicator_choices)
    ]

    map <- dashboard_leaflet(x) |>
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~pmin(22, pmax(5, sqrt(eligible_children))),
        color = ~coverage_color(coverage),
        fillColor = ~coverage_color(coverage),
        fillOpacity = 0.85,
        opacity = 1,
        weight = 1,
        label = ~paste0(village, ": ", round(coverage, 1), "%"),
        popup = ~paste0(
          "<b>", village, "</b><br>",
          "Island/Atoll: ", island_atoll, "<br>",
          "District: ", district, "<br>County: ", county, "<br>",
          indicator_label, ": ", round(coverage, 1), "%<br>",
          "Children UTD: ", children_utd, " of ", eligible_children
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = coverage_colors,
        labels = coverage_legend_labels,
        opacity = 0.9,
        title = indicator_label
      )

    fit_map_to_data(map, x)
  })

  planning_scope <- reactive({
    patient |>
      filter_geo("planning",input) |>
      filter_age(input$planning_age_group)
  })

  planning_reminder_data <- reactive({
    planning_scope() |>
      filter(onreminder == 1, !is.na(latitude), !is.na(longitude), village != "Unknown") |>
      group_by(community_id, island_atoll, district, county, region, village) |>
      summarise(
        children = n_distinct(patient_id),
        latitude = median(latitude, na.rm = TRUE),
        longitude = median(longitude, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(marker_radius = pmin(24, pmax(5, 3 + 2.2 * sqrt(children))))
  })

  output$planning_children <- renderText(
    format(n_distinct(planning_scope()$patient_id), big.mark = ",")
  )
  output$planning_reminder <- renderText(
    format(n_distinct(planning_scope()$patient_id[planning_scope()$onreminder == 1]), big.mark = ",")
  )
  output$planning_villages <- renderText(n_distinct(planning_reminder_data()$village))

  output$planning_reminder_map <- renderLeaflet({
    x <- planning_reminder_data()
    validate(need(nrow(x) > 0, "No geocoded reminder/recall records are available for this selection."))

    map <- dashboard_leaflet(x) |>
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~marker_radius,
        color = ~region_color(region),
        fillColor = ~region_color(region),
        fillOpacity = 0.75,
        opacity = 1,
        weight = 1.3,
        label = ~paste0(village, ": ", children, " children"),
        popup = ~paste0(
          "<b>", village, "</b><br>",
          "Island/Atoll: ", island_atoll, "<br>",
          "District: ", district, "<br>County: ", county, "<br>",
          "Children on reminder/recall: ", children
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = unname(region_colors),
        labels = names(region_colors),
        opacity = 0.9,
        title = "Island/Atoll"
      )

    fit_map_to_data(map, x)
  })

  output$planning_vaccine_table <- renderDT({
    x <- planning_scope() |>
      select(all_of(unname(AS_VACCINE_NEED_MAP))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "needed") |>
      mutate(Vaccine = names(AS_VACCINE_NEED_MAP)[match(variable, AS_VACCINE_NEED_MAP)]) |>
      group_by(Vaccine) |>
      summarise(`Children due` = sum(needed == 1, na.rm = TRUE), .groups = "drop") |>
      complete(Vaccine = planning_vaccine_order, fill = list(`Children due` = 0)) |>
      mutate(Vaccine = factor(Vaccine, levels = planning_vaccine_order)) |>
      arrange(Vaccine) |>
      mutate(Vaccine = as.character(Vaccine))

    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      class = "compact stripe",
      options = list(dom = "t", paging = FALSE, ordering = FALSE, columnDefs = list(list(className = "dt-center", targets = 1)))
    )
  })

  output$planning_product_table <- renderDT({
    x <- planning_scope() |>
      select(all_of(unname(AS_PRODUCT_MAP))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "needed") |>
      mutate(Product = names(AS_PRODUCT_MAP)[match(variable, AS_PRODUCT_MAP)]) |>
      group_by(Product) |>
      summarise(`Doses needed` = sum(needed == 1, na.rm = TRUE), .groups = "drop") |>
      complete(Product = planning_product_order, fill = list(`Doses needed` = 0)) |>
      mutate(Product = factor(Product, levels = planning_product_order)) |>
      arrange(Product) |>
      mutate(Product = as.character(Product))

    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      class = "compact stripe",
      options = list(dom = "t", paging = FALSE, ordering = FALSE, columnDefs = list(list(className = "dt-center", targets = 1)))
    )
  })

  selected_community <- reactive({
    community |> filter(community_id == input$community_id) |> slice(1)
  })

  output$community_profile <- renderUI({
    x <- selected_community()
    req(nrow(x) == 1)

    tagList(
      h3(x$village),
      h5(paste(x$island_atoll,x$district,x$county,sep=" | ")),
      tags$hr(),
      tags$p(strong("Priority: "), x$priority_score, " (", x$priority_group, ")"),
      tags$p(strong("Children: "), x$child_population),
      tags$p(strong("Not UTD: "), x$children_not_utd, " (", round(x$proportion_not_utd, 1), "%)"),
      tags$p(strong("MMR coverage: "), round(x$mmr_coverage, 1), "%"),
      tags$p(strong("Varicella coverage: "), round(x$var_coverage, 1), "%"),
      tags$p(strong("HepA coverage: "), round(x$hepa_coverage, 1), "%"),
      tags$p(strong("Median months since vaccination among not UTD: "), round(x$median_months_since_vax, 1)),
      tags$p(strong("2020 Census population: "), x$population),
      tags$p(strong("2020 population under age 5: "), x$population_under5),
      tags$p(strong("Density (people / km²): "), round(x$population_density_km2,1)),
      tags$p(strong("Demographic basis: "), x$metric_basis),
      tags$p(strong("Coordinate basis: "), x$coordinate_basis),
      tags$p(strong("Mapping status: "), x$data_quality_flag)
    )
  })

  output$components_plot <- renderPlotly({
    x <- selected_community()
    req(nrow(x) == 1)

    data <- tibble(
      component = c(
        "Community patient risk",
        "Inter-island access",
        "Low-density access difficulty",
        "Density-adjusted transmission",
        "Child burden / census population"
      ),
      score = c(
        x$community_patient_risk_component,
        x$island_access_component,
        x$low_density_component_for_score,
        x$transmission_component,
        x$burden_component
      ),
      weight = c(0.90, 0.01, 0.01, 0.04, 0.04)
    ) |>
      mutate(component = factor(component, levels = rev(component)))

    plot_ly(
      data,
      x = ~score,
      y = ~component,
      type = "bar",
      orientation = "h",
      marker = list(color = "#0056A6"),
      text = ~paste0(
        round(score, 1), " / 100; weight ", round(weight * 100), "%"
      ),
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(title = "Component score (0-100)", range = c(0, 100)),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 225, r = 20, t = 10, b = 55),
        showlegend = FALSE
      )
  })

  output$community_vaccine_table <- renderDT({
    x <- patient |>
      filter(community_id == input$community_id) |>
      select(all_of(unname(AS_VACCINE_NEED_MAP))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "needed") |>
      mutate(Vaccine = names(AS_VACCINE_NEED_MAP)[
        match(variable, AS_VACCINE_NEED_MAP)
      ]) |>
      group_by(Vaccine) |>
      summarise(`Children due` = sum(needed == 1, na.rm = TRUE), .groups = "drop") |>
      complete(Vaccine = planning_vaccine_order, fill = list(`Children due` = 0)) |>
      mutate(Vaccine = factor(Vaccine, levels = planning_vaccine_order)) |>
      arrange(Vaccine) |>
      mutate(Vaccine = as.character(Vaccine))

    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      options = list(dom = "t", paging = FALSE, ordering = FALSE)
    )
  })

  output$community_product_table <- renderDT({
    x <- patient |>
      filter(community_id == input$community_id) |>
      select(all_of(unname(AS_PRODUCT_MAP))) |>
      pivot_longer(everything(), names_to = "variable", values_to = "needed") |>
      mutate(Product = names(AS_PRODUCT_MAP)[match(variable, AS_PRODUCT_MAP)]) |>
      group_by(Product) |>
      summarise(`Doses needed` = sum(needed == 1, na.rm = TRUE), .groups = "drop") |>
      complete(Product = planning_product_order, fill = list(`Doses needed` = 0)) |>
      mutate(Product = factor(Product, levels = planning_product_order)) |>
      arrange(Product) |>
      mutate(Product = as.character(Product))

    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      options = list(dom = "t", paging = FALSE, ordering = FALSE)
    )
  })

  output$quality_table <- renderDT({
    datatable(
      quality |>
        transmute(
          `Island/Atoll` = island_atoll,
          District = district,
          County = county,
          Village = village,
          Children = children,
          Latitude = round(latitude, 6),
          Longitude = round(longitude, 6),
          Status = coordinate_status
      ),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
