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
source(here::here("R", "seasonal_flu.R"), local = FALSE)
seasonal_flu <- load_as_seasonal_flu(
  here::here("data", "Analytic Code Output"),
  here::here("data", "Census_Flu_2026.xlsx"),
  here::here("data", "seasonal_flu_settings.csv")
)
flu_default_ymax <- if (is.null(seasonal_flu$error)) {
  max(1, ceiling(max(seasonal_flu$curves$coverage) * 1.1))
} else 10
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

community <- read_csv(dashboard_files[["community"]], show_col_types=FALSE) |>
  drop_excluded_columns() |> apply_community_priority_review() |> arrange(priority_rank,island_atoll,district,county,village)
vax_needs <- read_csv(dashboard_files[["vaccine_needs"]], show_col_types=FALSE) |> drop_excluded_columns()
product_needs <- read_csv(dashboard_files[["product_needs"]], show_col_types=FALSE) |> drop_excluded_columns()
patient <- read_csv(dashboard_files[["patient"]], show_col_types=FALSE) |> drop_excluded_columns()
quality <- read_csv(dashboard_files[["quality"]], show_col_types=FALSE) |> drop_excluded_columns()
quality <- quality |>
  left_join(community |> select(island_atoll,district,county,village,review_flag=data_quality_flag),
    by=c("island_atoll","district","county","village")) |>
  mutate(coordinate_status=coalesce(review_flag,coordinate_status)) |> select(-review_flag)
if (nrow(patient)==0) stop("Dashboard data have no children. Run the analysis first.")

# ==============================================================================
# Display settings and helpers
# ==============================================================================

dashboard_dates <- sort(unique(as.Date(patient$analysis_date)))
dashboard_as_of <- if (length(dashboard_dates) > 0) {
  paste(format(dashboard_dates, "%B %d, %Y"), collapse = "; ")
} else {
  "Unavailable"
}

region_choices <- c("American Samoa", sort(unique(patient$island_atoll)))

profile_village_choices <- function(x) {
  x <- x[order(x$village, x$county), , drop = FALSE]
  split_village <- duplicated(x$village) | duplicated(x$village, fromLast = TRUE)
  labels <- ifelse(split_village, paste0(x$village, " (", x$county, ")"), x$village)
  setNames(x$community_id, labels)
}
profile_initial <- community[1, ]
profile_selection <- function(data, id) {
  row <- data[data$community_id %in% id, , drop = FALSE]
  if (nrow(row) != 1) stop("Select one valid community.")
  list(island = row$island_atoll, district = row$district, community_id = row$community_id,
    districts = sort(unique(data$district[data$island_atoll == row$island_atoll])),
    villages = profile_village_choices(data[data$island_atoll == row$island_atoll &
      data$district == row$district, , drop = FALSE]))
}
priority_component_columns <- c("community_patient_risk_component", "transmission_component", "burden_component")
priority_component_labels <- c("Community patient risk", "Density-adjusted transmission", "Child burden / Census population")
priority_component_weights <- c(0.90, 0.05, 0.05)
score_label <- function(x) ifelse(is.na(x), "N/A", sprintf("%.1f", x))
profile_component_summary <- function(x, reference) {
  scores <- as.numeric(unlist(x[priority_component_columns], use.names = FALSE))
  data.frame(component = priority_component_labels, score = scores,
    weight = priority_component_weights, points = scores * priority_component_weights,
    median = vapply(reference[priority_component_columns], safe_median, numeric(1)), row.names = NULL)
}

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
  id = "dashboard_page",
  title = "American Samoa Coverage Planning",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#0056A6", navbar_bg = "#003E73"),
  # Keep the assessment date in the shared header so future pages inherit it.
  header = tagList(
    tags$head(
      if (file.exists(styles_file)) tags$link(rel = "stylesheet", href = "styles.css")
    ),
    uiOutput("dashboard_data_as_of")
  ),

  nav_panel(
    "Child Situation Overview",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        geo_inputs("overview"),
        sliderInput("min_priority", "Minimum priority score", 0, 100, 0),
        checkboxInput("ready_only", "Only villages ready for mapping", FALSE),
        p(
          class = "small-note",
          paste0(
            "Priority score: 90% community patient risk, ",
            "5% density-adjusted transmission potential, and ",
            "5% child burden / census population size."
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
    "Child Vaccination Coverage",
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
    "Seasonal Flu Coverage", value = "seasonal_flu",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        selectizeInput("flu_ages", "Age group", choices = AS_FLU_AGES,
          selected = AS_FLU_AGES, multiple = TRUE,
          options = list(plugins = list("remove_button"))),
        sliderInput("flu_ymax", "Coverage scale maximum (%)", min = 1,
          max = max(100, flu_default_ymax), value = flu_default_ymax, step = 1),
        p(class = "small-note", "Total combines the selected age groups. Adjust the scale to magnify low coverage. Scroll or drag on the chart to zoom; double-click to reset.")
      ),
      uiOutput("flu_reporting_period"),
      layout_columns(
        value_box(title = "Population size", value = textOutput("flu_population"),
          showcase = icon("users"), class = "overview-value-box"),
        value_box(title = "Influenza doses administered", value = textOutput("flu_doses"),
          showcase = icon("shield"), class = "overview-value-box"),
        col_widths = c(6, 6)
      ),
      card(
        full_screen = TRUE,
        card_header("American Samoa: cumulative seasonal influenza vaccination coverage"),
        plotlyOutput("flu_coverage", height = "440px")
      ),
      p(class = "small-note", paste0(
        "Coverage counts each person once after their first qualifying seasonal dose, divided by the Census population. ",
        "The dose card counts distinct seasonal doses. Ages follow the five age groups in the analytic extract. ",
        "Coverage measures receipt of at least one dose. Monthly results use completed months through the analysis cutoff. ",
        "Population denominators come from the AmSam sheet of Census_Flu_2026.xlsx. ",
        "Results cover American Samoa overall; no geographic breakdown is available."))
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
    "Community Profile (Child Data)",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        selectInput("profile_island", "Island/Atoll",
          choices = sort(unique(community$island_atoll)), selected = profile_initial$island_atoll),
        selectInput("profile_district", "District",
          choices = sort(unique(community$district[community$island_atoll == profile_initial$island_atoll])),
          selected = profile_initial$district),
        selectizeInput(
          "community_id",
          "Village",
          choices = profile_village_choices(community |>
            filter(island_atoll == profile_initial$island_atoll, district == profile_initial$district)),
          selected = profile_initial$community_id
        ),
        tags$hr(),
        div(
          class = "small-note",
          h5("What does the priority score mean?"),
          p("The score runs from 0 to 100. A higher score means the village needs more attention for vaccination planning."),
          tags$ul(
            tags$li(strong("90% — Children's vaccination needs: "),
              "How far behind children are on recommended vaccines, especially MMR, and how long it has been since their last vaccination."),
            tags$li(strong("5% — Potential for spread: "),
              "How closely people live together, combined with children's vaccination needs."),
            tags$li(strong("5% — Number of people needing attention: "),
              "The number of high-risk children and the village's Census population size.")
          ),
          p("The High, Moderate or Low label also considers vaccination coverage. A High village with 10 or fewer children is shown as Moderate and placed below other Moderate villages. Its score stays the same."),
          p("Unknown locations with more than 30 children are marked High so their data can be reviewed.")
        )
      ),
      layout_columns(
        card(card_header("Village profile"), uiOutput("community_profile")),
        card(
          card_header("Priority-score components"),
          tags$p(class = "small-note", "Raw scores use a shared 0–100 scale. Black markers show the median across all communities. The labels show each component's weighted contribution to the total score."),
          plotlyOutput("components_plot", height = "390px", fill = FALSE),
          tags$p(class = "small-note", "Patient risk contributes 90%, transmission 5%, and burden 5%. Burden combines high-risk child counts with Census village population size.")
        ),
        col_widths = c(5, 7)
      ),
      card(
        full_screen = TRUE,
        card_header("Compare village priority components"),
        tags$p(class = "small-note", "All communities in reviewed priority order. Cells show raw scores before weighting; darker means higher. Labels show rank and total priority score. Click a row to open its profile. The selected village is marked with an arrow. Missing scores are shown as N/A."),
        div(class = "priority-heatmap-scroll", uiOutput("priority_heatmap_ui"))
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
  output$dashboard_data_as_of <- renderUI({
    if (identical(input$dashboard_page, "seasonal_flu")) {
      label <- if (is.null(seasonal_flu$error)) format(seasonal_flu$through, "%B %d, %Y") else "Unavailable"
      tags$p(class = "data-as-of", paste("Data through", label))
    } else tags$p(class = "data-as-of", paste("Data as of", dashboard_as_of))
  })
  selected_flu_ages <- reactive(intersect(AS_FLU_AGES, input$flu_ages))
  output$flu_reporting_period <- renderUI({
    if (!is.null(seasonal_flu$error)) return(p(class = "small-note", seasonal_flu$error))
    year <- as.integer(format(seasonal_flu$season_start, "%Y"))
    p(class = "small-note", paste0(year, "–", year + 1,
      " season | Reporting through ", format(seasonal_flu$through, "%B %Y")))
  })
  output$flu_population <- renderText({
    if (!is.null(seasonal_flu$error)) return("Unavailable")
    if (!length(selected_flu_ages())) return("Select an age group")
    x <- seasonal_flu$population
    format(sum(x$population[x$age_group %in% selected_flu_ages()]), big.mark = ",", trim = TRUE)
  })
  output$flu_doses <- renderText({
    if (!is.null(seasonal_flu$error)) return("Unavailable")
    if (!length(selected_flu_ages())) return("Select an age group")
    x <- seasonal_flu$doses
    format(sum(x$doses[x$age_group %in% selected_flu_ages()]), big.mark = ",", trim = TRUE)
  })
  output$flu_coverage <- renderPlotly({
    validate(need(is.null(seasonal_flu$error), seasonal_flu$error),
      need(length(selected_flu_ages()) > 0, "Select at least one age group."))
    as_flu_plot(seasonal_flu, selected_flu_ages(), input$flu_ymax)
  })
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

  profile_target <- reactiveVal(NULL)
  observeEvent(input$profile_island, {
    districts <- sort(unique(community$district[community$island_atoll == input$profile_island]))
    req(length(districts) > 0)
    selected <- if (isTRUE(input$profile_district %in% districts)) input$profile_district else districts[[1]]
    target <- community[community$community_id %in% profile_target(), , drop = FALSE]
    if (nrow(target) == 1 && target$island_atoll == input$profile_island) selected <- target$district
    updateSelectInput(session, "profile_district", choices = districts, selected = selected)
  }, ignoreInit = FALSE)

  observeEvent(list(input$profile_island, input$profile_district), {
    req(input$profile_island, input$profile_district)
    x <- community |>
      filter(island_atoll == input$profile_island, district == input$profile_district)
    req(nrow(x) > 0)
    choices <- profile_village_choices(x)
    selected <- if (isTRUE(input$community_id %in% choices)) input$community_id else unname(choices[[1]])
    if (isTRUE(profile_target() %in% choices)) selected <- profile_target()
    freezeReactiveValue(input, "community_id")
    updateSelectizeInput(session, "community_id", choices = choices, selected = selected)
  }, ignoreInit = FALSE)

  observe({
    target <- profile_target()
    if (!is.null(target) && isTRUE(input$community_id == target)) {
      row <- community[community$community_id == target, , drop = FALSE]
      if (nrow(row) == 1 && isTRUE(input$profile_island == row$island_atoll) &&
          isTRUE(input$profile_district == row$district)) profile_target(NULL)
    }
  })

  selected_community <- reactive({
    req(input$profile_island, input$profile_district, input$community_id)
    x <- community |> filter(community_id == input$community_id,
      island_atoll == input$profile_island, district == input$profile_district)
    req(nrow(x) == 1)
    x
  })

  output$community_profile <- renderUI({
    x <- selected_community()
    req(nrow(x) == 1)

    tagList(
      h3(x$village),
      h5(paste(x$island_atoll,x$district,x$county,sep=" | ")),
      tags$hr(),
      tags$p(strong("Priority score: "), score_label(x$priority_score), " / 100"),
      tags$p(strong("Priority rank: "), x$priority_rank),
      tags$p(strong("Priority level: "), x$priority_group),
      if (isTRUE(x$small_denominator) && x$priority_group_before_review != x$priority_group)
        tags$p("Small denominator review: High changed to Moderate (10 or fewer children). Ranked below other Moderate villages, ordered by score."),
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
    d <- profile_component_summary(x, community)
    rows <- c(3, 2, 1)
    shapes <- annotations <- list()
    for (i in seq_along(rows)) {
      row <- rows[i]
      shapes[[length(shapes) + 1]] <- list(type = "rect", x0 = 0, x1 = 100,
        y0 = row - 0.10, y1 = row + 0.10, fillcolor = "#e8edf2",
        line = list(width = 0), layer = "below")
      if (!is.na(d$median[i])) shapes[[length(shapes) + 1]] <- list(type = "line",
        x0 = d$median[i], x1 = d$median[i], y0 = row - 0.17, y1 = row + 0.17,
        line = list(color = "#172b4d", width = 3))
      annotations[[length(annotations) + 1]] <- list(x = 0, y = row + 0.32,
        xanchor = "left", showarrow = FALSE,
        text = paste0("<b>", d$component[i], "</b>"), font = list(size = 12))
      annotations[[length(annotations) + 1]] <- list(x = 0, y = row - 0.31,
        xanchor = "left", showarrow = FALSE,
        text = paste0("Score ", score_label(d$score[i]), "/100 · Weight ", d$weight[i] * 100,
          "%<br>Adds ", score_label(d$points[i]), " points · Median ", score_label(d$median[i])),
        align = "left", font = list(size = 11, color = "#425466"))
    }
    plot_ly() |>
      add_bars(x = d$score, y = rows, orientation = "h", width = 0.20,
        marker = list(color = "#167d9a"),
        text = paste0(d$component, "<br>Score: ", score_label(d$score), "/100",
          "<br>All-community median: ", score_label(d$median),
          "<br>Weight: ", d$weight * 100, "%<br>Contribution: ", score_label(d$points), " points"),
        hoverinfo = "text") |>
      layout(shapes = shapes, annotations = annotations, showlegend = FALSE,
        xaxis = list(title = "Component score (before weighting)", range = c(0, 100),
          tickvals = seq(0, 100, 25), fixedrange = TRUE),
        yaxis = list(title = "", range = c(0.45, 3.55), showticklabels = FALSE,
          showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE),
        margin = list(l = 15, r = 15, t = 10, b = 55)) |>
      config(displayModeBar = FALSE)
  })

  priority_heatmap_data <- reactive({
    community |> arrange(priority_rank, island_atoll, district, county, village)
  })
  output$priority_heatmap_ui <- renderUI({
    plotlyOutput("priority_heatmap", height = paste0(max(440, 32 * nrow(community) + 120), "px"), fill = FALSE)
  })
  output$priority_heatmap <- renderPlotly({
    x <- priority_heatmap_data()
    req(nrow(x) > 0)
    scores <- as.matrix(x[priority_component_columns])
    split_village <- duplicated(x$village) | duplicated(x$village, fromLast = TRUE)
    village_labels <- ifelse(split_village, paste0(x$village, " (", x$county, ")"), x$village)
    selected_id <- if (is.null(input$community_id)) "" else input$community_id
    labels <- paste0(ifelse(x$community_id == selected_id, "→ ", ""),
      "#", x$priority_rank, "  ", village_labels, " · ", score_label(x$priority_score))
    hover <- matrix("", nrow = nrow(x), ncol = 3)
    annotations <- list()
    for (j in seq_len(3)) {
      for (i in seq_len(nrow(x))) {
        hover[i, j] <- paste0("<b>", htmltools::htmlEscape(x$village[i]), "</b><br>",
          htmltools::htmlEscape(paste(x$island_atoll[i], x$district[i], x$county[i], sep = " / ")),
          "<br>", priority_component_labels[j], "<br>Raw score: ", score_label(scores[i, j]),
          "/100<br>Weight: ", priority_component_weights[j] * 100,
          "%<br>Contribution: ", score_label(scores[i, j] * priority_component_weights[j]),
          " points<br>Total priority score: ", score_label(x$priority_score[i]),
          "<br>Priority level: ", x$priority_group[i])
        annotations[[length(annotations) + 1]] <- list(x = j, y = i,
          text = score_label(scores[i, j]), showarrow = FALSE,
          font = list(size = 12, color = if (!is.na(scores[i, j]) && scores[i, j] >= 60) "white" else "#172b4d"))
      }
    }
    plot_ly(x = 1:3, y = seq_len(nrow(x)), z = scores, type = "heatmap",
      source = "as_priority_components", zmin = 0, zmax = 100, xgap = 3, ygap = 3,
      colorscale = list(list(0, "#eff6fc"), list(0.5, "#65adc2"), list(1, "#084c70")),
      text = hover, hoverinfo = "text", hoverongaps = FALSE,
      colorbar = list(title = "Raw score", tickvals = seq(0, 100, 25))) |>
      layout(annotations = annotations,
        xaxis = list(title = "", side = "top", tickmode = "array", tickvals = 1:3,
          ticktext = c("Patient risk<br>Weight 90%", "Transmission<br>Weight 5%", "Child burden / population<br>Weight 5%"),
          fixedrange = TRUE),
        yaxis = list(title = "", autorange = "reversed", tickmode = "array",
          tickvals = seq_len(nrow(x)), ticktext = labels, automargin = TRUE, fixedrange = TRUE),
        margin = list(l = 280, r = 95, t = 80, b = 20), plot_bgcolor = "#f1f3f5") |>
      event_register("plotly_click") |> config(displayModeBar = FALSE)
  })

  observeEvent(event_data("plotly_click", source = "as_priority_components"), {
    clicked <- event_data("plotly_click", source = "as_priority_components")
    req(!is.null(clicked$y), length(clicked$y) > 0)
    index <- suppressWarnings(as.numeric(clicked$y[[1]]))
    x <- priority_heatmap_data()
    req(length(index) == 1, is.finite(index), index == floor(index), index >= 1, index <= nrow(x))
    target <- profile_selection(community, x$community_id[index])
    profile_target(target$community_id)
    updateSelectInput(session, "profile_island", selected = target$island)
    updateSelectInput(session, "profile_district",
      choices = target$districts, selected = target$district)
    updateSelectizeInput(session, "community_id", choices = target$villages, selected = target$community_id)
  })

  output$community_vaccine_table <- renderDT({
    x <- patient |>
      filter(community_id == selected_community()$community_id) |>
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
      filter(community_id == selected_community()$community_id) |>
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
