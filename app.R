suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(jsonlite)
  library(tidyverse)
})

source(file.path("R", "runtime", "finance_app_runtime.R"))

manifest <- readRDS(file.path("data", "finance_app_manifest.rds"))
runtime_context <- load_finance_explorer_context()
story_source_groups <- manifest$supported_options$source_group_presets$story
host_choices <- manifest$supported_options$host_choices
recipe_catalog <- manifest$supported_options$recipe_catalog
override_catalog <- manifest$supported_options$component_override_catalog |>
  filter(ui_exposed)

theme_explorer <- bs_theme(
  version = 5,
  bg = "#F6F1E8",
  fg = "#1F2A33",
  primary = "#A6342A",
  secondary = "#3E5C6E",
  base_font = font_google("IBM Plex Sans"),
  heading_font = font_google("Bitter"),
  code_font = font_google("IBM Plex Mono")
)

override_input_id <- function(component_id, field_name) {
  paste("override", component_id, field_name, sep = "__")
}

pretty_label <- function(x) {
  x |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_squish() |>
    stringr::str_to_title()
}

app_css <- "
.app-shell {max-width: 1680px; margin: 0 auto;}
.app-note {font-size: 0.92rem; color: #4C5B66; line-height: 1.35;}
.app-kicker {letter-spacing: 0.08em; text-transform: uppercase; font-weight: 700; color: #A6342A; font-size: 0.78rem;}
.control-label, .form-label {font-weight: 700; color: #27343D;}
.bslib-sidebar-layout > .sidebar {background: #EFE7DB;}
.card {border-radius: 18px; border: 1px solid #E1D7C9; box-shadow: 0 12px 28px rgba(31, 42, 51, 0.06);}
.mono-block pre {font-size: 0.82rem; white-space: pre-wrap;}
"

ui <- page_sidebar(
  title = div(
    class = "app-shell",
    div(class = "app-kicker", "Internal Research App"),
    "Africa-China Finance Explorer"
  ),
  theme = theme_explorer,
  tags$head(tags$style(HTML(app_css))),
  sidebar = sidebar(
    width = 340,
    div(
      class = "app-note",
      "This app is powered by a reduced release bundle exported from the research repo. ",
      "It does not rebuild raw or clean data at runtime."
    ),
    selectInput(
      "recipe_id",
      "Outcome recipe",
      choices = setNames(recipe_catalog$recipe_id, recipe_catalog$recipe_label),
      selected = manifest$ui_defaults$recipe_id
    ),
    uiOutput("override_controls"),
    sliderInput(
      "year_range",
      "Year range",
      min = manifest$coverage$year_min,
      max = manifest$coverage$year_max,
      value = c(manifest$coverage$year_min, manifest$coverage$year_max),
      sep = ""
    ),
    selectInput(
      "scale",
      "Scaling",
      choices = manifest$supported_options$scales,
      selected = manifest$ui_defaults$scale
    ),
    selectInput(
      "trim",
      "Trim rule",
      choices = manifest$supported_options$trims,
      selected = manifest$ui_defaults$trim
    ),
    sliderInput(
      "moving_average_n",
      "Extending moving average",
      min = manifest$ui_bounds$moving_average_n_min,
      max = manifest$ui_bounds$moving_average_n_max,
      value = manifest$ui_defaults$moving_average_n,
      step = 1
    ),
    checkboxInput(
      "high_corr_host",
      "Restrict to high-corruption hosts",
      value = manifest$ui_defaults$high_corr_host
    ),
    checkboxInput(
      "exclude_african_sources",
      "Exclude African source countries",
      value = manifest$ui_defaults$exclude_african_sources
    ),
    checkboxInput(
      "require_host_gdp",
      "Require valid host GDP",
      value = manifest$ui_defaults$require_host_gdp
    ),
    checkboxInput("use_all_hosts", "Use all African hosts", TRUE),
    uiOutput("host_controls")
  ),
  fillable = TRUE,
  div(
    class = "app-shell",
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header("Financing Series"),
        plotOutput("finance_plot", height = "680px")
      ),
      card(
        card_header("Bundle Metadata"),
        div(
          class = "app-note",
          strong("Bundle version: "), manifest$bundle_version, tags$br(),
          strong("Source commit: "), substr(manifest$source_commit, 1, 12), tags$br(),
          strong("Coverage: "), manifest$coverage$year_min, "-", manifest$coverage$year_max,
          ", ", manifest$coverage$host_count, " African hosts, ",
          manifest$coverage$source_count, " source entities."
        ),
        tags$hr(),
        card_header("Current Request"),
        div(class = "mono-block", verbatimTextOutput("request_preview"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$override_controls <- renderUI({
    rows <- override_catalog |>
      filter(recipe_id == input$recipe_id)

    if (nrow(rows) == 0L) {
      return(NULL)
    }

    tagList(
      lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, ]
        selectInput(
          inputId = override_input_id(row$component_id[[1]], row$field_name[[1]]),
          label = paste(pretty_label(row$component_id[[1]]), "->", pretty_label(row$field_name[[1]])),
          choices = row$allowed_values_raw[[1]],
          selected = row$default_value[[1]]
        )
      })
    )
  })

  output$host_controls <- renderUI({
    if (isTRUE(input$use_all_hosts)) {
      return(NULL)
    }

    selectizeInput(
      "selected_hosts",
      "African hosts",
      choices = setNames(host_choices$host_iso3, host_choices$host_country),
      selected = host_choices$host_iso3,
      multiple = TRUE,
      options = list(placeholder = "Choose one or more hosts")
    )
  })

  current_request <- reactive({
    selected_hosts <- if (isTRUE(input$use_all_hosts) || is.null(input$selected_hosts) || length(input$selected_hosts) == 0L) {
      host_choices$host_iso3
    } else {
      input$selected_hosts
    }

    rows <- override_catalog |>
      filter(recipe_id == input$recipe_id)

    component_overrides <- rows |>
      mutate(selected_value = purrr::map2_chr(
        component_id,
        field_name,
        \(component_id, field_name) {
          selected_value <- input[[override_input_id(component_id, field_name)]]

          if (is.null(selected_value)) {
            return(NA_character_)
          }

          selected_value
        }
      )) |>
      filter(!is.na(selected_value), selected_value != "") |>
      summarise(
        override_values = list(stats::setNames(as.list(selected_value), field_name)),
        .by = component_id
      ) |>
      deframe()

    list(
      recipe_id = input$recipe_id,
      component_overrides = component_overrides,
      source_groups = story_source_groups,
      host_groups = list("Selected hosts" = selected_hosts),
      year_min = input$year_range[[1]],
      year_max = input$year_range[[2]],
      scale = input$scale,
      trim = input$trim,
      moving_average_n = as.integer(input$moving_average_n),
      filters = list(
        africa_host = TRUE,
        high_corr_host = isTRUE(input$high_corr_host),
        exclude_african_sources = isTRUE(input$exclude_african_sources),
        require_host_gdp = isTRUE(input$require_host_gdp)
      )
    )
  })

  output$request_preview <- renderText({
    jsonlite::toJSON(current_request(), pretty = TRUE, auto_unbox = TRUE, null = "null")
  })

  output$finance_plot <- renderPlot({
    request <- current_request()
    build_finance_plot(request, context = runtime_context)
  }, res = 110)
}

app <- shinyApp(ui = ui, server = server)
