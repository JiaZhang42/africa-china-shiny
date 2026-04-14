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
host_group_presets <- manifest$supported_options$host_group_presets
host_choices <- manifest$supported_options$host_choices
source_choices <- manifest$supported_options$source_choices
recipe_catalog <- manifest$supported_options$recipe_catalog
override_catalog <- manifest$supported_options$component_override_catalog |>
  filter(ui_exposed)
story_group_names <- names(story_source_groups)

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

custom_source_name_id <- function(index) {
  paste0("custom_source_group_name_", index)
}

custom_source_members_id <- function(index) {
  paste0("custom_source_group_members_", index)
}

pretty_label <- function(x) {
  x |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_squish() |>
    stringr::str_to_title()
}

format_country_label <- function(country_name, iso3) {
  if_else(
    !is.na(country_name) & country_name != "",
    paste0(country_name, " (", iso3, ")"),
    iso3
  )
}

empty_to_null <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- stringr::str_trim(as.character(x))

  if (identical(x, "") || identical(x, "NULL")) {
    return(NULL)
  }

  x
}

compact_named_list <- function(x) {
  x |>
    purrr::keep(\(value) !is.null(value) && length(value) > 0L)
}

host_choices_prepared <- host_choices |>
  mutate(host_label = format_country_label(host_country, host_iso3)) |>
  arrange(host_label)
host_name_lookup <- stats::setNames(host_choices_prepared$host_label, host_choices_prepared$host_iso3)
host_choices_named <- stats::setNames(host_choices_prepared$host_iso3, host_choices_prepared$host_label)

source_choices_prepared <- source_choices |>
  mutate(source_label = format_country_label(source_country, source_iso3)) |>
  arrange(source_label)
source_choices_named <- stats::setNames(source_choices_prepared$source_iso3, source_choices_prepared$source_label)

host_group_preset_choices <- setNames(
  host_group_presets$preset_id,
  host_group_presets$preset_label
)

collect_component_overrides <- function(recipe_id, input, override_catalog) {
  override_catalog |>
    filter(recipe_id == !!recipe_id) |>
    mutate(
      selected_value = purrr::map2_chr(
        component_id,
        field_name,
        \(component_id, field_name) {
          input[[override_input_id(component_id, field_name)]] %||% NA_character_
        }
      )
    ) |>
    filter(!is.na(selected_value), selected_value != "") |>
    summarise(
      override_values = list(stats::setNames(as.list(selected_value), field_name)),
      .by = component_id
    ) |>
    deframe()
}

collect_custom_source_groups <- function(input, max_groups = 3L) {
  custom_groups <- vector("list", max_groups)

  for (index in seq_len(max_groups)) {
    group_name <- empty_to_null(input[[custom_source_name_id(index)]])
    group_members <- unique(input[[custom_source_members_id(index)]] %||% character())

    if (!is.null(group_name) && length(group_members) > 0L) {
      custom_groups[[index]] <- stats::setNames(list(group_members), group_name)
    }
  }

  compact_named_list(unlist(custom_groups, recursive = FALSE))
}

compose_source_groups <- function(selected_story_groups,
                                  custom_groups,
                                  story_source_groups = story_source_groups) {
  selected_story_groups <- intersect(story_group_names, selected_story_groups %||% character())
  source_groups <- list()

  if (length(custom_groups) > 0L) {
    source_groups <- c(source_groups, custom_groups)
  }

  for (group_name in setdiff(selected_story_groups, "Other")) {
    source_groups[[group_name]] <- story_source_groups[[group_name]]
  }

  if ("Other" %in% selected_story_groups) {
    source_groups[["Other"]] <- "__REST__"
  }

  if (length(source_groups) == 0L) {
    stop("Select at least one default source group or define a custom source group.")
  }

  if (anyDuplicated(names(source_groups)) > 0L) {
    stop("Source group names must be unique.")
  }

  source_groups
}

compose_host_groups <- function(host_mode,
                                input,
                                host_name_lookup,
                                host_choices,
                                host_group_presets) {
  if (identical(host_mode, "all")) {
    return(list("All African hosts" = host_choices$host_iso3))
  }

  if (identical(host_mode, "individual")) {
    selected_hosts <- unique(input$individual_hosts %||% head(host_choices$host_iso3, 1))

    if (length(selected_hosts) == 0L) {
      stop("Choose at least one individual host country.")
    }

    return(stats::setNames(as.list(selected_hosts), host_name_lookup[selected_hosts]))
  }

  if (identical(host_mode, "custom")) {
    group_name <- empty_to_null(input$custom_host_group_name) %||% "Custom host group"
    selected_hosts <- unique(input$custom_host_group_members %||% head(host_choices$host_iso3, 3))

    if (length(selected_hosts) == 0L) {
      stop("Choose at least one host country for the custom host group.")
    }

    return(stats::setNames(list(selected_hosts), group_name))
  }

  if (identical(host_mode, "preset")) {
    selected_presets <- input$selected_host_presets %||% host_group_presets$preset_id[[1]]

    if (length(selected_presets) == 0L) {
      stop("Choose at least one preset host group.")
    }

    preset_rows <- host_group_presets |>
      filter(preset_id %in% selected_presets) |>
      mutate(order = match(preset_id, selected_presets)) |>
      arrange(order)

    return(stats::setNames(preset_rows$host_iso3, preset_rows$preset_label))
  }

  stop("Unsupported host_mode: ", host_mode)
}

bundle_metadata_ui <- function(manifest, host_group_presets) {
  aid_override_rows <- manifest$supported_options$component_override_catalog |>
    filter(component_id == "china_aid", field_name %in% c("spec_id", "year_basis"))

  ids_override_rows <- manifest$supported_options$component_override_catalog |>
    filter(component_id == "all_ids_disbursement", field_name == "measure_id")

  tagList(
    div(
      class = "app-note",
      strong("Bundle version: "), manifest$bundle_version, tags$br(),
      strong("Source commit: "), substr(manifest$source_commit, 1, 12), tags$br(),
      strong("Export time: "), manifest$export_time_utc, tags$br(),
      strong("Coverage: "), manifest$coverage$year_min, "-", manifest$coverage$year_max,
      ", ", manifest$coverage$host_count, " African hosts, ",
      manifest$coverage$source_count, " source entities."
    ),
    tags$hr(),
    tags$p(class = "app-note", strong("Recipes in this bundle")),
    tags$ul(
      lapply(seq_len(nrow(manifest$supported_options$recipe_catalog)), function(index) {
        recipe_row <- manifest$supported_options$recipe_catalog[index, ]
        tags$li(
          tags$strong(recipe_row$recipe_label[[1]]), ": ",
          recipe_row$component_ids[[1]]
        )
      })
    ),
    tags$p(class = "app-note", strong("Exposed AidData choices")),
    tags$ul(
      lapply(seq_len(nrow(aid_override_rows)), function(index) {
        override_row <- aid_override_rows[index, ]
        tags$li(
          tags$strong(pretty_label(override_row$field_name[[1]])), ": ",
          override_row$allowed_values[[1]]
        )
      })
    ),
    tags$p(class = "app-note", strong("Exposed IDS measure choices")),
    tags$ul(
      lapply(ids_override_rows$allowed_values_raw[[1]], \(measure_id) {
        tags$li(measure_id)
      })
    ),
    tags$p(class = "app-note", strong("Preset host groups")),
    tags$ul(
      lapply(seq_len(nrow(host_group_presets)), function(index) {
        preset_row <- host_group_presets[index, ]
        preset_members <- purrr::map2_chr(
          preset_row$host_country[[1]],
          preset_row$host_iso3[[1]],
          format_country_label
        )

        tags$li(
          tags$strong(preset_row$preset_label[[1]]), ": ",
          paste(preset_members, collapse = ", ")
        )
      })
    )
  )
}

app_css <- "
.app-shell {max-width: 1920px; margin: 0 auto;}
.app-note {font-size: 0.92rem; color: #4C5B66; line-height: 1.35;}
.app-kicker {letter-spacing: 0.08em; text-transform: uppercase; font-weight: 700; color: #A6342A; font-size: 0.78rem;}
.control-label, .form-label {font-weight: 700; color: #27343D;}
.bslib-sidebar-layout > .sidebar {background: #EFE7DB;}
.card {border-radius: 18px; border: 1px solid #E1D7C9; box-shadow: 0 12px 28px rgba(31, 42, 51, 0.06);}
.app-card {height: 100%; display: flex; flex-direction: column;}
.app-card > .card-body {flex: 1 1 auto; overflow: auto;}
.request-json-pre pre {font-family: 'IBM Plex Mono', monospace; font-size: 0.82rem; white-space: pre-wrap; word-break: break-word; margin: 0;}
.content-scroll {overflow-x: auto; overflow-y: visible;}
.main-grid {display: grid; grid-template-columns: minmax(680px, 1fr) minmax(340px, 400px); gap: 1rem; align-items: start; min-width: 1040px;}
.app-pane {display: flex; flex-direction: column; min-width: 0;}
.plot-pane {min-width: 0;}
.request-pane {min-width: 340px;}
.plot-stage {resize: both; overflow: auto; min-width: 360px; min-height: 320px; width: 100%; height: min(72vh, 760px); border: 1px solid #E1D7C9; border-radius: 18px; background: #FFFFFF;}
.plot-stage .shiny-plot-output {display: block; width: 100% !important; height: 100% !important;}
@media (max-width: 1280px) {
  .main-grid {grid-template-columns: minmax(620px, 1fr) minmax(320px, 360px); min-width: 960px;}
}
@media (max-width: 900px) {
  .content-scroll {overflow-x: visible;}
  .main-grid {grid-template-columns: 1fr; min-width: 0;}
  .request-pane {min-width: 0;}
  .plot-stage {width: 100%; min-width: 0; min-height: 280px; height: 52vh;}
}
"

ui <- page_sidebar(
  title = div(
    class = "app-shell",
    div(class = "app-kicker", "Internal Research App"),
    "Africa-China Finance Explorer"
  ),
  theme = theme_explorer,
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("
      function registerPlotStageObserver() {
        const plotStage = document.getElementById('plot_stage');
        if (!plotStage || plotStage.dataset.resizeObserverAttached === '1' || typeof ResizeObserver === 'undefined') {
          return;
        }

        const sendSize = function() {
          const rect = plotStage.getBoundingClientRect();
          const width = Math.max(320, Math.round(rect.width));
          const height = Math.max(280, Math.round(rect.height));
          if (window.Shiny) {
            Shiny.setInputValue('plot_stage_width', width, {priority: 'event'});
            Shiny.setInputValue('plot_stage_height', height, {priority: 'event'});
          }
        };

        const observer = new ResizeObserver(function() {
          sendSize();
        });

        observer.observe(plotStage);
        plotStage.dataset.resizeObserverAttached = '1';
        sendSize();
      }

      document.addEventListener('DOMContentLoaded', function() {
        registerPlotStageObserver();
        const mutationObserver = new MutationObserver(function() {
          registerPlotStageObserver();
        });
        mutationObserver.observe(document.body, {childList: true, subtree: true});
      });

      Shiny.addCustomMessageHandler('copy-text', function(message) {
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(message.text);
          return;
        }
        const textArea = document.createElement('textarea');
        textArea.value = message.text;
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        document.execCommand('copy');
        document.body.removeChild(textArea);
      });
    "))
  ),
  sidebar = sidebar(
    width = 380,
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
      step = 1,
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
    tags$hr(),
    checkboxGroupInput(
      "selected_story_source_groups",
      "Default source groups to show",
      choices = story_group_names,
      selected = story_group_names
    ),
    selectInput(
      "custom_source_group_count",
      "Additional custom source groups",
      choices = 0:3,
      selected = 0
    ),
    div(
      class = "app-note",
      "Custom source groups are applied before the default story groups. ",
      "`Other` collects whatever sources remain."
    ),
    uiOutput("custom_source_group_controls"),
    tags$hr(),
    radioButtons(
      "host_mode",
      "Host grouping",
      choices = c(
        "All African hosts" = "all",
        "Individual host country or countries" = "individual",
        "Self-defined host group" = "custom",
        "Preset host groups" = "preset"
      ),
      selected = "all"
    ),
    uiOutput("host_controls")
  ),
  fillable = TRUE,
  div(
    class = "app-shell",
    div(
      class = "content-scroll",
      div(
        class = "main-grid",
        div(
          class = "app-pane plot-pane",
          card(
            class = "app-card",
            full_screen = TRUE,
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center gap-2",
                span("Financing Series"),
                actionButton("show_plot_labs", "Edit labels", class = "btn btn-outline-secondary btn-sm")
              )
            ),
            card_body(
              div(
                id = "plot_stage",
                class = "plot-stage",
                plotOutput("finance_plot", width = "100%", height = "100%")
              ),
              div(
                class = "app-note mt-3",
                "Drag the bottom-right corner of the plot stage to change width and height."
              )
            )
          )
        ),
        div(
          class = "app-pane request-pane",
          card(
            class = "app-card",
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center gap-2",
                span("Current Request"),
                div(
                  class = "d-flex gap-2",
                  actionButton("show_bundle_metadata", "Bundle metadata", class = "btn btn-outline-secondary btn-sm"),
                  actionButton("copy_request", "Copy JSON", class = "btn btn-outline-primary btn-sm")
                )
              )
            ),
            card_body(
              div(class = "request-json-pre", verbatimTextOutput("request_preview"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  auto_lab_state <- reactiveValues(
    previous = NULL,
    title = "",
    subtitle = "",
    x_lab = "",
    y_lab = "",
    caption = ""
  )

  output$override_controls <- renderUI({
    rows <- override_catalog |>
      filter(recipe_id == input$recipe_id)

    if (nrow(rows) == 0L) {
      return(NULL)
    }

    tagList(
      lapply(seq_len(nrow(rows)), function(index) {
        row <- rows[index, ]
        selectInput(
          inputId = override_input_id(row$component_id[[1]], row$field_name[[1]]),
          label = paste(pretty_label(row$component_id[[1]]), "->", pretty_label(row$field_name[[1]])),
          choices = row$allowed_values_raw[[1]],
          selected = row$default_value[[1]]
        )
      })
    )
  })

  output$custom_source_group_controls <- renderUI({
    group_count <- as.integer(input$custom_source_group_count %||% 0L)

    if (group_count <= 0L) {
      return(NULL)
    }

    tagList(
      lapply(seq_len(group_count), function(index) {
        tagList(
          textInput(
            custom_source_name_id(index),
            paste("Custom source group", index, "name"),
            value = paste("Custom source group", index)
          ),
          selectizeInput(
            custom_source_members_id(index),
            paste("Custom source group", index, "members"),
            choices = source_choices_named,
            selected = NULL,
            multiple = TRUE,
            options = list(placeholder = "Choose one or more source countries")
          )
        )
      })
    )
  })

  output$host_controls <- renderUI({
    if (identical(input$host_mode, "all")) {
      return(
        div(
          class = "app-note",
          "All African hosts in the bundle are pooled into one host group."
        )
      )
    }

    if (identical(input$host_mode, "individual")) {
      return(
        tagList(
          div(
            class = "app-note",
            "Each selected host country becomes its own panel in the figure."
          ),
          selectizeInput(
            "individual_hosts",
            "Individual host countries",
            choices = host_choices_named,
            selected = head(host_choices_prepared$host_iso3, 1),
            multiple = TRUE,
            options = list(placeholder = "Choose one or more host countries")
          )
        )
      )
    }

    if (identical(input$host_mode, "custom")) {
      return(
        tagList(
          textInput(
            "custom_host_group_name",
            "Custom host group name",
            value = "Custom host group"
          ),
          selectizeInput(
            "custom_host_group_members",
            "Host countries in this group",
            choices = host_choices_named,
            selected = head(host_choices_prepared$host_iso3, 3),
            multiple = TRUE,
            options = list(placeholder = "Choose one or more host countries")
          )
        )
      )
    }

    tagList(
      div(
        class = "app-note",
        "Preset host groups are computed from the same bundled runtime used by the figure."
      ),
      checkboxGroupInput(
        "selected_host_presets",
        "Preset host groups",
        choices = host_group_preset_choices,
        selected = host_group_presets$preset_id[[1]]
      )
    )
  })

  base_request <- reactive({
    component_overrides <- collect_component_overrides(
      recipe_id = input$recipe_id,
      input = input,
      override_catalog = override_catalog
    )

    custom_source_groups <- collect_custom_source_groups(
      input = input,
      max_groups = 3L
    )

    source_groups <- compose_source_groups(
      selected_story_groups = input$selected_story_source_groups,
      custom_groups = custom_source_groups,
      story_source_groups = story_source_groups
    )

    host_groups <- compose_host_groups(
      host_mode = input$host_mode,
      input = input,
      host_name_lookup = host_name_lookup,
      host_choices = host_choices_prepared,
      host_group_presets = host_group_presets
    )

    list(
      recipe_id = input$recipe_id,
      component_overrides = component_overrides,
      source_groups = source_groups,
      host_groups = host_groups,
      year_min = as.integer(input$year_range[[1]]),
      year_max = as.integer(input$year_range[[2]]),
      scale = input$scale,
      trim = input$trim,
      moving_average_n = as.integer(input$moving_average_n),
      filters = list(
        africa_host = TRUE,
        high_corr_host = isTRUE(input$high_corr_host),
        exclude_african_sources = isTRUE(input$exclude_african_sources),
        require_host_gdp = isTRUE(input$require_host_gdp)
      ),
      title = NULL,
      subtitle = NULL,
      x_lab = NULL,
      y_lab = NULL,
      caption = NULL
    )
  })

  auto_plot_labs <- reactive({
    plot_obj <- build_finance_plot(base_request(), context = runtime_context)

    list(
      title = plot_obj$labels$title %||% "",
      subtitle = plot_obj$labels$subtitle %||% "",
      x_lab = plot_obj$labels$x %||% "",
      y_lab = plot_obj$labels$y %||% "",
      caption = plot_obj$labels$caption %||% ""
    )
  })

  observeEvent(auto_plot_labs(), {
    auto_labs <- auto_plot_labs()
    current_inputs <- isolate(list(
      title = auto_lab_state$title,
      subtitle = auto_lab_state$subtitle,
      x_lab = auto_lab_state$x_lab,
      y_lab = auto_lab_state$y_lab,
      caption = auto_lab_state$caption
    ))

    should_update <- is.null(auto_lab_state$previous) ||
      identical(current_inputs, auto_lab_state$previous)

    if (should_update) {
      auto_lab_state$title <- auto_labs$title
      auto_lab_state$subtitle <- auto_labs$subtitle
      auto_lab_state$x_lab <- auto_labs$x_lab
      auto_lab_state$y_lab <- auto_labs$y_lab
      auto_lab_state$caption <- auto_labs$caption
    }

    auto_lab_state$previous <- auto_labs
  }, ignoreNULL = FALSE)

  observeEvent(input$show_plot_labs, {
    showModal(
      modalDialog(
        title = "Edit plot labels",
        easyClose = TRUE,
        size = "l",
        textInput("plot_title_modal", "Title", value = auto_lab_state$title),
        textInput("plot_subtitle_modal", "Subtitle", value = auto_lab_state$subtitle),
        textInput("plot_x_lab_modal", "X-axis label", value = auto_lab_state$x_lab),
        textInput("plot_y_lab_modal", "Y-axis label", value = auto_lab_state$y_lab),
        textAreaInput("plot_caption_modal", "Caption", value = auto_lab_state$caption, rows = 5, width = "100%"),
        footer = tagList(
          actionButton("reset_plot_labs", "Reset labels to automatic defaults"),
          modalButton("Close")
        )
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$plot_title_modal, {
    auto_lab_state$title <- input$plot_title_modal %||% ""
  }, ignoreInit = TRUE)

  observeEvent(input$plot_subtitle_modal, {
    auto_lab_state$subtitle <- input$plot_subtitle_modal %||% ""
  }, ignoreInit = TRUE)

  observeEvent(input$plot_x_lab_modal, {
    auto_lab_state$x_lab <- input$plot_x_lab_modal %||% ""
  }, ignoreInit = TRUE)

  observeEvent(input$plot_y_lab_modal, {
    auto_lab_state$y_lab <- input$plot_y_lab_modal %||% ""
  }, ignoreInit = TRUE)

  observeEvent(input$plot_caption_modal, {
    auto_lab_state$caption <- input$plot_caption_modal %||% ""
  }, ignoreInit = TRUE)

  observeEvent(input$reset_plot_labs, {
    auto_labs <- auto_plot_labs()
    auto_lab_state$title <- auto_labs$title
    auto_lab_state$subtitle <- auto_labs$subtitle
    auto_lab_state$x_lab <- auto_labs$x_lab
    auto_lab_state$y_lab <- auto_labs$y_lab
    auto_lab_state$caption <- auto_labs$caption
    auto_lab_state$previous <- auto_labs

    updateTextInput(session, "plot_title_modal", value = auto_labs$title)
    updateTextInput(session, "plot_subtitle_modal", value = auto_labs$subtitle)
    updateTextInput(session, "plot_x_lab_modal", value = auto_labs$x_lab)
    updateTextInput(session, "plot_y_lab_modal", value = auto_labs$y_lab)
    updateTextAreaInput(session, "plot_caption_modal", value = auto_labs$caption)
  }, ignoreInit = TRUE)

  current_request <- reactive({
    request <- base_request()

    request$title <- empty_to_null(auto_lab_state$title)
    request$subtitle <- empty_to_null(auto_lab_state$subtitle)
    request$x_lab <- empty_to_null(auto_lab_state$x_lab)
    request$y_lab <- empty_to_null(auto_lab_state$y_lab)
    request$caption <- empty_to_null(auto_lab_state$caption)

    request
  })

  request_json <- reactive({
    jsonlite::toJSON(current_request(), pretty = TRUE, auto_unbox = TRUE, null = "null")
  })

  observeEvent(input$copy_request, {
    session$sendCustomMessage("copy-text", list(text = request_json()))
    showNotification("Current request JSON copied.", type = "message", duration = 2)
  })

  observeEvent(input$show_bundle_metadata, {
    showModal(
      modalDialog(
        title = "Bundle Metadata",
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close"),
        bundle_metadata_ui(manifest, host_group_presets)
      )
    )
  })

  output$finance_plot <- renderPlot({
    build_finance_plot(current_request(), context = runtime_context)
  },
  res = 110,
  width = function() {
    max(320, as.integer(input$plot_stage_width %||% 960L))
  },
  height = function() {
    max(280, as.integer(input$plot_stage_height %||% 640L))
  })

  output$request_preview <- renderText({
    request_json()
  })
}

app <- shinyApp(ui = ui, server = server)
