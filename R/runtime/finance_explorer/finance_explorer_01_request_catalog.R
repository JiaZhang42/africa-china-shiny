# Script: Finance Explorer Request Catalog
# Author: Jia Zhang
# Purpose: Define the finance explorer request interface, measure and recipe
#   catalogs, and lightweight user-facing help.


## request documentation ------------------------------------------------------

# A finance explorer request is a named list passed to build_finance_panel(),
# build_finance_plot_data(), or build_finance_plot(). The required field is
# recipe_id; the remaining fields are optional and defaulted in
# normalize_request().
# A "component" means one recipe ingredient, such as China AidData official
# finance or non-China FDI, before those ingredients are combined into a single
# analysis panel.
# Core fields:
# - recipe_id: recipe name from build_recipe_catalog().
# - component_overrides: named list keyed by component_id. Generic fields such
#   as measure_id, include_source_iso3, and exclude_source_iso3 live at the
#   top level. Dataset-specific fields such as spec_id or year_basis can be
#   written either at the top level or inside options = list(...). Use
#   finance_component_override_catalog(recipe_id) to inspect the valid fields
#   and allowed values for each component.
# - source_groups / host_groups: named lists mapping display labels to ISO3
#   vectors. Use "__REST__" to collect all remaining sources into one group.
# - year_min / year_max: inclusive time window for the request.
# - scale: one of none, gdp, investment, fixed_investment, govexp.
# - trim: one of none or p99_by_year.
# - moving_average_n: extending trailing window length used in plotting.
# - simplified: when TRUE, suppresses most labels for crowded small multiples.
# - shock_years: optional numeric vector of vertical reference lines.
# - filters: optional list of sample restrictions. Currently supported:
#   africa_host, high_corr_host, exclude_african_sources, require_host_gdp.
# - title / subtitle / caption: optional plot-label overrides.
finance_request_template <- function(recipe_id = "china_aid_vs_rest_fdi") {
  recipe <- build_recipe_catalog() |>
    filter(.data$recipe_id == !!recipe_id)

  if (nrow(recipe) != 1L) {
    stop("Unknown recipe_id: ", recipe_id)
  }

  default_component_overrides <- recipe$components[[1]] |>
    purrr::map("options")

  names(default_component_overrides) <- recipe$components[[1]] |>
    purrr::map_chr("component_id")

  default_component_overrides <- purrr::keep(
    default_component_overrides,
    \(x) length(x) > 0L
  )

  list(
    recipe_id = recipe_id,
    component_overrides = default_component_overrides,
    source_groups = recipe$default_source_groups[[1]],
    host_groups = list(
      "Selected hosts" = c("NGA", "KEN")
    ),
    year_min = 2000L,
    year_max = 2023L,
    scale = "investment",
    trim = "none",
    moving_average_n = 1L,
    simplified = FALSE,
    shock_years = c(2003L, 2010L, 2015L),
    filters = list(
      africa_host = TRUE,
      high_corr_host = TRUE,
      exclude_african_sources = TRUE,
      require_host_gdp = TRUE
    ),
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )
}

finance_request_field_catalog <- function() {
  tibble(
    field = c(
      "recipe_id",
      "component_overrides",
      "source_groups",
      "host_groups",
      "year_min",
      "year_max",
      "scale",
      "trim",
      "moving_average_n",
      "simplified",
      "shock_years",
      "filters",
      "title / subtitle / caption"
    ),
    required = c(
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    ),
    options = c(
      "recipe id from build_recipe_catalog()",
      paste(
        "named list by component_id;",
        "supports generic component fields such as measure_id, include_source_iso3,",
        "exclude_source_iso3, plus dataset-specific options such as spec_id and year_basis;"
      ),
      "named list of labels -> ISO3 vector or '__REST__'",
      "named list of labels -> host ISO3 vector",
      "integer year",
      "integer year",
      "none, gdp, investment, fixed_investment, govexp",
      "none, p99_by_year",
      "positive integer",
      "TRUE / FALSE",
      "numeric vector or NULL",
      "africa_host, high_corr_host, exclude_african_sources, require_host_gdp",
      "character or NULL"
    ),
    notes = c(
      "drives which components are assembled",
      paste(
        "dataset-specific keys may be written at the top level or inside options = list(...);",
        "use finance_component_override_catalog(recipe_id) to inspect valid fields and choices"
      ),
      "defaults come from the selected recipe",
      "if omitted, all retained hosts are pooled into one group",
      "defaults to context minimum year",
      "defaults to context maximum year",
      "scaling is applied after the selected components are assembled",
      "trimming is applied on the assembled selected sample",
      "used only by build_finance_plot()",
      "for dense faceted layouts",
      "used by build_finance_plot()",
      "sample restrictions applied after joins to host controls",
      "plot-label overrides"
    )
  )
}

format_override_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }

  if (length(x) == 0L) {
    return("list()")
  }

  paste(as.character(x), collapse = ", ")
}

component_generic_override_catalog <- function(component, context) {
  allowed_measure_ids <- context$measure_catalog |>
    filter(dataset_id == component$dataset_id) |>
    pull(measure_id) |>
    unique() |>
    sort()

  tibble(
    field_name = c("measure_id", "include_source_iso3", "exclude_source_iso3"),
    field_scope = "component",
    default_value_raw = list(
      component$measure_id,
      component$include_source_iso3,
      component$exclude_source_iso3
    ),
    allowed_values_raw = list(
      allowed_measure_ids,
      NULL,
      NULL
    ),
    allowed_values = c(
      format_override_value(allowed_measure_ids),
      "character vector of source ISO3 codes or NULL",
      "character vector of source ISO3 codes or NULL"
    ),
    notes = c(
      "Switch to another measure within the same dataset.",
      "Keep only these source ISO3 codes in this component.",
      "Drop these source ISO3 codes from this component."
    )
  )
}

component_dataset_override_catalog <- function(component,
                                               context = load_finance_explorer_context()) {
  measure <- resolve_measure(component$measure_id, context)

  if (component$dataset_id == "aid") {
    return(
      tibble(
        field_name = c("spec_id", "year_basis"),
        field_scope = "dataset_option",
        default_value_raw = list(
          component$options$spec_id %||% "loan_grant",
          component$options$year_basis %||% measure$supported_time_bases[[1]][[1]]
        ),
        allowed_values_raw = list(
          sort(unique(context$aid_base$spec_id)),
          measure$supported_time_bases[[1]]
        ),
        allowed_values = c(
          format_override_value(sort(unique(context$aid_base$spec_id))),
          format_override_value(measure$supported_time_bases[[1]])
        ),
        notes = c(
          "AidData financing spec used in the host-year panel.",
          "AidData timing basis used to assign financing to year."
        )
      )
    )
  }

  tibble(
    field_name = character(),
    field_scope = character(),
    default_value_raw = list(),
    allowed_values_raw = list(),
    allowed_values = character(),
    notes = character()
  )
}

finance_recipe_catalog <- function() {
  build_recipe_catalog() |>
    transmute(
      recipe_id,
      recipe_label,
      component_count = purrr::map_int(components, length),
      component_ids = purrr::map_chr(
        components,
        \(x) paste(purrr::map_chr(x, "component_id"), collapse = ", ")
      ),
      datasets = purrr::map_chr(
        components,
        \(x) paste(unique(purrr::map_chr(x, "dataset_id")), collapse = ", ")
      )
    )
}

finance_component_override_catalog <- function(recipe_id = NULL,
                                               context = load_finance_explorer_context()) {
  recipe_catalog <- build_recipe_catalog()

  if (!is.null(recipe_id)) {
    recipe_catalog <- recipe_catalog |>
      filter(.data$recipe_id == !!recipe_id)

    if (nrow(recipe_catalog) != 1L) {
      stop("Unknown recipe_id: ", recipe_id)
    }
  }

  purrr::map_dfr(
    seq_len(nrow(recipe_catalog)),
    \(i) {
      recipe_row <- recipe_catalog[i, ]

      purrr::map_dfr(
        recipe_row$components[[1]],
        \(component) {
          bind_rows(
            component_generic_override_catalog(component, context),
            component_dataset_override_catalog(component, context)
          ) |>
            mutate(
              recipe_id = recipe_row$recipe_id[[1]],
              recipe_label = recipe_row$recipe_label[[1]],
              component_id = component$component_id,
              dataset_id = component$dataset_id,
              default_value = purrr::map_chr(default_value_raw, format_override_value),
              .before = 1
            ) |>
            select(
              recipe_id,
              recipe_label,
              component_id,
              dataset_id,
              field_name,
              field_scope,
              default_value,
              allowed_values,
              notes,
              default_value_raw,
              allowed_values_raw
            )
        }
      )
    }
  )
}

finance_explorer_help <- function() {
  api_catalog <- tibble(
    function_name = c(
      "finance_request_template()",
      "finance_request_field_catalog()",
      "finance_recipe_catalog()",
      "finance_component_override_catalog()",
      "load_finance_explorer_context()",
      "build_story_source_groups()",
      "build_finance_panel()",
      "build_finance_plot_data()",
      "rank_request_hosts()",
      "build_finance_plot()",
      "summarize_ids_coverage()",
      "build_finance_app_bundle()"
    ),
    returns = c(
      "request skeleton list",
      "field catalog tibble",
      "recipe catalog tibble",
      "component-override catalog tibble",
      "cached context list",
      "named source-group list",
      "row-level assembled panel",
      "aggregated series tibble used by plots",
      "host ranking tibble",
      "ggplot object",
      "IDS coverage summary tibble",
      "app bundle paths list"
    )
  )

  cat("Finance Explorer public API\n")
  purrr::walk2(
    api_catalog$function_name,
    api_catalog$returns,
    \(function_name, returns) {
      cat("- ", function_name, " -> ", returns, "\n", sep = "")
    }
  )
  cat("- Architecture guide: ", finance_explorer_report_path, "\n", sep = "")
  cat("\n")
  cat("Discovery workflow\n")
  cat("- finance_recipe_catalog() shows available recipe_id values and component ids.\n")
  cat("- finance_component_override_catalog(\"china_aid_vs_rest_fdi\") shows valid component_overrides fields and allowed values.\n")
  cat("- finance_request_template(\"china_aid_vs_rest_fdi\") returns a starter request with the recipe's default component overrides.\n")
  cat("- build_finance_app_bundle() exports the reduced app bundle, manifest, and runtime snapshot for the standalone Shiny repo.\n")

  invisible(api_catalog)
}


## registries -----------------------------------------------------------------

build_ids_measure_catalog <- function() {
  tibble(
    measure_id = c(
      "ids_ppg_bilateral_disbursement",
      "ids_ppg_bilateral_concessional_disbursement"
    ),
    dataset_id = "ids",
    measure_label = c(
      "IDS sovereign bilateral PPG disbursement",
      "IDS sovereign bilateral concessional PPG disbursement"
    ),
    raw_value_var = c(
      "ppg_bilateral_disbursement",
      "ppg_bilateral_concessional_disbursement"
    ),
    unit_family = "flow",
    default_scale = "investment",
    supported_scales = list(
      c("none", "gdp", "investment", "fixed_investment", "govexp"),
      c("none", "gdp", "investment", "fixed_investment", "govexp")
    ),
    supported_time_bases = list("calendar_year", "calendar_year"),
    supports_trim = c(TRUE, TRUE),
    notes = c(
      paste(
        "World Bank IDS sovereign bilateral disbursement in current USD,",
        "normalized to USD millions."
      ),
      paste(
        "World Bank IDS sovereign bilateral concessional disbursement in current USD,",
        "normalized to USD millions."
      )
    )
  )
}

build_measure_catalog <- function() {
  bind_rows(
    tibble(
      measure_id = c(
        "fdi_outflow",
        "aid_official_finance"
      ),
      dataset_id = c("fdi", "aid"),
      measure_label = c(
        "UNCTAD bilateral FDI outflow",
        "AidData official finance"
      ),
      raw_value_var = c(
        "flow_usd_m",
        "amount_nominal_usd_m"
      ),
      unit_family = c("flow", "flow"),
      default_scale = c("investment", "investment"),
      supported_scales = list(
        c("none", "gdp", "investment", "fixed_investment", "govexp"),
        c("none", "gdp", "investment", "fixed_investment", "govexp")
      ),
      supported_time_bases = list(
        "calendar_year",
        c("commitment_year", "implementation_start_year")
      ),
      supports_trim = c(TRUE, TRUE),
      notes = c(
        "UNCTAD bilateral FDI outflows in current USD millions.",
        paste(
          "AidData host-year official finance in current USD millions.",
          "Use component overrides for spec_id and year_basis."
        )
      )
    ),
    build_ids_measure_catalog()
  )
}

build_recipe_catalog <- function() {
  tibble(
    recipe_id = c(
      "all_fdi_outflow",
      "china_aid_vs_rest_fdi",
      "all_ids_ppg_bilateral_disbursement"
    ),
    recipe_label = c(
      "All source countries using FDI outflows",
      "China AidData versus non-China FDI",
      "All source countries using IDS sovereign bilateral PPG disbursement"
    ),
    components = list(
      list(
        list(
          component_id = "all_fdi",
          dataset_id = "fdi",
          measure_id = "fdi_outflow",
          include_source_iso3 = NULL,
          exclude_source_iso3 = NULL,
          options = list()
        )
      ),
      list(
        list(
          component_id = "china_aid",
          dataset_id = "aid",
          measure_id = "aid_official_finance",
          include_source_iso3 = "CHN",
          exclude_source_iso3 = NULL,
          options = list(spec_id = "loan_grant", year_basis = "commitment_year")
        ),
        list(
          component_id = "rest_fdi",
          dataset_id = "fdi",
          measure_id = "fdi_outflow",
          include_source_iso3 = NULL,
          exclude_source_iso3 = "CHN",
          options = list()
        )
      ),
      list(
        list(
          component_id = "all_ids_disbursement",
          dataset_id = "ids",
          measure_id = "ids_ppg_bilateral_disbursement",
          include_source_iso3 = NULL,
          exclude_source_iso3 = NULL,
          options = list()
        )
      )
    ),
    default_source_groups = list(
      default_source_groups_common,
      default_source_groups_common,
      default_source_groups_common
    ),
    default_host_group_name = c(
      "Selected hosts",
      "Selected hosts",
      "Selected hosts"
    )
  )
}


## request helpers ------------------------------------------------------------

resolve_recipe <- function(recipe_id, context) {
  recipe <- context$recipe_catalog |>
    filter(.data$recipe_id == !!recipe_id)

  if (nrow(recipe) != 1L) {
    stop("Unknown recipe_id: ", recipe_id)
  }

  recipe
}

resolve_measure <- function(measure_id, context) {
  measure <- context$measure_catalog |>
    filter(.data$measure_id == !!measure_id)

  if (nrow(measure) != 1L) {
    stop("Unknown measure_id: ", measure_id)
  }

  measure
}

merge_component_spec <- function(component, component_overrides = NULL) {
  override <- component_overrides[[component$component_id]] %||% list()

  if (length(override) == 0L) {
    return(component)
  }

  if (is.null(names(override)) || any(names(override) == "")) {
    stop(
      "component_overrides for component ",
      component$component_id,
      " must be a named list."
    )
  }

  component_field_names <- c(
    "measure_id",
    "include_source_iso3",
    "exclude_source_iso3",
    "options"
  )

  dataset_option_fields <- switch(
    component$dataset_id,
    aid = c("spec_id", "year_basis"),
    character()
  )

  unknown_override_fields <- setdiff(
    names(override),
    c(component_field_names, dataset_option_fields)
  )

  if (length(unknown_override_fields) > 0L) {
    stop(
      "Unknown component_overrides field(s) for component ",
      component$component_id,
      ": ",
      paste(unknown_override_fields, collapse = ", "),
      ". Allowed fields: ",
      paste(c(component_field_names, dataset_option_fields), collapse = ", "),
      ". Run finance_component_override_catalog(\"",
      build_recipe_catalog() |>
        mutate(has_component = purrr::map_lgl(
          components,
          \(x) component$component_id %in% purrr::map_chr(x, "component_id")
        )) |>
        filter(has_component) |>
        pull(recipe_id) |>
        first(),
      "\") for valid choices."
    )
  }

  merged_component <- component

  component_top_level <- override[intersect(names(override), component_field_names)]
  dataset_top_level <- override[intersect(names(override), dataset_option_fields)]

  if ("options" %in% names(component_top_level)) {
    if (!is.list(component_top_level$options)) {
      stop(
        "component_overrides$options for component ",
        component$component_id,
        " must be a named list."
      )
    }

    if (length(component_top_level$options) > 0L &&
        (is.null(names(component_top_level$options)) ||
         any(names(component_top_level$options) == ""))) {
      stop(
        "component_overrides$options for component ",
        component$component_id,
        " must use named fields."
      )
    }

    unknown_option_fields <- setdiff(names(component_top_level$options), dataset_option_fields)

    if (length(unknown_option_fields) > 0L) {
      stop(
        "Unknown dataset-specific option field(s) for component ",
        component$component_id,
        ": ",
        paste(unknown_option_fields, collapse = ", "),
        ". Allowed dataset-specific fields: ",
        paste(dataset_option_fields, collapse = ", "),
        "."
      )
    }
  }

  merged_component <- utils::modifyList(
    merged_component,
    component_top_level[setdiff(names(component_top_level), "options")]
  )

  merged_component$options <- utils::modifyList(
    merged_component$options %||% list(),
    component_top_level$options %||% list()
  )

  merged_component$options <- utils::modifyList(
    merged_component$options %||% list(),
    dataset_top_level
  )

  merged_component
}

make_axis_label <- function(scale, measure_label) {
  scale_labels <- c(
    none = "Amount (USD millions)",
    gdp = "External financing / GDP_Host",
    investment = "External financing / Investment_Host",
    fixed_investment = "External financing / Fixed_Investment_Host",
    govexp = "External financing / Gov_Exp_Host"
  )

  if (scale == "none") {
    return(paste0(measure_label, " (USD millions)"))
  }

  scale_labels[[scale]]
}

describe_component <- function(component, context) {
  measure <- resolve_measure(component$measure_id, context)

  if (component$dataset_id == "aid") {
    spec_id <- component$options$spec_id %||% "loan_grant"
    year_basis <- component$options$year_basis %||% "commitment_year"
    return(paste0(
      measure$measure_label,
      " [spec: ",
      spec_id,
      "; time basis: ",
      year_basis,
      "]"
    ))
  }

  if (component$dataset_id == "ids") {
    return(paste0(measure$measure_label, " [sovereign bilateral creditors only]"))
  }

  measure$measure_label
}

normalize_request <- function(request, context) {
  if (is.null(request$recipe_id)) {
    stop("request$recipe_id is required.")
  }

  recipe <- resolve_recipe(request$recipe_id, context)
  scale <- request$scale %||% "investment"
  trim <- request$trim %||% "none"
  moving_average_n <- as.integer(request$moving_average_n %||% 1L)

  if (!scale %in% c("none", "gdp", "investment", "fixed_investment", "govexp")) {
    stop("Unsupported scale: ", scale)
  }
  if (!trim %in% c("none", "p99_by_year")) {
    stop("Unsupported trim: ", trim)
  }
  if (is.na(moving_average_n) || moving_average_n < 1L) {
    stop("moving_average_n must be a positive integer.")
  }

  request$year_min <- as.integer(request$year_min %||% context$analysis_year_min)
  request$year_max <- as.integer(request$year_max %||% context$analysis_year_max)
  request$scale <- scale
  request$trim <- trim
  request$moving_average_n <- moving_average_n
  request$simplified <- isTRUE(request$simplified)
  request$component_overrides <- request$component_overrides %||% list()
  request$source_groups <- request$source_groups %||% recipe$default_source_groups[[1]]
  request$host_groups <- request$host_groups %||% NULL
  request$filters <- request$filters %||% list()
  request$shock_years <- request$shock_years %||% NULL

  if (request$year_min > request$year_max) {
    stop("year_min must be less than or equal to year_max.")
  }

  request
}
