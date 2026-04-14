# Script: Finance Explorer Assembly And Ranking
# Author: Jia Zhang
# Purpose: Assemble component panels into analysis panels, apply request-level
#   transformations, and rank hosts under the corrected host-eligibility rule.


## component assembly ---------------------------------------------------------

validate_component_spec <- function(component_spec, context) {
  measure <- resolve_measure(component_spec$measure_id, context)

  if (!identical(measure$dataset_id[[1]], component_spec$dataset_id)) {
    stop(
      "measure_id ",
      component_spec$measure_id,
      " does not belong to dataset_id ",
      component_spec$dataset_id,
      "."
    )
  }

  supported_time_bases <- measure$supported_time_bases[[1]]
  requested_time_basis <- component_spec$options$year_basis %||% supported_time_bases[[1]]

  if (!requested_time_basis %in% supported_time_bases) {
    stop(
      "Unsupported time basis for component ",
      component_spec$component_id,
      ": ",
      requested_time_basis
    )
  }

  if (component_spec$dataset_id == "aid") {
    valid_spec_ids <- context$aid_base |>
      distinct(spec_id) |>
      pull(spec_id)

    requested_spec_id <- component_spec$options$spec_id %||% "loan_grant"
    if (!requested_spec_id %in% valid_spec_ids) {
      stop(
        "Unknown AidData spec_id for component ",
        component_spec$component_id,
        ": ",
        requested_spec_id
      )
    }
  }

  invisible(component_spec)
}

apply_source_filters <- function(panel, component) {
  if (!is.null(component$include_source_iso3)) {
    panel <- panel |>
      filter(source_iso3 %in% component$include_source_iso3)
  }

  if (!is.null(component$exclude_source_iso3)) {
    panel <- panel |>
      filter(!source_iso3 %in% component$exclude_source_iso3)
  }

  panel
}

build_component_panel <- function(component_spec, context = load_finance_explorer_context()) {
  validate_component_spec(component_spec, context)
  measure <- resolve_measure(component_spec$measure_id, context)

  panel <- switch(
    component_spec$dataset_id,
    fdi = context$fdi_base |>
      filter(measure_id == component_spec$measure_id),
    aid = context$aid_base |>
      filter(
        measure_id == component_spec$measure_id,
        spec_id == (component_spec$options$spec_id %||% "loan_grant"),
        time_basis == (component_spec$options$year_basis %||% "commitment_year")
      ),
    ids = context$ids_base |>
      filter(measure_id == component_spec$measure_id),
    stop("Unsupported dataset_id: ", component_spec$dataset_id)
  )

  panel |>
    apply_source_filters(component_spec) |>
    mutate(
      component_id = component_spec$component_id,
      unit_family = measure$unit_family,
      measure_label = measure$measure_label
    )
}

assign_group_labels <- function(data, var_name, groups, default_group_name) {
  if (is.null(groups) || length(groups) == 0L) {
    return(data |>
      mutate(group_label = factor(default_group_name, levels = default_group_name)))
  }

  if (is.null(names(groups)) || any(names(groups) == "")) {
    stop("Group definitions must be a named list.")
  }

  values <- data[[var_name]]
  group_label <- rep(NA_character_, length(values))
  rest_label <- NULL

  for (group_name in names(groups)) {
    group_codes <- groups[[group_name]]

    if (length(group_codes) == 1L && identical(group_codes, "__REST__")) {
      rest_label <- group_name
      next
    }

    group_label[is.na(group_label) & values %in% group_codes] <- group_name
  }

  if (!is.null(rest_label)) {
    group_label[is.na(group_label)] <- rest_label
  }

  data |>
    mutate(group_label = factor(group_label, levels = names(groups))) |>
    filter(!is.na(group_label))
}


## request transforms ---------------------------------------------------------

apply_request_filters <- function(panel, request) {
  filters <- request$filters %||% list()

  if (isTRUE(filters$africa_host)) {
    panel <- panel |>
      filter(africa_host == 1)
  }

  if (isTRUE(filters$high_corr_host)) {
    panel <- panel |>
      filter(high_corr_host == 1)
  }

  if (isTRUE(filters$exclude_african_sources)) {
    panel <- panel |>
      filter(!source_iso3 %in% africa_iso3)
  }

  if (isTRUE(filters$require_host_gdp)) {
    panel <- panel |>
      filter(!is.na(host_gdp), host_gdp > 0)
  }

  panel
}

compute_scaled_outcome <- function(panel, scale) {
  if (scale == "none") {
    return(panel$flow_usd_m)
  }

  if (scale == "gdp") {
    return(safe_host_ratio(panel$flow_usd_m, panel$host_gdp))
  }

  if (scale == "investment") {
    return(safe_host_ratio(panel$flow_usd_m, panel$host_investment))
  }

  if (scale == "fixed_investment") {
    return(safe_host_ratio(panel$flow_usd_m, panel$host_fixed_investment))
  }

  if (scale == "govexp") {
    return(safe_host_ratio(panel$flow_usd_m, panel$host_gov_expenditure))
  }

  stop("Unsupported scale: ", scale)
}

apply_trim <- function(panel, trim) {
  if (trim == "none") {
    return(panel |>
      mutate(outcome_value = outcome_value_raw))
  }

  if (trim != "p99_by_year") {
    stop("Unsupported trim option: ", trim)
  }

  panel |>
    group_by(year) |>
    mutate(
      outcome_value = pmin(
        outcome_value_raw,
        quantile(outcome_value_raw, 0.99, na.rm = TRUE)
      )
    ) |>
    ungroup()
}

validate_requested_scale <- function(component_panels, request, context) {
  scale_support <- component_panels |>
    distinct(measure_id) |>
    left_join(
      context$measure_catalog |>
        select(measure_id, supported_scales),
      by = "measure_id"
    ) |>
    mutate(scale_supported = purrr::map_lgl(supported_scales, \(x) request$scale %in% x))

  unsupported_measures <- scale_support |>
    filter(!scale_supported) |>
    pull(measure_id)

  if (length(unsupported_measures) > 0L) {
    stop(
      "Scale ",
      request$scale,
      " is not supported for measure(s): ",
      paste(unsupported_measures, collapse = ", ")
    )
  }

  invisible(request)
}


## assembled panel ------------------------------------------------------------

build_finance_panel <- function(request, context = load_finance_explorer_context()) {
  request <- normalize_request(request, context)
  recipe <- resolve_recipe(request$recipe_id, context)

  components <- purrr::map(
    recipe$components[[1]],
    merge_component_spec,
    component_overrides = request$component_overrides
  )

  component_panels <- purrr::map_dfr(components, build_component_panel, context = context)

  if (nrow(component_panels) == 0L) {
    stop("No rows remain after applying component filters.")
  }

  unit_families <- component_panels |>
    distinct(unit_family) |>
    pull(unit_family)

  if (length(unit_families) != 1L) {
    stop("Mixed unit families are not supported in a single plot request.")
  }

  validate_requested_scale(component_panels, request, context)

  panel <- component_panels |>
    left_join(context$cpi_host_panel, by = join_by(host_iso3, year)) |>
    left_join(context$host_controls, by = join_by(host_iso3, year)) |>
    mutate(africa_host = as.integer(host_iso3 %in% context$africa_iso3)) |>
    filter(year >= request$year_min, year <= request$year_max) |>
    apply_request_filters(request)

  panel <- assign_group_labels(
    panel,
    var_name = "source_iso3",
    groups = request$source_groups,
    default_group_name = "All sources"
  ) |>
    rename(source_group = group_label)

  panel <- if (is.null(request$host_groups) || length(request$host_groups) == 0L) {
    default_host_group_name <- recipe$default_host_group_name[[1]]
    panel |>
      mutate(host_group = factor(default_host_group_name, levels = default_host_group_name))
  } else {
    assign_group_labels(
      panel,
      var_name = "host_iso3",
      groups = request$host_groups,
      default_group_name = "Selected hosts"
    ) |>
      rename(host_group = group_label)
  }

  if (nrow(panel) == 0L) {
    stop("No rows remain after applying request filters and grouping.")
  }

  panel$outcome_value_raw <- compute_scaled_outcome(panel, request$scale)
  panel <- apply_trim(panel, request$trim)

  attr(panel, "finance_request") <- request
  attr(panel, "recipe_label") <- recipe$recipe_label
  attr(panel, "component_descriptions") <- purrr::map_chr(components, describe_component, context = context)
  attr(panel, "unit_family") <- unit_families[[1]]
  attr(panel, "axis_label") <- make_axis_label(
    request$scale,
    panel |>
      distinct(measure_label) |>
      pull(measure_label) |>
      paste(collapse = " / ")
  )

  panel
}


## ranking --------------------------------------------------------------------

qualify_rank_hosts <- function(panel,
                               host_qualification_var = NULL,
                               qualification_year_min,
                               qualification_year_max) {
  if (is.null(host_qualification_var)) {
    return(panel)
  }

  if (!host_qualification_var %in% names(panel)) {
    stop("host_qualification_var is not available in the assembled panel: ", host_qualification_var)
  }

  eligible_hosts <- panel |>
    filter(
      year >= qualification_year_min,
      year <= qualification_year_max,
      .data[[host_qualification_var]] == 1
    ) |>
    distinct(host_iso3)

  panel |>
    semi_join(eligible_hosts, by = "host_iso3")
}

rank_request_hosts <- function(request,
                               rank_source_groups = NULL,
                               rank_year_min = NULL,
                               rank_year_max = NULL,
                               ranking_var = "flow_usd_m",
                               top_n = 10L,
                               host_qualification_var = NULL,
                               qualification_year_min = NULL,
                               qualification_year_max = NULL,
                               context = load_finance_explorer_context()) {
  request_for_ranking <- request
  request_for_ranking$host_groups <- NULL
  request_for_ranking$filters <- request_for_ranking$filters %||% list()

  if (!is.null(host_qualification_var)) {
    request_for_ranking$filters[[host_qualification_var]] <- NULL
  }

  panel <- build_finance_panel(request_for_ranking, context = context)

  if (!ranking_var %in% names(panel)) {
    stop("ranking_var is not available in the assembled panel: ", ranking_var)
  }

  if (!is.null(rank_source_groups)) {
    panel <- panel |>
      filter(source_group %in% rank_source_groups)
  }

  rank_year_min <- as.integer(rank_year_min %||% min(panel$year, na.rm = TRUE))
  rank_year_max <- as.integer(rank_year_max %||% max(panel$year, na.rm = TRUE))
  qualification_year_min <- as.integer(qualification_year_min %||% rank_year_min)
  qualification_year_max <- as.integer(qualification_year_max %||% rank_year_max)
  top_n <- as.integer(top_n)

  if (is.na(top_n) || top_n < 1L) {
    stop("top_n must be a positive integer.")
  }

  host_lookup <- if ("host_country" %in% names(panel)) {
    panel |>
      summarise(host_country = first(host_country), .by = host_iso3)
  } else {
    context$host_lookup
  }

  panel |>
    qualify_rank_hosts(
      host_qualification_var = host_qualification_var,
      qualification_year_min = qualification_year_min,
      qualification_year_max = qualification_year_max
    ) |>
    filter(year >= rank_year_min, year <= rank_year_max) |>
    summarise(
      ranking_value = sum(.data[[ranking_var]], na.rm = TRUE),
      .by = host_iso3
    ) |>
    left_join(host_lookup, by = "host_iso3") |>
    select(host_iso3, host_country, ranking_value) |>
    arrange(desc(ranking_value), host_iso3) |>
    slice_head(n = top_n)
}
