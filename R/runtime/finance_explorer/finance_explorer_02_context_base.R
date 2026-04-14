# Script: Finance Explorer Context And Base Panels
# Author: Jia Zhang
# Purpose: Define constants, styling, lightweight utilities, and source-
#   specific normalized base panels plus the cached context loader.


## styling helpers ------------------------------------------------------------

wrap_caption <- function(text, width = 100) {
  stringr::str_wrap(text, width = width)
}

plot_font_family <- function(preferred, fallback = "sans") {
  if (interactive()) preferred else fallback
}

theme_hierarchy <- function(base_size = 12, dark_text = "#1A242F") {
  mid_text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <- monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]

  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(
        colour = mid_text,
        family = plot_font_family("Brandon Text"),
        lineheight = 1.1
      ),
      plot.title = element_text(
        colour = dark_text,
        family = plot_font_family("Enriqueta"),
        face = "bold",
        size = rel(1.2),
        margin = margin(12, 0, 8, 0)
      ),
      plot.subtitle = element_text(size = rel(1.0), margin = margin(4, 0, 0, 0)),
      axis.text.y = element_text(colour = light_text, size = rel(0.8)),
      axis.title.y = element_text(size = 12, margin = margin(0, 4, 0, 0)),
      axis.text.x = element_text(colour = mid_text, size = rel(1.0)),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.justification = 1,
      panel.grid = element_line(colour = "grey98"),
      plot.caption = element_text(
        size = rel(0.8),
        lineheight = 1.05,
        hjust = 0,
        margin = margin(8, 0, 0, 0)
      ),
      plot.caption.position = "plot",
      strip.text = element_text(
        colour = dark_text,
        family = plot_font_family("Enriqueta"),
        face = "bold",
        size = rel(0.95)
      ),
      plot.margin = margin(20, 20, 20, 20)
    )
}


## constants ------------------------------------------------------------------

report_colors <- c(
  oecd = "#1172B5",
  china = "#BC3C29",
  other = "#8C8C8C"
)

group_color_map <- c(
  "China" = report_colors[["china"]],
  "OECD ABC" = report_colors[["oecd"]],
  "US + UK + France" = report_colors[["oecd"]],
  "Other" = report_colors[["other"]]
)

default_source_groups_common <- list(
  "China" = "CHN",
  "US + UK + France" = c("USA", "GBR", "FRA"),
  "Other" = "__REST__"
)

not_states <- c(
  "Africa", "Asia", "Caribbean", "Central America", "Central Asia",
  "Developed economies", "Developing economies",
  "Eastern and South-Eastern Asia", "Europe", "European Union",
  "International Organisations", "Latin America and the Caribbean",
  "North Africa", "North America", "Oceania", "Other Africa", "Other Europe",
  "Other developed economies", "South America", "South and Central America",
  "Unspecified", "West Africa", "West Asia", "World",
  "Commonwealth of Independent States", "Transition Economies",
  "South, East and South-East Asia", "Private buying and selling of property",
  "Unspecified European Union excl UK", "Asia and Pacific",
  "South-East Europe", "Unspecified Central and Eastern Europe",
  "Unspecified CIS", "Unspecified Southeast Europe and CIS",
  "West Indies", "Belgium / Luxembourg",
  "Yugoslavia (former)", "Czechoslovakia (former)", "Serbia and Montenegro",
  "Soviet Union", "Zaire"
)

ofc_expanded <- c(
  "Netherlands", "Ireland", "Luxembourg", "Singapore", "Switzerland",
  "Hong Kong, China", "Macao, China", "Cayman Islands", "British Virgin Islands",
  "Bermuda", "Guernsey", "Jersey", "Isle of Man", "Panama", "Bahamas (the)",
  "Mauritius", "Seychelles"
)

oecd_abc_iso3 <- c(
  "ARG", "AUS", "AUT", "BEL", "BRA", "BGR", "CAN", "CHL", "CZE", "DNK",
  "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN",
  "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "KOR",
  "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
)

africa_iso3 <- c(
  "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD",
  "COM", "COG", "COD", "CIV", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH",
  "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG",
  "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA",
  "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TGO", "TUN",
  "TZA", "UGA", "ZMB", "ZWE"
)


## utility helpers ------------------------------------------------------------

safe_host_ratio <- function(flow_usd_m, denominator) {
  if_else(
    !is.na(denominator) & denominator > 0,
    100 * flow_usd_m * 1e6 / denominator,
    NA_real_
  )
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}

mean_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

make_group_colors <- function(group_names) {
  group_names <- unique(as.character(group_names))
  known_names <- intersect(group_names, names(group_color_map))
  known_colors <- group_color_map[known_names]
  unknown_names <- setdiff(group_names, names(group_color_map))

  if (length(unknown_names) == 0L) {
    return(known_colors[group_names])
  }

  extra_colors <- setNames(
    grDevices::hcl.colors(length(unknown_names), palette = "Dark 3"),
    unknown_names
  )

  c(known_colors, extra_colors)[group_names]
}

build_story_source_groups <- function(context = load_finance_explorer_context()) {
  source_universe <- c(
    context$fdi_base$source_iso3,
    context$aid_base$source_iso3,
    context$ids_base$source_iso3
  ) |>
    unique() |>
    sort()

  list(
    "China" = "CHN",
    "US + UK + France" = c("USA", "GBR", "FRA"),
    "Other" = setdiff(source_universe, c("CHN", oecd_abc_iso3))
  )
}


## source-specific normalized panels ------------------------------------------

build_cpi_host_panel <- function(cpi, analysis_year_min, analysis_year_max) {
  cpi |>
    transmute(
      host_iso3 = iso3,
      year = as.integer(year),
      cpi_match_year = year,
      cpi_match_score = cpi_score_100
    ) |>
    filter(!is.na(host_iso3), host_iso3 != "") |>
    distinct(host_iso3, year, .keep_all = TRUE) |>
    group_by(host_iso3) |>
    complete(year = seq(analysis_year_min, analysis_year_max, by = 1L)) |>
    arrange(year, .by_group = TRUE) |>
    fill(cpi_match_year, cpi_match_score, .direction = "up") |>
    ungroup() |>
    mutate(high_corr_host = if_else(!is.na(cpi_match_score), as.integer(cpi_match_score <= 50), NA_integer_))
}

build_host_controls <- function(wb) {
  wb_controls <- wb |>
    transmute(
      host_iso3 = country_code,
      host_country = country,
      year = as.integer(year),
      host_gdp = gdp,
      host_investment = gross_capital_formation,
      host_fixed_investment = gross_fixed_capital_formation,
      host_gov_expenditure = gov_final_consumption,
      host_export_pct = export_pct,
      political_stability,
      regulatory_quality,
      government_effectiveness,
      rule_of_law
    ) |>
    filter(!is.na(host_iso3), host_iso3 != "") |>
    group_by(host_iso3) |>
    arrange(year, .by_group = TRUE) |>
    mutate(host_gdp_growth = 100 * (log(host_gdp) - log(lag(host_gdp)))) |>
    ungroup()

  host_lookup <- wb_controls |>
    summarise(host_country = first(host_country), .by = host_iso3)

  list(
    host_controls = wb_controls,
    host_lookup = host_lookup
  )
}

build_fdi_base <- function(bilateral, country_unctad_linktable) {
  bilateral |>
    filter(!(country %in% not_states), !(partner %in% not_states)) |>
    filter(!(country %in% ofc_expanded), !(partner %in% ofc_expanded)) |>
    left_join(
      country_unctad_linktable |>
        transmute(country = country_unctad, source_iso3 = iso3),
      by = "country"
    ) |>
    left_join(
      country_unctad_linktable |>
        transmute(partner = country_unctad, host_iso3 = iso3),
      by = "partner"
    ) |>
    filter(!is.na(source_iso3), !is.na(host_iso3), source_iso3 != host_iso3) |>
    mutate(year = as.integer(year)) |>
    group_by(source_iso3, host_iso3) |>
    mutate(has_any_outflow = any(!is.na(outflow))) |>
    filter(has_any_outflow) |>
    ungroup() |>
    mutate(outflow = replace_na(outflow, 0)) |>
    filter(outflow >= 0) |>
    transmute(
      dataset_id = "fdi",
      measure_id = "fdi_outflow",
      source_iso3,
      source_country = country,
      host_iso3,
      host_country = partner,
      year,
      flow_usd_m = outflow,
      time_basis = "calendar_year",
      component_id = NA_character_,
      spec_id = NA_character_,
      spec_label = NA_character_
    )
}

build_aid_base <- function(aiddata_country_year_specs, host_lookup) {
  aiddata_country_year_specs |>
    left_join(host_lookup, by = "host_iso3") |>
    transmute(
      dataset_id = "aid",
      measure_id = "aid_official_finance",
      source_iso3 = "CHN",
      source_country = "China",
      host_iso3,
      host_country,
      year = as.integer(year),
      flow_usd_m = amount_nominal_usd_m,
      time_basis = year_basis,
      component_id = NA_character_,
      spec_id,
      spec_label
    )
}

build_ids_base <- function(ids_wide, measure_catalog, host_lookup) {
  ids_measure_lookup <- measure_catalog |>
    filter(dataset_id == "ids") |>
    transmute(measure_id, raw_value_var)

  ids_wide |>
    filter(
      !is.na(counterpart_iso3),
      counterpart_iso3 != "",
      country_code != counterpart_iso3
    ) |>
    semi_join(host_lookup, by = join_by(country_code == host_iso3)) |>
    pivot_longer(
      cols = all_of(unique(ids_measure_lookup$raw_value_var)),
      names_to = "raw_value_var",
      values_to = "value_usd"
    ) |>
    left_join(ids_measure_lookup, by = "raw_value_var") |>
    filter(!is.na(measure_id), !is.na(value_usd)) |>
    transmute(
      dataset_id = "ids",
      measure_id,
      source_iso3 = counterpart_iso3,
      source_country = counterpart,
      host_iso3 = country_code,
      host_country = country,
      year = as.integer(year),
      flow_usd_m = value_usd / 1e6,
      time_basis = "calendar_year",
      component_id = NA_character_,
      spec_id = NA_character_,
      spec_label = NA_character_
    )
}


## context loader -------------------------------------------------------------

load_finance_explorer_context <- function(refresh = FALSE) {
  if (!refresh && exists("context", envir = finance_explorer_env, inherits = FALSE)) {
    return(get("context", envir = finance_explorer_env, inherits = FALSE))
  }

  bilateral <- read_csv(
    fs::path(analysis_root, "data", "clean", "fdi", "bilateral_fdi_unctad_consolidated.csv"),
    show_col_types = FALSE
  )

  wb <- read_csv(
    fs::path(analysis_root, "data", "clean", "wb.csv"),
    show_col_types = FALSE
  )

  cpi <- read_csv(
    fs::path(analysis_root, "data", "clean", "cpi", "cpi_2000_2025.csv"),
    show_col_types = FALSE
  )

  country_unctad_linktable <- read_csv(
    fs::path(analysis_root, "data", "clean", "linktables", "country_unctad_linktable.csv"),
    show_col_types = FALSE
  )

  aiddata_country_year_specs <- read_csv(
    fs::path(analysis_root, "data", "clean", "aiddata", "aiddata_africa_country_year_specs.csv"),
    show_col_types = FALSE
  )

  ids_wide <- read_csv(
    fs::path(analysis_root, "data", "clean", "wb_ids", "wb_ids_consolidated.csv"),
    show_col_types = FALSE
  )

  analysis_year_min <- min(c(bilateral$year, cpi$year, ids_wide$year), na.rm = TRUE)
  analysis_year_max <- max(c(bilateral$year, cpi$year, ids_wide$year), na.rm = TRUE)

  measure_catalog <- build_measure_catalog()
  recipe_catalog <- build_recipe_catalog()
  controls <- build_host_controls(wb)
  cpi_host_panel <- build_cpi_host_panel(cpi, analysis_year_min, analysis_year_max)

  context <- list(
    measure_catalog = measure_catalog,
    recipe_catalog = recipe_catalog,
    host_controls = controls$host_controls,
    host_lookup = controls$host_lookup,
    cpi_host_panel = cpi_host_panel,
    fdi_base = build_fdi_base(bilateral, country_unctad_linktable),
    aid_base = build_aid_base(aiddata_country_year_specs, controls$host_lookup),
    ids_base = build_ids_base(ids_wide, measure_catalog, controls$host_lookup),
    africa_iso3 = africa_iso3,
    oecd_abc_iso3 = oecd_abc_iso3,
    analysis_year_min = analysis_year_min,
    analysis_year_max = analysis_year_max
  )

  assign("context", context, envir = finance_explorer_env)
  context
}

summarize_ids_coverage <- function(context = load_finance_explorer_context()) {
  africa_ids <- context$ids_base |>
    filter(host_iso3 %in% context$africa_iso3)

  tibble(
    rows_total = nrow(context$ids_base),
    africa_rows = nrow(africa_ids),
    africa_hosts = n_distinct(africa_ids$host_iso3),
    sovereign_creditors = n_distinct(africa_ids$source_iso3),
    africa_year_min = min(africa_ids$year, na.rm = TRUE),
    africa_year_max = max(africa_ids$year, na.rm = TRUE)
  )
}
