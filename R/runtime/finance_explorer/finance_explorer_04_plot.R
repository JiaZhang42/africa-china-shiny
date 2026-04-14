# Script: Finance Explorer Plotting
# Author: Jia Zhang
# Purpose: Aggregate assembled finance panels into plotted series and build
#   final ggplot objects from an inspectable plot-data stage.


## plot data ------------------------------------------------------------------

apply_extending_moving_average <- function(data, moving_average_n) {
  if (moving_average_n <= 1L) {
    return(data |>
      mutate(series_value_plot = series_value))
  }

  data |>
    group_by(source_group, host_group) |>
    arrange(year, .by_group = TRUE) |>
    mutate(
      series_value_plot = purrr::map_dbl(
        seq_along(series_value),
        \(i) {
          start <- max(1L, i - moving_average_n + 1L)
          window <- series_value[start:i]

          if (all(is.na(window))) {
            return(NA_real_)
          }

          mean(window, na.rm = TRUE)
        }
      )
    ) |>
    ungroup()
}

build_finance_plot_data <- function(request,
                                    context = load_finance_explorer_context(),
                                    panel = NULL) {
  if (is.null(panel)) {
    panel <- build_finance_panel(request, context = context)
  }

  request <- attr(panel, "finance_request")
  aggregation_fn <- if (request$scale == "none") sum_or_na else mean_or_na
  source_group_levels <- levels(panel$source_group) %||% unique(as.character(panel$source_group))
  host_group_levels <- levels(panel$host_group) %||% unique(as.character(panel$host_group))

  plot_data <- panel |>
    mutate(
      source_group = factor(as.character(source_group), levels = source_group_levels),
      host_group = factor(as.character(host_group), levels = host_group_levels)
    ) |>
    summarise(
      series_value = aggregation_fn(outcome_value),
      .by = c(source_group, host_group, year)
    ) |>
    complete(
      source_group = factor(source_group_levels, levels = source_group_levels),
      host_group = factor(host_group_levels, levels = host_group_levels),
      year = seq(request$year_min, request$year_max, by = 1L)
    ) |>
    apply_extending_moving_average(request$moving_average_n)

  attr(plot_data, "finance_request") <- request
  attr(plot_data, "axis_label") <- attr(panel, "axis_label")
  attr(plot_data, "recipe_label") <- attr(panel, "recipe_label")
  attr(plot_data, "component_descriptions") <- attr(panel, "component_descriptions")

  plot_data
}


## plotting -------------------------------------------------------------------

build_finance_plot <- function(request, context = load_finance_explorer_context()) {
  panel <- build_finance_panel(request, context = context)
  plot_data <- build_finance_plot_data(request, context = context, panel = panel)

  request <- attr(plot_data, "finance_request")
  axis_label <- attr(plot_data, "axis_label")
  recipe_label <- attr(plot_data, "recipe_label")
  component_descriptions <- attr(plot_data, "component_descriptions")
  host_group_levels <- levels(plot_data$host_group) %||% unique(as.character(plot_data$host_group))

  default_title <- if (!is.null(request$host_groups) && length(request$host_groups) == 1L) {
    names(request$host_groups)[[1]]
  } else {
    recipe_label
  }
  plot_title <- request$title %||% default_title
  plot_subtitle <- request$subtitle %||%
    if (length(host_group_levels) > 1L) "Host groups are shown in separate panels." else NULL
  plot_x_lab <- request$x_lab %||% NULL
  plot_y_lab <- request$y_lab %||% axis_label

  trim_note <- if (request$trim == "p99_by_year") "Trim: within-year p99." else "Trim: none."
  moving_average_note <- if (request$moving_average_n > 1L) {
    paste0("Moving average: extending ", request$moving_average_n, "-year window.")
  } else {
    "Moving average: none."
  }

  plot_caption <- request$caption %||% paste(
    paste("Components:", paste(component_descriptions, collapse = "; ")),
    paste0("Scale: ", request$scale, "."),
    trim_note,
    moving_average_note
  )

  plot_obj <- plot_data |>
    ggplot(aes(x = year, y = series_value_plot, color = source_group)) +
    geom_line(linewidth = if (request$simplified) 0.7 else 0.9) +
    geom_point(size = if (request$simplified) 0.9 else 1.4) +
    scale_color_manual(
      values = make_group_colors(levels(plot_data$source_group)),
      drop = FALSE,
      na.translate = FALSE
    ) +
    scale_x_continuous(
      breaks = if (request$simplified) {
        seq(request$year_min, request$year_max, 5)
      } else {
        seq(request$year_min, request$year_max, 2)
      }
    ) +
    labs(
      x = if (request$simplified) NULL else plot_x_lab,
      y = if (request$simplified) NULL else plot_y_lab,
      color = NULL,
      title = plot_title,
      subtitle = if (request$simplified) NULL else plot_subtitle,
      caption = if (request$simplified) NULL else wrap_caption(plot_caption)
    ) +
    theme_hierarchy(base_size = if (request$simplified) 8.5 else 11)

  if (!is.null(request$shock_years) && length(request$shock_years) > 0L) {
    plot_obj <- plot_obj +
      geom_vline(
        xintercept = request$shock_years,
        linetype = "dashed",
        linewidth = 0.4
      )
  }

  if (length(host_group_levels) > 1L) {
    plot_obj <- plot_obj +
      facet_wrap(vars(host_group))
  }

  if (request$simplified) {
    plot_obj <- plot_obj +
      theme(
        legend.position = "none",
        plot.caption = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = rel(1.0), margin = margin(6, 0, 4, 0)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.7)),
        plot.margin = margin(8, 8, 8, 8)
      )
  }

  plot_obj
}
