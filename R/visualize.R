#' Draw the Points From a Nested Sampling Run
#'
#' Create a plot of the posterior distributions from a nested sampling run,
#' or trace the evolution of the discarded live points along the log prior
#' volume.
#'
#' @param x An `ernest_run` object.
#' @param ... Arguments passed to [as_draws_rvars()].
#' @param type The type of plot to create. Either a `"density"` plot, which shows
#' the posterior density of each distribution, or `"trace"`, which shows the
#' distribution of points along estimates of the log prior volume.
#' @param vars A character vector of variable names to plot.
#' @param plot Logical, whether to return a `ggplot` of the visualization, or
#' a `tibble` of the data used to create the plot.
#'
#' @return Either a `ggplot` object if `plot = TRUE`, or a `tibble`.
#'
#' @importFrom generics visualize
#' @export
#' @export visualize
visualize.ernest_run <- function(x, ..., type = c("density", "trace"), vars = NULL, plot = TRUE) {
  check_dots_used()
  check_installed("tidyselect", reason = "Select variables to be visualized.")
  if (plot) {
    check_installed("ggdist", reason = "Plot posterior distributions.")
  }
  type <- arg_match(type)

  draws <- as_draws_rvars(x, ...)
  vars <- vars %||% expr(tidyselect::everything())
  cols <- tidyselect::eval_select(vars, draws)
  cols <- cols[setdiff(names(cols), ".log_weight")]

  if (type == "density") {
    visualize_density_(draws, cols, plot)
  } else {
    visualize_trace_(draws, cols, x$log_volume, plot)
  }
}

visualize_density_ <- function(draws, cols, plot) {
  resamp <- posterior::resample_draws(draws)

  df <- tibble::tibble(
    ".var" = names(cols),
    ".dist" = list_c(resamp[cols])
  )

  if (!plot) return(df)

  df |>
    ggplot() +
    ggdist::stat_halfeye(mapping = aes(xdist = .dist)) +
    ggplot2::facet_wrap(~ .var, scales = "free_x") +
    scale_x_continuous("Value") +
    scale_y_continuous("Density")
}

visualize_trace_ <- function(draws, cols, log_vol, plot) {
  points <- map(draws[cols], posterior::draws_of)

  df <- tibble::tibble(
    ".var" = rep(names(cols), each = posterior::ndraws(draws)),
    ".point" = list_c(points),
    ".log_volume" = rep(log_vol, length(cols)),
    ".weight" = rep(weights(draws), length(cols))
  )
  if (!plot) return(df)

  df |>
    ggplot(aes(x = .log_volume, y = .point, colour = .weight)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(rows = ggplot2::vars(.var), scales = "free_y") +
    scale_x_continuous("Log Volume") +
    scale_y_continuous("Value") +
    ggplot2::scale_colour_viridis_c("Weight")
}
