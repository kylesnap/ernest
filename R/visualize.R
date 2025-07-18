#' Plot the posterior distribution of an `ernest_run`
#'
#' Create a plot of the posterior distributions from a nested sampling run,
#' or trace the evolution of the discarded live points along the log prior
#' volume.
#'
#' @param x An [ernest_run] object.
#' @param ... Arguments passed to [as_draws_rvars()].
#' @param type Character string specifying the type of plot to create.
#' One of `"density"` or `"trace"`, case sensitive.
#' -`"density"`: Shows the posterior density of each distribution.
#' -`"trace"`: Shows the distribution of points along estimates of the log
#' prior volume.
#' @param vars A character vector of variable names to plot. Case sensitive.
#' @param plot Logical, whether to return a `ggplot` of the visualization, or
#' a `tibble` of the data used to create the plot.
#'
#' @returns Either a `ggplot` object if `plot = TRUE`, or a `tibble`.
#'
#' @note This method requires the `ggdist` package for plotting the posterior.
#' @seealso [plot()] for visualizing the evidence estimates from an `ernest_run`.
#' @srrstats {G2.3, G2.3a} Using `arg_match` to validate character input.
#' @srrstats {G2.3b} Explicitly mentions that `vars` is case sensitive.
#' @examples
#' # Load example run
#' library(ggdist)
#' data(ernest_run_example)
#'
#' # Plot posterior distributions of the parameters
#' visualize(ernest_run_example, type = "density")
#'
#' # Plot the trace of the radial coordinate in unit scale
#' visualize(
#'   ernest_run_example,
#'   type = "trace",
#'   vars = ".radial",
#'   units = "unit_cube",
#'   radial = TRUE
#' )
#' @method visualize ernest_run
#' @export
visualize.ernest_run <- function(
  x,
  ...,
  type = c("density", "trace"),
  vars = NULL,
  plot = TRUE
) {
  check_dots_used()
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

  if (!plot) {
    return(df)
  }

  df |>
    ggplot() +
    ggdist::stat_halfeye(mapping = aes(xdist = .data[[".dist"]])) +
    ggplot2::facet_wrap(~ .data[[".var"]], scales = "free_x") +
    scale_x_continuous("Value") +
    scale_y_continuous("Density")
}

visualize_trace_ <- function(draws, cols, log_vol, plot) {
  points <- map(draws[cols], posterior::draws_of)

  df <- tibble::tibble(
    ".var" = rep(names(cols), each = posterior::ndraws(draws)),
    ".point" = list_c(points),
    ".log_volume" = rep(log_vol, length(cols)),
    ".weight" = rep(stats::weights(draws), length(cols))
  )
  if (!plot) {
    return(df)
  }

  df |>
    ggplot(aes(
      x = .data[[".log_volume"]],
      y = .data[[".point"]],
      colour = .data[[".weight"]]
    )) +
    ggplot2::geom_point() +
    facet_grid(rows = ggplot2::vars(.data[[".var"]]), scales = "free_y") +
    scale_x_continuous("Log Volume") +
    scale_y_continuous("Value") +
    ggplot2::scale_colour_viridis_c("Weight")
}
