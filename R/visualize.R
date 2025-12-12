#' Plot the posterior distribution of an `ernest_run`
#'
#' Create a plot of the posterior distributions from a nested sampling run,
#' or trace the evolution of discarded live points along the log prior
#' volume.
#'
#' @param x An [ernest_run] object.
#' @param type Case-sensitive string. The type of plot to create:
#' * `"density"`: Shows the posterior density of each parameter.
#' * `"trace"`: Shows the distribution of points along estimates of the log
#' prior volume.
#' @param vars <[`tidy-select`][dplyr::dplyr_tidy_select]> Variables to
#' plot from the run. If `NULL`, all variables are plotted.
#' @param plot Logical. If `TRUE`, returns a `ggplot` of the visualisation;
#' if `FALSE`, returns a `tibble` of the data used to create the plot.
#' @inheritDotParams as_draws.ernest_run units radial
#'
#' @returns A `ggplot` object if `plot = TRUE`, otherwise a `tibble`.
#'
#' @seealso [plot()] for visualising evidence estimates from an
#'   `ernest_run`.
#'
#' @srrstats {G2.3, G2.3a, G2.3b} Uses arg_match() to ensure an informative
#' error message is provided when the user provides an invalid value for
#' `type`.
#' @srrstats {BS6.2, BS6.3} Plot method for posterior samples and sequences of
#' samples.
#'
#' @examples
#' # Load example run
#' library(ggdist)
#' data(example_run)
#'
#' # Plot posterior distributions of the parameters
#' visualize(example_run, type = "density")
#'
#' # Plot the trace of the radial coordinate in unit scale
#' visualize(
#'   example_run,
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
  rlang::check_installed(c("ggdist", "tidyselect"))
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

#' Visualize posterior density for selected variables
#'
#' Generates a tibble or ggplot of posterior densities for selected variables
#' from a nested sampling run.
#'
#' @param draws A draws_rvars object containing posterior samples.
#' @param cols Named integer vector of columns to visualize.
#' @param plot Logical. If TRUE, returns a ggplot; if FALSE, returns a tibble.
#'
#' @return A ggplot object or a tibble, depending on `plot`.
#' @noRd
visualize_density_ <- function(draws, cols, plot) {
  resamp <- posterior::resample_draws(draws)

  df <- data.frame(
    ".var" = names(cols),
    ".dist" = list_c(resamp[cols])
  )

  if (!plot) {
    return(tibble::as_tibble(df))
  }

  df |>
    ggplot2::ggplot() +
    ggdist::stat_halfeye(mapping = ggplot2::aes(xdist = .data[[".dist"]])) +
    ggplot2::facet_wrap(~ .data[[".var"]], scales = "free_x") +
    ggplot2::scale_x_continuous("Value") +
    ggplot2::scale_y_continuous("Density")
}

#' Visualize trace of variables along log prior volume
#'
#' Generates a tibble or ggplot showing the evolution of selected variables
#' along log prior volume.
#'
#' @param draws A draws_rvars object containing posterior samples.
#' @param cols Named integer vector of columns to visualize.
#' @param log_vol Numeric vector of log prior volumes.
#' @param plot Logical. If TRUE, returns a ggplot; if FALSE, returns a tibble.
#'
#' @return A ggplot object or a tibble, depending on `plot`.
#' @noRd
visualize_trace_ <- function(draws, cols, log_vol, plot) {
  points <- lapply(draws[cols], posterior::draws_of)

  df <- data.frame(
    ".var" = rep(names(cols), each = posterior::ndraws(draws)),
    ".point" = list_c(points),
    ".log_volume" = rep(log_vol, length(cols)),
    ".weight" = rep(stats::weights(draws), length(cols))
  )
  if (!plot) {
    return(tibble::as_tibble(df))
  }

  df |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[".log_volume"]],
      y = .data[[".point"]],
      colour = .data[[".weight"]]
    )) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[[".var"]]),
      scales = "free_y"
    ) +
    ggplot2::scale_x_continuous("Log-volume") +
    ggplot2::scale_y_continuous("Value") +
    ggplot2::scale_colour_viridis_c("Weight")
}
