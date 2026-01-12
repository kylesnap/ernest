#' Visualize posterior distributions or traces from a nested sampling run
#'
#' Produces visualizations of the posterior distributions or the evolution of
#' variables along the log-prior volume from a nested sampling run.
#'
#' @param x An [ernest_run] object containing the results of a nested sampling
#' run.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more variables
#' to plot from the run. If omitted, all variables are plotted.
#' @param .which Character string specifying the type of plot to produce.
#' Options are `"density"` for the posterior density of each parameter or
#' `"trace"` for the trace of variables along log-prior volume.
#' @inheritParams as_draws.ernest_run
#'
#' @returns
#' A `ggplot2::ggplot()` object.
#'
#' @details
#' The `visualize()` function is designed to quickly explore the results of a
#' nested sampling run through two types of plots:
#' - **Density plots** show the marginal posterior for each selected variable,
#' using `ggdist::stat_halfeye()` to visualize uncertainty and distribution
#' shape.
#' - **Trace plots** display the evolution of variables as a function of
#' log-prior volume, with points coloured by posterior weight. This can help
#' diagnose sampling behavior and identify regions of interest in the prior
#' volume.
#'
#' Posterior weights are derived from the individual contributions of each
#' sampled point in the prior space to a run's log-evidence estimate. A point's
#' weight is a function of (a) the point's likelihood and (b) the estimated
#' amount of volume within that point's likelihood contour. See ernest's
#' vignettes for more information.
#'
#' @note
#' This package requires the {tidyselect} package to be installed. If
#' `which = "trace"` is selected, the {ggdist} package is also required.
#'
#' @srrstats {G2.3, G2.3a, G2.3b} Uses arg_match() to ensure informative error
#' messages for invalid `which` values.
#' @srrstats {BS6.2, BS6.3} Provides plot methods for posterior samples and
#' sequences of samples.
#'
#' @seealso
#' - [plot()] for diagnostic plots of nested sampling runs.
#' - [as_draws_rvars()] for extracting posterior samples.
#'
#' @examples
#' # Load example run
#' library(ggdist)
#' data(example_run)
#'
#' # Plot posterior densities for all parameters
#' visualize(example_run, .which = "density")
#'
#' # Plot trace of the radial coordinate in unit-cube scale
#' visualize(
#'   example_run,
#'   .which = "trace",
#'   .units = "unit_cube",
#'   .radial = TRUE
#' )
#' @export
visualize.ernest_run <- function(
  x,
  ...,
  .which = c("density", "trace"),
  .units = c("original", "unit_cube"),
  .radial = FALSE
) {
  rlang::check_installed(c("ggdist", "tidyselect"))
  .which <- arg_match(.which)
  draws <- as_draws_rvars(x, units = .units, radial = .radial)

  expr <- if (dots_n(...) == 0) {
    if (length(x$prior$names) > 10) {
      cli::cli_abort(c(
        "Cannot automatically plot {length(x$prior$names)} variables.",
        "i" = "Use {.fn tidyselect::everything()} to override this error."
      ))
    }
    expr(tidyselect::all_of(c(
      !!x$prior$names,
      !!(if (.radial) ".radial" else NULL)
    )))
  } else {
    if (dots_n(...) > 10) {
      cli::cli_warn("Plots with more than 10 variables may be cluttered.")
    }
    expr(c(...))
  }

  if (.which == "density") {
    check_installed("ggdist", "to create density plots.")
    draws <- posterior::resample_draws(draws)
    pos <- tidyselect::eval_select(expr, data = draws)
    selected <- draws[pos]
    visualize_density(selected)
  } else {
    draws_df <- as.data.frame(posterior::as_draws_df(draws))
    pos <- tidyselect::eval_select(expr, data = draws_df, error_call = call)
    visualize_trace(draws_df, pos, x$log_volume, weights(draws))
  }
}

#' Creates a faceted plot of the marginal posterior distributions for each
#' selected variable from a nested sampling run, using `ggdist::stat_halfeye()`
#' to show uncertainty and distribution shape.
#'
#' @param selected A draws_rvars object containing posterior samples for the
#' selected variables. This should be resampled before being passed to this
#' function.
#'
#' @return A `ggplot2::ggplot()` object with one facet per variable.
#' @noRd
visualize_density <- function(selected) {
  tibble(".variable" = names(selected), ".dist" = list_c(selected)) |>
    ggplot() +
    ggdist::stat_halfeye(
      mapping = ggplot2::aes(xdist = .data[[".dist"]]),
      point_interval = "median_hdci"
    ) +
    facet_grid(rows = vars(.data[[".variable"]])) +
    scale_x_continuous("Value") +
    scale_y_continuous("Density") +
    theme_minimal()
}

#' Creates a faceted scatter plot showing the evolution of selected variables
#' as a function of log-prior volume.
#'
#' @param draws Data frame of posterior samples for all variables.
#' @param pos Integer vector of column positions for the selected variables to
#' plot.
#' @param log_volume Numeric vector of log-prior volumes for each sample.
#' @param weights Numeric vector of posterior weights for each sample.
#'
#' @return A `ggplot2::ggplot()` object with one facet per variable,
#' showing traces colored by posterior weight.
#' @noRd
visualize_trace <- function(draws, pos, log_volume, weights) {
  df <- do.call(
    rbind,
    lapply(pos, \(p) {
      value <- unlist(draws[p], use.names = FALSE)
      tibble::tibble(
        ".variable" = names(draws)[p],
        ".value" = as.numeric(value),
        "log_volume" = log_volume,
        "weights" = weights
      )
    })
  )

  df |>
    ggplot(aes(x = log_volume, y = .value, colour = weights)) +
    geom_point() +
    scale_colour_distiller("Posterior Weight", palette = "Reds") +
    facet_grid(rows = vars(.data[[".variable"]])) +
    scale_x_continuous("Log. Volume") +
    scale_y_continuous("Value") +
    theme_minimal()
}
