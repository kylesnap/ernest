#' Plot diagnostics for a nested sampling run
#'
#' Show the normalized likelihood, importance weights, and evidence as functions
#' of log. volume.
#'
#' @param x Either an `ernest_estimates` (from [calculate()]) or an
#' `ernest_run` object.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' `x`, invisibly. A `ggplot2::ggplot()` object is printed as a
#' side effect.
#'
#' If `x` is an `ernest_estimates` object, or if `ndraws` is greater than one,
#' the plot show `ndraw` simulated log. volume values at each iteration. Else,
#' the plot will use the log. volume estimates that were recorded during the
#' run.
#'
#' @note Plotting simulated results requires the `ggdist` package.
#'
#' @seealso [calculate()] for generating `ernest_estimates` objects; also, [visualize()] to
#' plot the posterior distributions of the parameters from a run.
#'
#' @rdname plot.ernest
#' @export
#' @examples
#' # Plot integration results from a run.
#' data(ernest_run_example)
#' plot(ernest_run_example)
#'
#' # Simulate results before plotting.
#' library(ggdist)
#' data(ernest_run_example)
#' plot(ernest_run_example, ndraws = 50)
plot.ernest_estimates <- function(x, ...) {
  check_dots_empty()
  print(autoplot(x, ...))
}

#' @param ndraws Number of simulated log. volume values to generate before plotting.
#' If `ndraws = 0`, plot the log. volume estimates generated during the run.
#'
#' @rdname plot.ernest
#' @export
#' @examples
#' # Simulate results from a run, then plot simulated results.
#' library(ggdist)
#' data(ernest_run_example)
#'
#' sim <- calculate(ernest_run_example, ndraws = 50)
#' plot(sim)
plot.ernest_run <- function(x, ..., ndraws = 0) {
  check_dots_empty()
  if (ndraws != 0) {
    if (ndraws < 2L) {
      cli::cli_abort(
        "`ndraws` must be greater than 1 to plot an `ernest_estimate` object."
      )
    }
    x <- calculate(x, ndraws = ndraws)
  }
  print(autoplot(x, ...))
}

# AUTOPLOT METHODS -----

#' @importFrom ggplot2 autoplot ggplot aes geom_line geom_col
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous vars
#' @importFrom ggplot2 scale_fill_viridis_d facet_grid
#' @noRd
#' @export
autoplot.ernest_estimates <- function(object, ...) {
  check_dots_empty()
  check_installed(
    c("ggdist"),
    reason = "For plotting `ernest_estimate` objects."
  )
  if (attr(object, "ndraws") == 0) {
    cli::cli_abort("Unavailable for `ernest_estimate` objects with one draw.")
  }

  log_z <- object$log_evidence[length(object$log_evidence)]
  log_vol <- mean(object$log_volume)
  weight <- exp(object$log_weight - log_z)

  df_w <- ggdist::curve_interval(weight, .width = c(0.5, 0.8, 0.95))
  df_w <- tibble::tibble(
    log_vol = rep(log_vol, 3),
    .var = df_w$.value,
    .label = "Posterior Weight",
    .lower = df_w$.lower,
    .upper = df_w$.upper,
    .width = df_w$.width
  )
  df_ll <- tibble::tibble(
    log_vol = log_vol,
    .var = stats::median(exp(object$log_lik - max(max(object$log_lik)))),
    .label = "Normalized Likelihood",
    .lower = NA,
    .upper = NA,
    .width = NA
  )
  df_z <- ggdist::curve_interval(
    exp(object$log_evidence),
    .width = c(0.5, 0.8, 0.95),
  )
  df_z <- tibble::tibble(
    log_vol = rep(log_vol, 3),
    .var = df_z$.value,
    .label = "Evidence",
    .lower = df_z$.lower,
    .upper = df_z$.upper,
    .width = df_z$.width
  )
  df <- vctrs::vec_c(df_w, df_ll, df_z)

  ggplot(df, aes(x = .data[["log_vol"]], y = .data[[".var"]])) +
    ggdist::geom_lineribbon(
      data = ~ subset(., .label == "Evidence"),
      aes(ymin = .data[[".lower"]], ymax = .data[[".upper"]])
    ) +
    geom_line(data = ~ subset(., .label == "Normalized Likelihood")) +
    geom_col(
      data = ~ subset(., .label == "Posterior Weight"),
      position = "identity",
      width = 0.1
    ) +
    scale_x_continuous("Log Volume") +
    scale_y_continuous(NULL) +
    scale_fill_viridis_d("Tukey Depth") +
    facet_grid(rows = vars(.data[[".label"]]), scales = "free_y") +
    ggdist::theme_ggdist()
}

#' @importFrom ggplot2 geom_ribbon
#' @export
autoplot.ernest_run <- function(object, ...) {
  check_dots_empty()
  theme <- if (is_installed("ggdist")) {
    ggdist::theme_ggdist()
  } else {
    ggplot2::theme_minimal()
  }

  log_z <- object$log_evidence[length(object$log_evidence)]
  log_z_err <- sqrt(object$log_evidence_var)
  df_w <- tibble::tibble(
    log_vol = object$log_volume,
    .var = exp(object$log_weight - log_z),
    .lower = NA,
    .upper = NA,
    .width = NA,
    .label = "Posterior Weight"
  )
  df_ll <- tibble::tibble(
    log_vol = object$log_volume,
    .var = exp(object$log_lik - max(object$log_lik)),
    .lower = NA,
    .upper = NA,
    .width = NA,
    .label = "Normalized Likelihood"
  )
  df_z <- tibble::tibble(
    log_vol = rep(object$log_volume, 2),
    .var = rep(exp(object$log_evidence), 2),
    .lower = c(
      exp(object$log_evidence - log_z_err),
      exp(object$log_evidence - 2 * log_z_err)
    ),
    .upper = c(
      exp(object$log_evidence + log_z_err),
      exp(object$log_evidence + 2 * log_z_err)
    ),
    .width = rep(c("1", "2"), each = length(object$log_volume)),
    .label = "Evidence"
  )
  df <- vctrs::vec_c(df_w, df_ll, df_z)

  ggplot(df, aes(x = .data[["log_vol"]], y = .data[[".var"]])) +
    geom_line(
      data = ~ subset(., .label == "Evidence")
    ) +
    geom_ribbon(
      data = ~ subset(., .label == "Evidence"),
      aes(ymin = .data[[".lower"]], ymax = .data[[".upper"]], fill = .data[[".width"]]),
      alpha = 0.5
    ) +
    geom_line(data = ~ subset(., .label == "Normalized Likelihood")) +
    geom_col(
      data = ~ subset(., .label == "Posterior Weight"),
      position = "identity",
      width = 0.1
    ) +
    scale_x_continuous("Log Volume") +
    scale_y_continuous(NULL) +
    scale_fill_viridis_d(expression(sigma[log(z)])) +
    facet_grid(rows = vars(.data[[".label"]]), scales = "free_y") +
    theme
}
