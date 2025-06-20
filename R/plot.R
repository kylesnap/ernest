#' Plot the Integration Progress From a Nested Sampling Run
#'
#' Using an `ernest_estimates` or an `ernest_run` object, `plot` creates
#' a faceted `ggplot()` showing iterative integration results over estimates
#' of the log prior volume. These plots show the likelihood values (normalized),
#' the posterior weights, and the model evidence. The latter quantity is
#' accompanied with an error ribbon capturing uncertainty.
#'
#' @param x An object of class 'ernest_run' or 'ernest_estimates'.
#' @param ndraws For `plot.ernest_run`, the number of draws to use for
#' estimating the evidence integral. If 0, the original results are used.
#' @inheritParams rlang::args_dots_empty
#'
#' @details
#' Calling `plot.ernest_run(run, ndraws = n)` is identical to calling
#' `plot.ernest_estimates(calculate(run, ndraws = n))`.
#'
#' @rdname plot.ernest
#' @export
plot.ernest_estimates <- function(x, ...) {
  check_dots_empty()
  print(autoplot(x, ...))
}

#' @rdname plot.ernest
#' @export
plot.ernest_run <- function(x, ndraws = 0, ...) {
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
  df_w <- tibble::tibble(
    log_vol = mean(object$log_volume),
    .var = exp(object$log_weight - log_z),
    .label = "Posterior Weight"
  )
  df_ll <- tibble::tibble(
    log_vol = mean(object$log_volume),
    .var = exp(object$log_lik - max(max(object$log_lik))),
    .label = "Normalized Likelihood"
  )
  df_z <- tibble::tibble(
    log_vol = mean(object$log_volume),
    .var = exp(object$log_evidence),
    .label = "Evidence"
  )
  df <- vctrs::vec_c(df_w, df_ll, df_z) |>
    ggdist::curve_interval(.var, .width = c(0.5, 0.8, 0.95))

  ggplot(df, aes(x = log_vol, y = .var)) +
    ggdist::geom_lineribbon(
      data = ~ subset(., .label == "Evidence"),
      aes(ymin = .lower, ymax = .upper)
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
    facet_grid(rows = vars(.label), scales = "free_y") +
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

  ggplot(df, aes(x = log_vol, y = .var)) +
    geom_line(
      data = ~ subset(., .label == "Evidence")
    ) +
    geom_ribbon(
      data = ~ subset(., .label == "Evidence"),
      aes(ymin = .lower, ymax = .upper, fill = .width),
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
    facet_grid(rows = vars(.label), scales = "free_y") +
    theme
}
