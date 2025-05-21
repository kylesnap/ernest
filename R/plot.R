#' Autoplot an `ernest_sampler` object
#'
#' @importFrom ggplot2 autoplot ggplot aes geom_line geom_ribbon geom_vline
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 facet_grid vars scale_x_continuous scale_y_continuous
#' @export
#' @noRd
autoplot.ernest_sampler <- function(
  object,
  exponentiate = TRUE,
  true_log_z = NULL,
  ...
) {
  check_dots_empty()
  if (object$niterations < 1L) {
    cli::cli_abort("No iterations have been run.")
  }
  calc <- object$calculate()
  if (is_empty(calc)) {
    cli::cli_abort("No evidence has been calculated.")
  }

  # Variables for plotting
  ll_df <- tibble(
    "log_volume" = calc$log_volume,
    "val" = calc$log_likelihood - max(calc$log_likelihood),
    "err" = NA,
    "val.min" = NA,
    "val.max" = NA,
    "val.2min" = NA,
    "val.2max" = NA,
    "panel" = paste0(
      if (isFALSE(exponentiate)) "Log-" else NULL,
      "Likelihood\n(Normalized)"
    )
  )
  lw_df <- tibble(
    "log_volume" = calc$log_volume,
    "val" = calc$log_weight - max(calc$log_weight),
    "err" = NA,
    "val.min" = NA,
    "val.max" = NA,
    "val.2min" = NA,
    "val.2max" = NA,
    "panel" = paste0(
      if (isFALSE(exponentiate)) "Log. " else NULL,
      "Weight\n(Normalized)"
    )
  )
  z_df <- tibble(
    "log_volume" = calc$log_volume,
    "val" = calc$log_evidence,
    "err" = sqrt(calc$log_evidence.var),
    "val.min" = exp(.data$val - .data$err),
    "val.max" = exp(.data$val + .data$err),
    "val.2min" = exp(.data$val - 2 * .data$err),
    "val.2max" = exp(.data$val + 2 * .data$err),
    "panel" = paste0(
      if (isFALSE(exponentiate)) "Log. " else NULL,
      "Evidence"
    )
  )
  plot_df <- rbind(ll_df, lw_df, z_df)
  if (exponentiate) {
    plot_df$val <- exp(plot_df$val)
    plot_df$val.min <- exp(plot_df$val.min)
    plot_df$val.max <- exp(plot_df$val.max)
    plot_df$val.2min <- exp(plot_df$val.2min)
    plot_df$val.2max <- exp(plot_df$val.2max)
  }
  vol_cutoff <- calc$log_volume[object$niterations]
  p <- ggplot(data = plot_df, aes(x = -.data$log_volume, y = .data$val)) +
    facet_grid(rows = vars(.data$panel), scales = "free_y") +
    geom_ribbon(
      data = z_df,
      aes(ymin = .data$val.min, ymax = .data$val.max),
      fill = "blue",
      alpha = 0.4
    ) +
    geom_ribbon(
      data = z_df,
      aes(ymin = .data$val.2min, ymax = .data$val.2max),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(colour = "blue") +
    scale_x_continuous(expr(-log("Volume"))) +
    scale_y_continuous(NULL) +
    geom_vline(aes(xintercept = -vol_cutoff), linetype = "dashed")
  if (!is.null(true_log_z)) {
    p <- p +
      geom_hline(
        data = z_df,
        aes(yintercept = if (exponentiate) exp(true_log_z) else true_log_z),
        linetype = "dotted"
      )
  }
  p
}

#' Plot an `ernest_sampler` object
#'
#' Use `ggplot` to create a plot of the evidence, importance weights, and
#' normalized likelihood values over the estimated volumes from a nested
#' sampling run.
#'
#' @param x An `ernest_sampler` object.
#' @param exponentiate Whether to transform log values before plotting.
#' Note that volume estimates are always plotted in negative log units.
#' @param true_log_z The analytic evidence value on a log scale.
#' @param ... Must be left empty.
#'
#' @returns A plot of the run's progress, which is made up of three stacked
#' plots:
#' * Normalized likelihood values over log volumes.
#' * importance weights over log volumes.
#' * Model evidence over log volumes, with an error envelope showing 1 and 2
#' SDs from the estimate.
#'
#' @importFrom graphics plot
#' @export
plot.ernest_sampler <- function(
  x,
  exponentiate = TRUE,
  true_log_z = NULL,
  ...
) {
  print(autoplot(x, exponentiate = exponentiate, true_log_z = true_log_z, ...))
}
