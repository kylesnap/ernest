#' Plot an `ErnestSampler` object
#'
#' Use `ggplot` to create a plot of the evidence, importance weights, and normalized
#' likelihood values over the estimated volumes from a nested sampling run.
#'
#' @param object An `ErnestSampler` object.
#' @param true_log_z The analytic evidence value on a log scale.
#' @param ... Must be left empty.
#'
#' @importFrom ggplot2 autoplot ggplot aes geom_line geom_ribbon geom_vline geom_hline
#' @importFrom ggplot2 facet_grid vars scale_x_continuous scale_y_continuous
#' @export
#' @noRd
autoplot.ErnestSampler <- function(object, true_log_z = NULL, ...) {
  check_dots_empty()
  calc <- calculate(object)
  if (is.null(calc)) {
    cli::cli_abort("No evidence has been calculated for this run.")
  }
  ll_df <- tibble(
    "log_vol" = calc$log_vol,
    "val" = exp(calc$log_lik.norm),
    "panel" = "Likelihood\n(Normalized)"
  )
  lw_df <- tibble(
    "log_vol" = calc$log_vol,
    "val" = exp(calc$log_weight),
    "panel" = "Weight\n(Normalized)"
  )
  z_df <- tibble(
    "log_vol" = calc$log_vol,
    "val" = exp(calc$log_z),
    "panel" = "Evidence"
  )
  z_lim_df <- tibble(
    "log_vol" = calc$log_vol,
    "val" = exp(calc$log_z),
    "val.min" = exp(calc$log_z - calc$log_z_err),
    "val.max" = exp(calc$log_z + calc$log_z_err),
    "val.2min" = exp(calc$log_z - 2 * calc$log_z_err),
    "val.2max" = exp(calc$log_z + 2 * calc$log_z_err),
    "panel" = "Evidence"
  )
  plot_df <- rbind(ll_df, lw_df, z_df)
  vol_cutoff <- plot_df$log_vol[object$wrk$n_iter]
  p <- ggplot(data = plot_df, aes(x = -.data$log_vol, y = .data$val)) +
    facet_grid(rows = vars(.data$panel), scales = "free_y") +
    geom_ribbon(
      data = z_lim_df,
      aes(ymin = .data$val.min, ymax = .data$val.max),
      fill = "blue",
      alpha = 0.4
    ) +
    geom_ribbon(
      data = z_lim_df,
      aes(ymin = .data$val.2min, ymax = .data$val.2max),
      fill = "blue",
      alpha = 0.2
    ) +
    geom_line(colour = "blue") +
    scale_x_continuous(expr(-log("Volume"))) +
    scale_y_continuous(NULL) +
    geom_vline(aes(xintercept = -vol_cutoff), linetype = "dashed")
  if (!is.null(true_log_z)) {
    p <- p + geom_hline(
      data = z_lim_df,
      aes(xintercept = true_log_z),
      linetype = "dotted"
    )
  }
  p
}

#' Plot an `ErnestSampler` object
#'
#' Use `ggplot` to create a plot of the evidence, importance weights, and normalized
#' likelihood values over the estimated volumes from a nested sampling run.
#'
#' @param x An `ErnestSampler` object.
#' @param true_log_z The analytic evidence value on a log scale. Currently does
#' nothing.
#' @param ... Must be left empty.
#'
#' @importFrom graphics plot
#' @export
plot.ErnestSampler <- function(x, true_log_z = NULL, ...) {
  print(autoplot(x, true_log_z = true_log_z, ...))
}
