#' Plot diagnostics from nested sampling results
#'
#' Visualizes key diagnostics from nested sampling outputs as functions of
#' log-prior volume.
#'
#' @param x [[ernest_run]] or [[ernest_estimate]]\cr An object containing
#' results from nested sampling.
#' @param which `[character()]`\cr One or more diagnostics to display. Must be
#' any of `"evidence"`, `"weight"`, and `"likelihood"`.
#' @param n `[integer(1)]`\cr Number of evaluation points along the
#' log-volume axis used to summarize each curve.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' For `plot`, the `ggplot` object, invisibly. It is also printed as a side
#' effect.
#' For `summary`, a `list` with possible elements `evidence`, `weight`,
#' and `likelihood`. Each element is a data frame.
#'
#' @details
#' `plot()` is a visualization wrapper around [summary.ernest_estimate()],
#' followed by `autoplot()`. Use `which` to select diagnostics:
#'
#' * `which = "evidence"`: Estimated marginal likelihood as a function of
#' log-prior volume.
#' * `which = "weight"`: Posterior mass concentration across log-prior volume.
#' * `which = "likelihood"`: Normalized likelihood across log-prior volume.
#'
#' If `x` is an `ernest_run`, plotting first computes
#' `calculate(x, ndraws = 0)`. In this mode, `log_volume` and `log_weight` are
#' deterministic, and evidence uncertainty comes from the analytical normal
#' approximation generated from the original sampling run.
#'
#' If `x` is an `ernest_estimate` generated with `ndraws > 0`, diagnostics are
#' summarized over simulated log-volume trajectories. For these simulated
#' estimates, uncertainty bands for `evidence` and `weight` are computed as
#' interval summaries on interpolated curves.
#'
#' To get the underlying data frames used for plotting, use
#' `summary.ernest_estimate()`. This is useful when you want full control over
#' plotting.
#'
#' @note
#' Plotting multiple diagnostics with `which` requires \CRANpkg{patchwork}.
#' Plotting `evidence` or `weight` diagnostics requires \CRANpkg{ggdist}.
#'
#' @srrstats {BS6.1} Default plot for return object.
#'
#' @seealso [calculate.ernest_run()]
#' @family visualizations
#'
#' @examples
#' # Plot diagnostics from a run (analytical uncertainty for evidence).
#' data(example_run)
#' plot(example_run)
#'
#' # Plot diagnostics from simulated log-volume trajectories.
#' set.seed(123)
#' est <- calculate(example_run, ndraws = 100)
#' plot(est)
#' @rdname plot-ernest
#' @export
plot.ernest_estimate <- function(
  x,
  which = c("evidence", "weight", "likelihood"),
  n = 512,
  ...
) {
  which <- check_plot_which(which)
  print(autoplot(x, which, n = n, ...))
}

#' @rdname plot-ernest
#' @export
plot.ernest_run <- function(
  x,
  which = c("evidence", "weight", "likelihood"),
  n = 512,
  ...
) {
  which <- check_plot_which(which)
  withr::with_preserve_seed(
    obj <- calculate(x, ndraws = 0)
  )
  print(autoplot(obj, which, n = n, ...))
}

#' @rdname plot.ernest
#'
#' @param object [[ernest_estimate]]\cr Output from [calculate.ernest_run()].
#' @param which `[character()]`\cr One or more diagnostics to summarize. Must be
#' any of `"evidence"`, `"weight"`, and `"likelihood"`.
#' @param width `[double()]`\cr Vector of probabilities to use that determine
#' the widths of the resulting intervals, as in [[ggdist::curve_interval]].
#' Defaults to three widths roughly corresponding to +/- 1, 2, and 3 SD.
#'
#' @export
summary.ernest_estimate <- function(
  object,
  which = c("evidence", "weight", "likelihood"),
  n = 512,
  width = NULL,
  ...
) {
  check_dots_empty()
  which <- arg_match(which, multiple = TRUE)
  width <- width %||% c(0.5218917, 0.8440126, 0.9666667)
  width <- vec_cast(width, to = double())
  nwidth <- length(width)
  check_number_whole(n, min = 2)
  if (any(c("evidence", "weight") %in% which)) {
    check_installed("ggdist", "to summarize evidence and weight diagnostics")
  }

  log_volume <- posterior::draws_of(object$log_volume)
  log_evidence <- posterior::draws_of(object$log_evidence)
  log_weight <- posterior::draws_of(object$log_weight)
  weight <- if (nrow(log_weight) == 1) {
    log_z <- matrixStats::logSumExp(drop(log_weight))
    exp(log_weight - log_z)
  } else {
    exp(sweep(
      log_weight,
      1,
      log_evidence[, ncol(log_evidence)],
      FUN = "-"
    ))
  }

  lik <- exp(object$log_lik - max(object$log_lik))
  knots <- seq(mean(log_volume[, ncol(log_volume)]), 0, length.out = n)

  # Safely summarize curves with or without uncertainty intervals.
  safe_curve <- \(x, y) {
    if (!is.matrix(drop(y))) {
      data_frame0(
        "x" = x,
        ".value" = y
      )
    } else {
      ggdist::curve_interval(
        data_frame0(
          "x" = x,
          ".value" = posterior::rvar(t(y))
        ),
        .along = x,
        .width = width
      )
    }
  }

  # Summarize evidence diagnostics, with or without uncertainty intervals.
  summarize_evidence <- \() {
    if (attr(object, "ndraws") == 0) {
      df <- data_frame0(
        "x" = log_volume[1, ],
        "dist" = attr(object, "log_z_dist")
      )
      df <- df[as.integer(seq(1, to = nrow(df), length.out = n)), ]
      df <- data_frame0(
        "x" = rep(df$x, nwidth),
        !!!ggdist::point_interval(
          df$dist,
          .width = width
        )
      )
      df[c(".value", ".lower", ".upper")] <- exp(df[c(
        ".value",
        ".lower",
        ".upper"
      )])
      return(df)
    }

    yout <- vapply(
      seq_len(nrow(log_evidence)),
      \(i) {
        stats::approx(
          log_volume[i, ],
          exp(log_evidence[i, ]),
          xout = knots,
          rule = 2
        )$y
      },
      double(n)
    )
    safe_curve(knots, yout)
  }

  # Summarize weight diagnostics, with or without uncertainty intervals.
  summarize_weight <- \() {
    yout <- vapply(
      seq_len(nrow(weight)),
      \(i) {
        dens <- ggdist::density_bounded(
          x = log_volume[i, ],
          weights = weight[i, ],
          bounds = c(NA, 0)
        )
        stats::approx(dens$x, dens$y, xout = knots, rule = 2)$y
      },
      double(n)
    )
    safe_curve(knots, yout)
  }

  # Summarize likelihood diagnostics, which are always point estimates.
  summarize_likelihood <- \() {
    lik_app <- approx(
      colMeans(log_volume),
      lik,
      xout = knots,
      rule = 2
    )
    data_frame0("x" = lik_app$x, ".value" = lik_app$y)
  }

  evidence <- if ("evidence" %in% which) summarize_evidence()
  weight_df <- if ("weight" %in% which) summarize_weight()
  likelihood <- if ("likelihood" %in% which) summarize_likelihood()

  list(
    "evidence" = evidence,
    "weight" = weight_df,
    "likelihood" = likelihood
  )
}

#' Validate Plot Type Argument
#'
#' @param which Character vector: Must be one of "evidence", "weight", or
#' "likelihood".
#' @param call The calling environment.
#'
#' @return Character vector of validated plot types.
#' @noRd
check_plot_which <- function(which, call = caller_env()) {
  which <- arg_match(
    which,
    values = c("evidence", "weight", "likelihood"),
    multiple = TRUE,
    call = call
  )
  if (length(which) == 0) {
    cli::cli_abort(
      "At least one plot type must be specified in `which`.",
      call = call
    )
  } else if (length(which) > 1) {
    check_required("patchwork", "to combine multiple plots", call = call)
  }
  which
}

# AUTOPLOT METHODS -----
# nocov start

#' Generates a ggplot object for an `ernest_estimate` object containing
#' uncertainty simulations.
#'
#' @param object An `ernest_estimate` object containing uncertainty simulations.
#' @param which Which plots to display.
#' @param n Number of evaluation points to summarize each curve.
#' @param ... Ignored.
#'
#' @return A ggplot object.
#' @noRd
#' @importFrom ggplot2 autoplot
#' @importFrom posterior draws_of
#' @export
autoplot.ernest_estimate <- function(object, which, n = 512, ...) {
  check_dots_empty()
  check_number_whole(n, min = 2)
  diagnostic_data <- summary(
    object,
    which = which,
    n = n,
    width = c(0.5218917, 0.8440126, 0.9666667)
  )
  xint <- attr(object, "dead_log_vol")
  plots <- lapply(which, \(x) {
    switch(
      x,
      "evidence" = autoplot_(
        diagnostic_data[["evidence"]],
        "Evidence",
        filllab = if (attr(object, "ndraws") == 0) "Quantile" else "MHD",
        xintercept = xint
      ),
      "weight" = autoplot_(
        diagnostic_data[["weight"]],
        "Importance Weight",
        xintercept = xint
      ),
      "likelihood" = autoplot_(
        diagnostic_data[["likelihood"]],
        "Normalized Likelihood",
        xintercept = xint
      )
    )
  })

  if (length(plots) > 1) {
    patchwork::wrap_plots(plots) +
      patchwork::plot_layout(ncol = 1, axes = "collect")
  } else {
    plots[[1]]
  }
}

#' Internal helper used by autoplot to visualize a relationship between
#' a variable and log-volume.
#'
#' @param df A data frame containing columns `x` and `.value`, and optionally
#' `.lower`, `.upper`, and `.width` for interval plotting.
#' @param ylab Label for the y-axis.
#' @param filllab Label for the fill legend when intervals are plotted.
#' @param filllabs Optional custom labels for the fill legend when intervals
#' are plotted.
#' @param xintercept Optional numeric value for a vertical dashed line to
#' indicate the "dead" log-volume where the likelihood first becomes positive.
#'
#' @return A `ggplot2::ggplot()` object.
#' @importFrom ggplot2 ggplot aes geom_vline geom_line theme_minimal
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_fill_brewer
#' @noRd
autoplot_ <- function(
  df,
  ylab,
  filllab = "MHD",
  filllabs = NULL,
  xintercept = NULL
) {
  p <- ggplot(df, aes(.data$x, y = .data$.value)) +
    scale_x_continuous("Log-volume") +
    scale_y_continuous(ylab) +
    theme_minimal()
  xint <- geom_vline(xintercept = xintercept, linetype = 2)
  if (".width" %in% names(df)) {
    p +
      ggdist::geom_lineribbon(aes(
        y = .data$.value,
        ymin = .data$.lower,
        ymax = .data$.upper
      )) +
      scale_fill_brewer(
        filllab,
        breaks = c(0.5218917, 0.8440126, 0.9666667),
        labels = c(0.50, 0.80, 0.95),
        palette = "Reds"
      ) +
      xint
  } else {
    p + geom_line(data = df, aes(y = .data$.value)) + xint
  }
}

# nocov end
