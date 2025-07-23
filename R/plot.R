#' Plot diagnostics for a nested sampling run
#'
#' Show the normalized likelihood, importance weights, and evidence as functions
#' of log. volume.
#'
#' @param x (ernest_estimate or ernest_run) An object either of class
#' `ernest_estimate` (containing uncertainty simulations) or of class
#' `ernest_run` (containing a nested sampling run).
#' @inheritParams rlang::args_dots_empty
#'
#' @returns
#' `x`, invisibly. A `ggplot2::ggplot()` object is printed as a
#' side effect.
#'
#' The plot is faceted in three frames. The horizontal axis shows values of log.
#' volume: If `x` is an `ernest_run` these estimates are derived from the run,
#' if `x` is an `ernest_estimate` (or `ndraws != 0`), these values are
#' simulated.
#'
#' The three `y` axes are as follows:
#'
#' * **Evidence:** Estimate with a corresponding error ribbon drawn from either
#' the estimated standard error (if `ernest_run`) or drawn from the high density
#' credible interval (HDCI) (if `ernest_sampler`);
#' * **Normalized Likelihood:** The likelihood value of the criteria used
#' to draw new points from the likelihood-restricted prior sampler, normalized
#' by the maximum likelihood generated during the run, and;
#' * **Posterior Weight**: The density of the posterior weights attributed to
#' regions of volume within the prior. If using an `ernest_estimate` object,
#' an error ribbon is drawn with the HDCI of this estimate.
#'
#' @note For `ernest_estimate`, `ndraws` must be sufficiently large to calculate
#' HDI. If HDI calculation fails, the user will be warned and an `ernest_run`
#' plot will be generated instead.
#'
#' @seealso [calculate()] for generating `ernest_estimate` objects; also,
#' [visualize()] to plot the posterior distributions of the parameters from a
#' run.
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
plot.ernest_estimate <- function(x, ...) {
  check_dots_empty()
  print(autoplot(x, ...))
}

#' @inheritParams calculate.ernest_run
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
  ndraws <- as_scalar_count(ndraws, positive = FALSE)
  if (ndraws != 0) {
    x <- calculate(x, ndraws = ndraws)
  }
  print(autoplot(x, ...))
}

# AUTOPLOT METHODS -----

#' Autoplot for Ernest Estimates
#'
#' @param object An `ernest_estimate` object containing uncertainty simulations.
#' @param ... Additional arguments passed to the method.
#'
#' @return A ggplot object.
#' @noRd
#' @export
autoplot.ernest_estimate <- function(object, ...) {
  check_dots_empty()
  plot_tbl <- calc_hdi_tbl(object)
  autoplot_run_(
    plot_tbl$df,
    fill_name = plot_tbl$fill_name,
    fill_limits = plot_tbl$fill_limits,
    fill_labels = plot_tbl$fill_labels
  )
}

#' Autoplot for Ernest Run Objects
#'
#' @param object An `ernest_run` object containing a nested sampling run.
#' @param ... Additional arguments passed to the method.
#' @return A ggplot object.
#' @export
autoplot.ernest_run <- function(object, ...) {
  check_dots_empty()
  plot_tbl <- calc_var_tbl(
    log_volume = object$log_volume,
    log_evidence = object$log_evidence,
    log_evidence_var = object$log_evidence_var,
    log_weight = object$log_weight,
    log_lik = object$log_lik
  )
  autoplot_run_(
    plot_tbl$df,
    fill_name = plot_tbl$fill_name,
    fill_limits = plot_tbl$fill_limits,
    fill_labels = plot_tbl$fill_labels
  )
}

#' Calculate a data.frame for plotting evidence simulations
#'
#' @param object An `ernest_estimate` object containing uncertainty simulations.
#' @return A list containing a data.frame for plotting and additional metadata
#' for the fill aesthetic.
#' @note If HDI calculation fails, a warning is issued and a fallback
#' data.frame with variance estimates is returned instead.
#' @noRd
calc_hdi_tbl <- function(object) {
  try_fetch(
    {
      max_log_z <- tail(object$log_evidence, 1)
      w <- exp(object$log_weight - max_log_z)
      density_draws <- get_density(object$log_volume, w)
      log_volume <- density_draws$log_volume

      w_68 <- hdci(density_draws$density, width = 0.68)
      w_95 <- hdci(density_draws$density, width = 0.95)
      df_w <- vctrs::vec_rbind(w_68, w_95)
      df_w$log_vol <- rep(log_volume, 2)
      df_w$.label <- "Posterior Weight"

      rvar_evid <- interpolate_evidence(
        object$log_volume,
        object$log_evidence,
        log_volume
      )

      z_68 <- hdci(rvar_evid, width = 0.68)
      z_95 <- hdci(rvar_evid, width = 0.95)
      df_z <- vctrs::vec_rbind(z_68, z_95)
      df_z$log_vol <- rep(log_volume, 2)
      df_z$.label <- "Evidence"

      log_lik <- drop(posterior::draws_of(object$log_lik))
      df_ll <- data.frame(
        "log_vol" = mean(object$log_volume),
        ".label" = "Normalized Likelihood",
        ".var" = exp(log_lik - max(log_lik)),
        ".lower" = NA,
        ".upper" = NA,
        ".width" = NA
      )
      list(
        df = vctrs::vec_c(df_w, df_ll, df_z),
        "fill_name" = "HDCI",
        "fill_limits" = c(0.95, 0.68),
        "fill_labels" = c("95%", "68%")
      )
    },
    error = function(cnd) {
      ndraws <- attr(object, "ndraws")
      cli::cli_warn(
        c(
          "Could't calculate HDIs from `object`.",
          "!" = "Returning a plot with evidence variance estimates instead.",
          "i" = if (ndraws <= 100) {
            "Is `ndraws` large enough to calculate HDIs?"
          } else {
            NULL
          }
        ),
        parent = cnd
      )

      calc_var_tbl(
        log_volume = mean(object$log_volume),
        log_evidence = mean(object$log_evidence),
        log_evidence_var = posterior::sd(object$log_evidence) %|% 0.0,
        log_weight = mean(object$log_weight),
        log_lik = mean(object$log_lik)
      )
    }
  )
}

#' Calculate a data.frame for plotting evidence values from a run
#' @param log_volume A numeric vector of log. volume values.
#' @param log_evidence A numeric vector of log. evidence values.
#' @param log_evidence_var A numeric vector of log. evidence variances.
#' @param log_weight A numeric vector of log. weights.
#' @param log_lik A numeric vector of log. likelihood values.
#' @return A list containing a data.frame for plotting and additional metadata
#' for the fill aesthetic.
#' @noRd
calc_var_tbl <- function(
  log_volume,
  log_evidence,
  log_evidence_var,
  log_weight,
  log_lik
) {
  df_z <- data.frame(
    "log_vol" = rep(log_volume, 3),
    ".label" = "Evidence",
    ".var" = rep(log_evidence, 3),
    ".width" = rep(c(1, 2, 3), each = length(log_volume)),
    ".err" = rep(sqrt(log_evidence_var), 3)
  )
  df_z[".lower"] <- exp(df_z$.var - (df_z$.width * df_z$.err))
  df_z[".upper"] <- exp(df_z$.var + (df_z$.width * df_z$.err))
  df_z$.var <- exp(df_z$.var)
  df_z$.err <- NULL

  max_log_z <- tail(log_evidence, 1)
  df_w <- data.frame(
    "log_vol" = log_volume,
    ".label" = "Posterior Weight",
    ".var" = exp(log_weight - max_log_z),
    ".lower" = NA,
    ".upper" = NA,
    ".width" = NA
  )
  df_ll <- data.frame(
    "log_vol" = log_volume,
    ".label" = "Normalized Likelihood",
    ".var" = exp(log_lik - max(log_lik)),
    ".lower" = NA,
    ".upper" = NA,
    ".width" = NA
  )
  list(
    "df" = vctrs::vec_c(df_w, df_ll, df_z),
    "fill_name" = expression(hat(sigma[Z])),
    "fill_limits" = c(3, 2, 1),
    "fill_labels" = c(
      expression(3 * sigma),
      expression(2 * sigma),
      expression(1 * sigma)
    )
  )
}

#' Autoplot for Ernest Run Objects
#'
#' @param object a data.frame containing the
#' @param fill_name Name of the fill aesthetic in the plot.
#' @param fill_limits Limits for the fill aesthetic.
#' @param fill_labels Labels for the fill aesthetic.
#'
#' @srrstats {G2.4c} Explicit cast of `fill_limits` to character.
#'
#' @return A ggplot object representing the diagnostic plots.
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 autoplot ggplot aes geom_line geom_col
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous vars
#' @importFrom ggplot2 scale_fill_viridis_d facet_grid
#' @noRd
autoplot_run_ <- function(df, fill_name, fill_limits, fill_labels) {
  df$.width <- factor(
    as.character(df$.width),
    levels = as.character(fill_limits)
  )

  weight_ribbon <- any(!is.na(df$.width) & df$.label == "Posterior Weight")
  weight_geom <- if (weight_ribbon) {
    geom_ribbon(
      data = subset(df, df$.label == "Posterior Weight"),
      aes(
        ymin = .data[[".lower"]],
        ymax = .data[[".upper"]],
        fill = .data[[".width"]]
      ),
      alpha = 0.7
    )
  } else {
    geom_line(
      data = ~ subset(., df$.label == "Posterior Weight"),
      position = "identity"
    )
  }

  ggplot(df, aes(x = .data[["log_vol"]], y = .data[[".var"]])) +
    geom_ribbon(
      data = ~ subset(., df$.label == "Evidence"),
      aes(
        ymin = .data[[".lower"]],
        ymax = .data[[".upper"]],
        fill = .data[[".width"]]
      ),
      alpha = 0.7
    ) +
    geom_line(
      data = ~ subset(., df$.label == "Evidence")
    ) +
    geom_line(data = ~ subset(., df$.label == "Normalized Likelihood")) +
    geom_line(
      data = ~ subset(., df$.label == "Posterior Weight"),
      position = "identity"
    ) +
    weight_geom +
    scale_x_continuous("Log Volume") +
    scale_y_continuous(NULL) +
    scale_fill_viridis_d(
      fill_name,
      limits = factor(fill_limits),
      labels = fill_labels,
      direction = 1
    ) +
    facet_grid(rows = vars(.data[[".label"]]), scales = "free_y") +
    ggplot2::theme_classic()
}
