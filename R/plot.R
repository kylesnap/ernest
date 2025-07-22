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
#' if `x` is an `ernest_estimate` (or `ndraws != 0`), these values are simulated.
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
#' HDI. If HDI calculation fails, the user will be warned and an `ernest_run` plot
#' will be generated instead.
#'
#' @seealso [calculate()] for generating `ernest_estimate` objects; also, [visualize()] to
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

#' @importFrom ggplot2 autoplot ggplot aes geom_line geom_col
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous vars
#' @importFrom ggplot2 scale_fill_viridis_d facet_grid
#' @noRd
#' @export
autoplot.ernest_estimate <- function(object, ...) {
  check_dots_empty()

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
      autoplot_run_(
        vctrs::vec_c(df_w, df_ll, df_z),
        "HDCI",
        c(0.95, 0.68),
        c("95%", "68%")
      )
    },
    error = function(cnd) {
      cli::cli_warn(
        c(
          "Plotting {.cls ernest_run} instead after a failure.",
          "i" = "Is `ndraws` large enough to calculate HDIs?"
        ),
        parent = cnd
      )

      make_plot_df(
        log_volume = mean(object$log_volume),
        log_evidence = mean(object$log_evidence),
        log_evidence_var = posterior::sd(object$log_evidence) %|% 0.0,
        log_weight = mean(object$log_weight),
        log_lik = mean(object$log_lik)
      )
    }
  )
}

#' @export
autoplot.ernest_run <- function(object, ...) {
  check_dots_empty()

  make_plot_df(
    log_volume = object$log_volume,
    log_evidence = object$log_evidence,
    log_evidence_var = object$log_evidence_var,
    log_weight = object$log_weight,
    log_lik = object$log_lik
  )
}

#' Create a data.frame for plotting the results of a run
#' @noRd
make_plot_df <- function(
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
  df <- vctrs::vec_c(df_w, df_ll, df_z)
  autoplot_run_(
    df,
    expression(hat(sigma[Z])),
    c(3, 2, 1),
    c(expression(3 * sigma), expression(2 * sigma), expression(1 * sigma))
  )
}

#' Autoplot for Ernest Run Objects
#'
#' @param object a data.frame from make_plot_df
#' @param fill_name Name of the fill aesthetic in the plot.
#' @param fill_limits Limits for the fill aesthetic.
#' @param fill_labels Labels for the fill aesthetic.
#'
#' @srrstats {G2.4c} Explicit cast of `fill_limits` to character.
#'
#' @return A ggplot object representing the diagnostic plots.
#' @importFrom ggplot2 geom_ribbon
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
