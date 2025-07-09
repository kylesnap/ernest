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
#' The plot is faceted in three frames. The horizontal axis shows values of log.
#' volume: If `x` is an `ernest_run` these estimates are derived from the run,
#' if `x` is an `ernest_estimates` (or `ndraws != 0`), these values are simulated.
#' The three `y` axes are as follows:
#'
#' * **Evidence:** Estimate with a corresponding error ribbon drawn from either
#' the estimated standard error (if `ernest_run`) or drawn from the high density
#' credible interval (HDCI) (if `ernest_sampler`);
#' * **Normalized Likelihood:** The likelihood value of the criteria used
#' to draw new points from the likelihood-restricted prior sampler, normalized
#' by the maximum likelihood generated during the run, and;
#' * **Posterior Weight**: The density of the posterior weights attributed to
#' regions of volume within the prior. If using an `ernest_estimates` object,
#' an error ribbon is drawn with the HDCI of this estimate.
#'
#' @note For `ernest_estimates`, `ndraws` must be sufficiently large to calculate
#' HDI. If HDI calculation fails, the user will be warned and an `ernest_run` plot
#' will be generated instead.
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
  check_number_whole(ndraws, min = 0)
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
autoplot.ernest_estimates <- function(object, ...) {
  check_dots_empty()

  df <- try_fetch({
    max_log_z <- tail(object$log_evidence, 1)
    w <- exp(object$log_weight - max_log_z)
    density_draws <- get_density(object$log_volume, w)
    log_volume <- density_draws$log_volume

    w_68 <- hdci(density_draws$density, width = 0.68)
    w_95 <- hdci(density_draws$density, width = 0.95)
    df_w <- vctrs::vec_rbind(w_68, w_95)
    df_w$log_vol <- rep(log_volume, 2)
    df_w$.label <- "Posterior Weight"

    lst_z <- list()
    draws_evid <- posterior::draws_rvars(
      "log_volume" = object$log_volume,
      "log_evidence" = object$log_evidence
    )
    .draw <- NULL
    log_evidence <- NULL
    posterior::for_each_draw(draws_evid, {
      lst_z[[.draw]] <<- stats::approx(
        log_volume,
        exp(log_evidence),
        xout = !!log_volume,
        rule = 2L
      )$y
    })
    rvar_evid <- do.call(rbind, lst_z) |>
      posterior::rvar()

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
    vctrs::vec_c(df_w, df_ll, df_z)
  },
  error = function(cnd) {
    cli::cli_warn(
      c(
        "Can't plot the provided {.cls ernest_estimates}; plotting an {.cls ernest_run} instead.",
        "i" = "Is `ndraws` large enough to calculate HDIs?"
      )
    )
    make_plot_df(
      log_volume = mean(object$log_volume),
      log_evidence = mean(object$log_evidence),
      log_evidence_var = posterior::sd(object$log_evidence) %|% 0.0,
      log_weight = mean(object$log_weight),
      log_lik = mean(object$log_lik)
    )
  })

  autoplot_run_(df, "HDCI", c(0.95, 0.68), c("95%", "68%"))
}

#' @importFrom ggplot2 geom_ribbon
#' @export
autoplot.ernest_run <- function(object, ...) {
  check_dots_empty()

  df <- make_plot_df(
    log_volume = object$log_volume,
    log_evidence = object$log_evidence,
    log_evidence_var = object$log_evidence_var,
    log_weight = object$log_weight,
    log_lik = object$log_lik
  )

  autoplot_run_(
    df,
    expression(hat(sigma[Z])),
    c(3, 2, 1),
    c(expression(3*sigma), expression(2*sigma), expression(1*sigma))
  )
}

#' Create a data.frame for plotting the results of a run
#' @noRd
make_plot_df <- function(log_volume, log_evidence, log_evidence_var, log_weight, log_lik) {
  df_z <- data.frame(
    "log_vol" = rep(log_volume, 3),
    ".label" = "Evidence",
    ".var" = rep(log_evidence, 3),
    ".width" = rep(c(1,2,3), each = length(log_volume)),
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
  vctrs::vec_c(df_w, df_ll, df_z)
}

autoplot_run_ <- function(df, fill_name, fill_limits, fill_labels) {
  df$.width <- factor(
    as.character(df$.width),
    levels = as.character(fill_limits)
  )

  weight_ribbon <- any(!is.na(df$.width) & df$.label == "Posterior Weight")
  ribbon_df <- if (weight_ribbon) {
    subset(df, df$.label != "Normalized Likelihood")
  } else {
    subset(df, df$.label == "Evidence")
  }

  ggplot(df, aes(x = .data[["log_vol"]], y = .data[[".var"]])) +
    geom_ribbon(
      data = ribbon_df,
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
