#' Compute the integral table
#'
#' @param log_lik Vector of log likelihoods
#' @param log_vol Vector of log volumes
#'
#' @return The integral
#'
#' @importFrom utils head
#' @importFrom utils tail
#' @noRd
compute_integral <- function(log_lik, log_vol) {
  pad_log_lik <- c(-1e300, log_lik)
  d_log_vol <- diff(c(0, log_vol))
  log_d_vol <- log_vol - d_log_vol + log1p(-exp(d_log_vol))
  log_d_vol2 <- log_d_vol + log(0.5)

  d_log_vol <- -diff(c(0, log_vol))

  log_wt <- map2_dbl(
    tail(pad_log_lik, -1),
    head(pad_log_lik, -1),
    \(x, y) logaddexp(x, y)
  ) + log_d_vol2

  log_z <- accumulate(log_wt, \(x, y) logaddexp(x, y))
  log_z_max <- tail(log_z, 1)

  h_part1 <- cumsum(
    sum(
      exp(tail(pad_log_lik, -1) - log_z_max + log_d_vol2) *
        tail(pad_log_lik, -1),
      exp(head(pad_log_lik, -1) - log_z_max + log_d_vol2) *
        head(pad_log_lik, -1)
    )
  )
  h <- h_part1 - log_z_max * exp(log_z - log_z_max)
  dh <- diff(c(0, h))
  log_z_var <- abs(cumsum(dh * d_log_vol))

  tibble::tibble(
    "log_lik" = log_lik,
    "log_vol" = log_vol,
    "log_weight" = log_wt,
    "log_z" = log_z,
    "log_z_var" = log_z_var,
    "information" = h
  )
}

#' Check that the args in 'x' are unique (from hardhat!)
#' @noRd
check_unique_names <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  nms <- names(x)
  if (length(nms) != length(x) || any(is.na(nms) | nms == "")) {
    cli::cli_abort("All elements of {.arg {arg}} must be named.")
  }
  if (anyDuplicated(nms)) {
    cli::cli_abort("All elements of {.arg {arg}} must have unique names.")
  }
  !anyDuplicated(nms)
  return(invisible(NULL))
}

# This function takes in a vector of distributions, and returns a vector of expressions.
# These expression are EITHER a base-R expression to calculate each distribution's IPT, or
# returns an expression for calculating an arbitrary quantile.
get_quantile_exprs <- function(v_dist) {
  v_dist <- unname(vctrs::vec_data(v_dist))
  p <- sym("p")
  v_expr <- imap(v_dist, function(x, i) {
    switch(
      class(x)[1],
      "dist_beta" = expr(stats::qbeta(p[!!i], !!x[["shape1"]], !!x[["shape2"]])),
      "dist_binomial" = expr(stats::qbinom(p[!!i], !!x[["n"]], !!x[["p"]])),
      "dist_cauchy" = expr(stats::qcauchy(p[!!i], !!x[["location"]], !!x[["scale"]])),
      "dist_chisq" = expr(stats::qchisq(p[!!i], !!x[["df"]], !!x[["ncp"]])),
      "dist_exponential" = expr(stats::qexp(p[!!i], !!x[["rate"]])),
      "dist_f" = if(is.null(x[["ncp"]])) {
        expr(stats::qf(p[!!i], !!x[["df1"]], !!x[["df2"]]))
      } else {
        expr(stats::qf(p[!!i], !!x[["df1"]], !!x[["df2"]], !!x[["ncp"]]))
      },
      "dist_gamma" = expr(stats::qgamma(p[!!i], !!x[["shape"]], !!x[["rate"]])),
      "dist_geometric" = expr(stats::qgeom(p[!!i], !!x[["p"]])),
      "dist_hypergeometric" = expr(stats::qhyper(p[!!i], !!x[["m"]], !!x[["n"]], !!x[["k"]])),
      "dist_logistic" = expr(stats::dlogis(p[!!i], !!x[["l"]], !!x[["s"]])),
      "dist_lognormal" = expr(stats::dlnorm(p[!!i], !!x[["mu"]], !!x[["sigma"]])),
      "dist_negbin" = expr(stats::qnbinom(p[!!i], !!x[["n"]], !!x[["p"]])),
      "dist_normal" = expr(stats::dnorm(p[!!i], !!x[["mu"]], !!x[["sigma"]])),
      "dist_poisson" = expr(stats::qpois(p[!!i], !!x[["l"]])),
      "dist_student_t" = {
        if (is.null(x[["ncp"]])) {
          expr(stats::qt(p[!!i], !!x[["df"]]) * !!x[["sigma"]] + !!x[["mu"]])
        } else {
          expr(stats::qt(p[!!i], !!x[["df"]], !!x[["ncp"]]) * !!x[["sigma"]] + !!x[["mu"]])
        }
      },
      "dist_uniform" = expr(stats::qunif(p[!!i], !!x[["l"]], !!x[["u"]])),
      "dist_weibull" = expr(stats::qweibull(p[!!i], !!x[["shape"]], !!x[["scale"]])),
      expr(stats::quantile(!!x, p[!!i]))
    )
  })
  fn_body <- expr(c(!!!v_expr))
  new_function(
    exprs(p = ),
    fn_body
  )
}
