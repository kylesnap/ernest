#' Estimate Evidence using a Nested Sampling Run
#'
#' Computes evidence and related quantities from a nested
#' sampling run, optionally by simulating the volumes of each nested
#' likelihood shell.
#'
#' @param x An `ernest_run` object.
#' @inheritParams rlang::args_dots_empty
#' @param ndraws An optional positive integer. The number of log-volume
#' sequences to simulate. If equal to zero, no simulations will be made, and a
#' one draw vector of log-volumes are produced from the estimates contained in
#' `x`. If `NULL`, `getOption("posterior.rvar_ndraws")` is used (default 4000).
#'
#' @returns A [tibble::tibble()], containing `n_iter + n_points` rows
#' and several columns:
#'
#' * `log_lik`: The log-likelihood of the model.
#' * `log_volume`: The log-volume of the prior space.
#' * `log_weight`: The log weights of the live points.
#' * `log_evidence`: The log-evidence of the model.
#' * `log_evidence_err`: The standard error of the log-evidence (only available
#' when `ndraws = 0`).
#'
#' The tibble has the additional class `ernest_estimate`, which has its own
#' [plot][plot.ernest_estimate()] method.
#'
#' Each column is returned as an [posterior::rvar()] vector.
#'
#' @references Higson, E., Handley, W., Hobson, M., & Lasenby, A. (2019).
#' Nestcheck: Diagnostic Tests for Nested Sampling Calculations. Monthly Notices
#' of the Royal Astronomical Society, 483(2), 2044–2056.
#' <https://doi.org/10.1093/mnras/sty3090>
#'
#' @examples
#' # Load an example run
#' data(example_run)
#'
#' # View results as a tibble with `ndraws = FALSE` (the default).
#' calculate(example_run)
#'
#' # Generate 100 simulated log-volume values for each iteration.
#' calculate(example_run, ndraws = 100)
#'
#' @aliases ernest_estimate
#' @export
calculate.ernest_run <- function(x, ..., ndraws = NULL) {
  check_dots_empty()
  ndraws <- ndraws %||% getOption("posterior.rvar_ndraws", 4000L)
  check_number_whole(ndraws, lower = 0L)

  if (ndraws == 0L) {
    return(tibble::new_tibble(
      list(
        "log_lik" = posterior::as_rvar(x$log_lik),
        "log_volume" = posterior::as_rvar(x$log_volume),
        "log_weight" = posterior::as_rvar(x$log_weight),
        "log_evidence" = posterior::as_rvar(x$log_evidence),
        "log_evidence_err" = posterior::as_rvar(sqrt(x$log_evidence_var))
      ),
      ndraws = 0L,
      class = "ernest_estimate"
    ))
  }

  log_lik <- x$log_lik
  log_volume <- get_logvol(x$n_points, x$n_iter, ndraws = ndraws)
  log_weight <- get_logweight(log_lik, log_volume)
  log_evidence <- get_logevid(log_weight)

  tibble::new_tibble(
    list(
      "log_lik" = posterior::as_rvar(log_lik),
      "log_volume" = posterior::rvar(log_volume),
      "log_weight" = posterior::rvar(log_weight),
      "log_evidence" = posterior::rvar(log_evidence)
    ),
    ndraws = as.integer(ndraws),
    class = "ernest_estimate"
  )
}

#' @export
#' @noRd
format.ernest_estimate <- function(x, ...) {
  vec <- cli::cli_format_method({
    cli::cli_text("Nested sampling estimates {.cls ernest_estimate}")
    if (attr(x, "ndraws") != 0) {
      cli::cli_text("No. of Simulated Draws: {attr(x, 'ndraws')}")
    }
    log_z <- tail(x$log_evidence, 1)
    log_vol <- tail(x$log_volume, 1)
    cli::cli_text("Log. Volume: {.val {log_vol}}")
    cli::cli_text("Log. Evidence: {.val {log_z}}")
  })
  c(vec, NextMethod(x))
}

#' @export
#' @noRd
print.ernest_estimate <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# HELPERS FOR CALCULATING EVIDENCE ------

#' Simulate log-volumes for nested sampling
#'
#' Simulates log-volumes for dead and live points in a nested sampling run.
#'
#' @param n_points Integer. The number of points in the prior space.
#' @param n_iter Integer. The number of iterations in the nested sampling run.
#' @param ndraws Integer. The number of draws to simulate for each volume, or
#' NULL.
#'
#' @return A matrix of simulated log-volumes with dimensions `ndraws` by
#' `n_iter + n_points`. If ndraws is NULL, these are the expected values.
#' @noRd
get_logvol <- function(n_points, n_iter, ndraws = NULL) {
  points <- vctrs::vec_c(rep(n_points, n_iter), seq(n_points, 1, -1))

  if (is.null(ndraws)) {
    vol <- -1 * (points^-1)
    matrix(cumsum(vol), nrow = 1)
  } else {
    vol <- matrix(stats::runif(ndraws * length(points)), nrow = ndraws)
    vol <- log(vol)
    matrixStats::rowCumsums(sweep(vol, 2, points, "/"))
  }
}

#' Compute log weights for nested sampling
#'
#' Calculates log weights for each point in a nested sampling run using the
#' trapezoidal rule in log-space.
#'
#' @param log_lik Numeric vector of log-likelihoods from the run.
#' @param log_volume A matrix of log-volume estimates, with each row being a
#' draw.
#'
#' @return A matrix of log weights, matching the shape of `log_volume`.
#' @noRd
get_logweight <- function(log_lik, log_volume) {
  if (!is.matrix(log_volume)) {
    dim(log_volume) <- c(1, length(log_volume))
  }
  ndraws <- nrow(log_volume)
  ncol <- ncol(log_volume)
  log_dvol <- matrix(0, nrow = nrow(log_volume), ncol = ncol(log_volume))

  log_dvol_lead <- log_volume[, seq(1, ncol - 2), drop = FALSE]
  log_dvol_lag <- log_volume[, seq(3, ncol), drop = FALSE]
  log_dvol[, seq(2, ncol - 1)] <- logspace_sub(log_dvol_lead, log_dvol_lag) -
    log(2)

  log_dvol_lag <- matrixStats::rowLogSumExps(log_volume, cols = c(1, 2)) -
    log(2)
  log_dvol[, 1] <- logspace_sub(matrix(0, nrow = ndraws), log_dvol_lag)

  log_dvol[, ncol] <- matrixStats::rowLogSumExps(
    log_volume,
    cols = c(ncol - 1, ncol)
  ) -
    log(2)
  sweep(log_dvol, 2, log_lik, "+")
}

#' Compute cumulative log-evidence from log weights
#'
#' Calculates the cumulative log-evidence from log weights.
#'
#' @param log_weight A matrix of log-weights, each row a draw from log_volume.
#'
#' @return A matrix of cumulative log-evidences, with the dimensions of
#' `log_weight`.
#' @noRd
get_logevid <- function(log_weight) {
  if (!is.matrix(log_weight)) {
    dim(log_weight) <- c(1, length(log_weight))
  }
  log_z <- log_weight
  for (i in seq(2, ncol(log_weight))) {
    log_z[, i] <- matrixStats::rowLogSumExps(
      log_z,
      cols = c(i - 1, i)
    )
  }
  log_z
}

#' Compute information (KL divergence) for nested sampling
#'
#' Calculates the information (Kullback-Leibler divergence) for a nested
#' sampling run.
#'
#' @param log_lik Numeric vector of log-likelihoods.
#' @param log_volume Numeric vector of log-volumes.
#' @param log_evidence Numeric vector of log-evidence values.
#'
#' @return A numeric vector of information values for each iteration.
#' @noRd
get_information <- function(log_lik, log_volume, log_evidence) {
  loglstar_pad <- c(-1e300, log_lik)
  dlogvol <- diff(c(0, log_volume))
  logdvol <- log_volume - dlogvol + log1p(-exp(dlogvol))
  logdvol2 <- logdvol + log(0.5)
  max_logz <- log_evidence[length(log_evidence)]

  exp1 <- exp(loglstar_pad[-1] - max_logz + logdvol2)
  exp2 <- exp(loglstar_pad[-length(loglstar_pad)] - max_logz + logdvol2)
  h_part1 <- cumsum(
    exp1 * loglstar_pad[-1] + exp2 * loglstar_pad[-length(loglstar_pad)]
  )
  h_part1 - max_logz * exp(log_evidence - max_logz)
}

#' Log Subtract Helper
#'
#' @param a,b Matrices, where (elementwise) a > b.
#'
#' @returns LSE(a, b), or log(exp(a) + exp(b))
#' @noRd
logspace_sub <- function(a, b) {
  a + log1p(-exp(b - a))
}
