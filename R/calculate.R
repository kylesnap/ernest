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
#' @references Skilling, J. (2006). Nested Sampling for General
#' Bayesian Computation. Bayesian Analysis, 1(4), 833–859.
#' <https://doi.org/10.1214/06-BA127>
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
  log_volume <- sim_volume(x$n_points, x$n_iter, ndraws = ndraws)
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
#' @param ndraws Integer. The number of draws to simulate for each volume.
#'
#' @return A matrix of simulated log-volumes with dimensions `ndraws` by 
#' `n_iter + n_points`.
#' @noRd
sim_volume <- function(n_points, n_iter, ndraws) {
  volumes <- matrix(nrow = ndraws, ncol = (n_points + n_iter))
  volumes[, seq(1, to = n_iter)] <- stats::rbeta(n_iter, n_points, 1)

  bound <- c(n_iter, n_iter + n_points)
  sn <- seq(n_points, 1)

  y_arr <- matrix(
    stats::rexp((n_points + 1) * ndraws, rate = 1.0),
    nrow = ndraws
  )
  ycsum <- t(apply(y_arr, 1, cumsum))
  ycsum <- ycsum / ycsum[, n_points + 1]
  append <- c(n_points, sn - 1)
  uorder <- ycsum[, append + 1, drop = FALSE]

  rorder <- uorder[, -1, drop = FALSE] / uorder[, -ncol(uorder), drop = FALSE]
  volumes[, (bound[1] + 1):bound[2]] <- rorder

  matrixStats::rowCumsums(log(volumes))
}

#' Compute log weights for nested sampling
#'
#' Calculates log weights for each point in a nested sampling run using the
#' trapezoidal rule in log-space.
#'
#' @param log_lik Numeric vector of log-likelihoods from the run.
#' @param log_volume Numeric vector or matrix of log-volume estimates.
#'
#' @return A vector or matrix of log weights, matching the shape of
#' `log_volume`.
#' @noRd
get_logweight <- function(log_lik, log_volume) {
  is_matrix <- if (!is.matrix(log_volume)) {
    log_volume <- matrix(log_volume, nrow = 1)
    FALSE
  } else {
    TRUE
  }
  n <- ncol(log_volume)

  diff_volume <- cbind(
    log_volume[, 1],
    matrixStats::rowDiffs(log_volume)
  )
  vol_term <- log_volume - diff_volume + log(-expm1(diff_volume))

  log_lik <- c(-1e300, log_lik)
  idx <- seq(2, length(log_lik))
  lik_term <- matrixStats::rowLogSumExps(
    c(log_lik[idx], log_lik[idx - 1]),
    dim. = c(n, 2)
  ) +
    log(0.5)

  vol_term <- sweep(vol_term, 2, lik_term, FUN = "+")
  if (!is_matrix) {
    vol_term <- drop(vol_term)
  }
  vol_term
}

#' Compute cumulative log-evidence from log weights
#'
#' Calculates the cumulative log-evidence from log weights.
#'
#' @param log_weight Numeric vector or matrix of log weights.
#'
#' @return A vector or matrix of cumulative log-evidence.
#' @noRd
get_logevid <- function(log_weight) {
  if (is.matrix(log_weight)) {
    for (i in seq(2, ncol(log_weight))) {
      log_weight[, i] <- matrixStats::rowLogSumExps(
        log_weight,
        cols = c(i - 1, i)
      )
    }
    log_weight
  } else {
    logcumsumexp(log_weight)
  }
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
