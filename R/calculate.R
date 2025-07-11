#' Estimate Evidence using a Nested Sampling Run
#'
#' @param x An `ernest_run` object.
#' @inheritParams rlang::args_dots_empty
#' @param ndraws A positive integer or a boolean. If `FALSE`, the log volumes
#' will be derived from their expected values. If not `FALSE`, the log volumes
#' will be simulated using `ndraws` samples from each volume's
#' joint distribution.
#'
#' @returns A tibble, containing `run$n_iter + run$n_points` rows and the
#' following columns:
#' * `log_lik`: The log-likelihood of the model.
#' * `log_volume`: The log volume of the prior space.
#' * `log_weight`: The log weights of the live points.
#' * `log_evidence`: The log evidence of the model.
#' * `log_evidence.err`: The standard error of the log evidence (only available
#' when `ndraws = 0`).
#'
#' The tibble has the additional class `ernest_estimates`, which has its own
#' [plot][plot.ernest_estimates()] method.
#'
#' Each column is returned as an [posterior::rvar()] vector.
#' @examples
#' # Load an example run
#' data(ernest_run_example)
#'
#' # View results as a tibble with `ndraws = FALSE` (the default).
#' calculate(ernest_run_example)
#'
#' # Generate 100 simulated log volume values for each iteration.
#' calculate(ernest_run_example, ndraws = 100)
#'
#' @method calculate ernest_run
#' @export
calculate.ernest_run <- function(x, ..., ndraws = FALSE) {
  check_dots_empty()
  if (is.logical(ndraws)) {
    ndraws <- as.integer(ndraws)
  }
  check_number_whole(ndraws, min = 0)

  if (ndraws == 0) {
    return(tibble::new_tibble(
      list(
        "log_lik" = posterior::as_rvar(x$log_lik),
        "log_volume" = posterior::as_rvar(x$log_volume),
        "log_weight" = posterior::as_rvar(x$log_weight),
        "log_evidence" = posterior::as_rvar(x$log_evidence),
        "log_evidence.err" = posterior::as_rvar(sqrt(x$log_evidence_var))
      ),
      ndraws = 0L,
      class = "ernest_estimates"
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
    ndraws = ndraws,
    class = "ernest_estimates"
  )
}

#' Simulate log volumes for nested sampling
#'
#' @param n_points The number of points in the prior space.
#' @param n_iter The number of iterations in the nested sampling run.
#' @param ndraws The number of draws to simulate for each volume.
#'
#' @return An (ndraws * (n_iter + n_points)) vector of log volumes. The n_iter
#' dead points are simulated from the order statistics of the uniform
#' distribution; the n_points live points are simulated from the order
#' statistics of the exponential distribution.
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
#' @param log_lik The log likelihoods from the run
#' @param log_volume Either a vector or a matrix containing log volume
#' estimates
#' @returns A vector or column, in the same shape as log_volume, with the
#' log weight of each point. More generally, this applies the trapezoidal rule
#' in log-space.
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

#' Compute cumulative log evidence from log weights
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
