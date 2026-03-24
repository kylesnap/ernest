#' Estimate Evidence using a Nested Sampling Run
#'
#' Computes evidence and related quantities from a nested
#' sampling run, optionally by simulating the volumes of each nested
#' likelihood shell.
#'
#' @param x [[ernest_run]]\cr Results from a nested sampling run.
#' @param ndraws `[integer(1)]`\cr The number of log-volume sequences to
#' simulate.
#' If equal to zero, no simulations will be made, and a one draw vector of
#' log-volumes are produced from the estimates contained in `x`.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns [[tibble::tibble()]] with class `ernest_estimate`.
#'
#' The iterative estimates from the nested sampling run. Contains the following
#' columns:
#' * `log_lik`: `[[double()]]` The log-likelihood of the model.
#' * `log_volume`: `[[double() or rvar]]` The log-volume of the prior space.
#' * `log_weight`: `[[double() or rvar]]` The log weights of the points in the
#' live set.
#' * `log_evidence`: `[[double() or rvar]]` The log-evidence of the model.
#'
#' If `ndraws = 0`, columns are retunred as doubles. Else, they are returned
#' as [[posterior::rvar()]] objects, with `ndraws` rows and the same number of
#' columns as the original `log_volume` and `log_weight` matrices.
#'
#' If `ndraws = 0`,  an additional column is included:
#' * `log_evidence_err`: `[[double()]]` The standard error of the log-evidence.
#'
#' @references Higson, E., Handley, W., Hobson, M., & Lasenby, A. (2019).
#' Nestcheck: Diagnostic Tests for Nested Sampling Calculations. Monthly Notices
#' of the Royal Astronomical Society, 483(2), 2044–2056.
#' \doi{10.1093/mnras/sty3090}
#'
#' @examples
#' # Load an example run
#' data(example_run)
#'
#' # View results as a tibble with `ndraws = 0`.
#' calculate(example_run, ndraws = 0)
#'
#' # Generate 100 simulated log-volume values for each iteration.
#' calculate(example_run, ndraws = 100)
#'
#' @aliases ernest_estimate
#' @export
calculate.ernest_run <- function(x, ndraws = 1000L, ...) {
  check_dots_empty()
  check_number_whole(ndraws, min = 0)
  est_volume <- get_log_vol(x$nlive, niter = x$niter)
  log_vol_rng <- range(est_volume)
  dead_log_vol <- est_volume[x$niter]

  log_lik <- x$weights$log_lik
  log_volume <- get_log_vol(x$nlive, x$niter, ndraws = ndraws)
  log_weight <- get_log_w(log_lik, log_volume)

  result <- if (ndraws == 0) {
    list(
      "log_lik" = log_lik,
      "log_volume" = drop(log_volume),
      "log_weight" = drop(log_weight$log_weight),
      "log_evidence" = drop(log_weight$log_evidence),
      "log_evidence_err" = {
        information <- get_information(
          log_lik,
          log_volume,
          drop(log_weight$log_evidence)
        )
        sqrt(get_log_zvar(information, log_volume))
      }
    )
  } else {
    list(
      "log_lik" = log_lik,
      "log_volume" = posterior::rvar(log_volume),
      "log_weight" = posterior::rvar(log_weight$log_weight),
      "log_evidence" = posterior::rvar(log_weight$log_evidence)
    )
  }

  tibble::new_tibble(
    tibble::as_tibble(result),
    ndraws = ndraws,
    log_vol_rng = log_vol_rng,
    dead_log_vol = dead_log_vol,
    class = "ernest_estimate"
  )
}

#' @importFrom tibble tbl_sum
#' @export
#' @noRd
tbl_sum.ernest_estimate <- function(x, ...) {
  desc <- if (attr(x, "ndraws") == 0) {
    "Expected values"
  } else {
    sprintf("Simulated (`ndraws` = %d)", attr(x, "ndraws"))
  }
  c(
    "<ernest_estimate>" = sprintf("%d niter.", nrow(x)),
    "Log-volumes" = desc
  )
}

#' Compute the nested sampling integral and statistics
#'
#' Calculates the nested sampling integral and related statistics from
#' log-likelihoods and log-volumes.
#'
#' @param log_lik Numeric vector of log-likelihoods in descending order.
#' @param log_volume Numeric vector of log-volumes in ascending order.
#'
#' @return A list containing log-likelihoods, log-volumes, log-weights,
#' log-evidence, log-evidence variance, and information.
#' @noRd
compute_integral <- function(log_lik, log_volume) {
  if (vctrs::vec_size_common(log_lik, log_volume) == 0L) {
    return(list(
      log_lik = double(0),
      log_volume = double(0),
      log_weight = double(0),
      log_evidence = double(0),
      log_evidence_var = double(0),
      information = double(0)
    ))
  }
  log_weight <- get_log_w(log_lik, log_volume)
  information <- get_information(log_lik, log_volume, log_weight$log_evidence)
  log_evidence_var <- get_log_zvar(information, log_volume)

  vctrs::vec_cast_common(
    log_lik = log_lik,
    log_volume = log_volume,
    log_weight = drop(log_weight$log_weight),
    log_evidence = drop(log_weight$log_evidence),
    log_evidence_var = log_evidence_var,
    information = information,
    .to = double()
  )
}

# HELPERS FOR CALCULATING EVIDENCE ------

#' Simulate log-volumes for nested sampling
#'
#' @param nlive The number of points in the prior space.
#' @param niter The number of iterations in the nested sampling run.
#' @param ndraws The number of draws to simulate for each volume.
#'
#' @return A matrix of simulated log-volumes.
#' @noRd
get_log_vol <- function(nlive, niter, ndraws = 0) {
  points <- vctrs::vec_c(rep(nlive, niter), seq(nlive, 1, -1))

  if (ndraws == 0) {
    vol <- cumsum(-1 * (points^-1))
    return(vol)
  }
  vol <- matrix(
    log(stats::runif(ndraws * length(points))) / rep(points, each = ndraws),
    nrow = ndraws
  )
  matrixStats::rowCumsums(vol)
}

#' Calculates log weights for each point in a nested sampling run using the
#' trapezoidal rule in log-space.
#'
#' @param log_lik Log-likelihood values.
#' @param log_volume Log-volume values.
#' @param cum_z Whether to compute cumulative log-evidence values
#' or a single log-evidence value from the final log-weight values.
#'
#' @return A named list containing `log_weight` and `log_evidence`. Both are
#' always matrices.
#' @noRd
get_log_w <- function(log_lik, log_volume, cum_z = TRUE, call = caller_env()) {
  if (!is.matrix(log_lik)) {
    dim(log_lik) <- c(1, length(log_lik))
  }
  if (!is.matrix(log_volume)) {
    dim(log_volume) <- c(1, length(log_volume))
  }
  c(log_lik, log_volume) %<-%
    vctrs::vec_recycle_common(
      log_lik,
      log_volume,
      .call = call
    )
  nrow <- nrow(log_lik)
  ncol <- ncol(log_lik)

  log_dvol <- matrix(0, nrow = nrow, ncol = ncol)
  log_dvol_lead <- log_volume[, seq(1, ncol - 2), drop = FALSE]
  log_dvol_lag <- log_volume[, seq(3, ncol), drop = FALSE]
  log_dvol[, seq(2, ncol - 1)] <- logspace_sub(log_dvol_lead, log_dvol_lag) -
    log(2)

  log_dvol_lag <- matrixStats::rowLogSumExps(log_volume, cols = c(1, 2)) -
    log(2)
  log_dvol[, 1] <- logspace_sub(matrix(0, nrow = nrow(log_dvol)), log_dvol_lag)

  log_dvol[, ncol] <- matrixStats::rowLogSumExps(
    log_volume,
    cols = c(ncol - 1, ncol)
  ) -
    log(2)
  log_weight <- log_dvol + log_lik

  log_evidence <- if (cum_z) {
    get_cum_log_z(log_weight)
  } else {
    matrixStats::rowLogSumExps(log_weight)
  }

  list(
    "log_weight" = log_weight,
    "log_evidence" = log_evidence
  )
}

#' Compute cumulative log-evidence from log weights
#'
#' Calculates the cumulative log-evidence from log weights.
#'
#' @param log_weight A matrix or vector of log-weights.
#'
#' @return A matrix of cumulative log-evidences, with the dimensions of
#' `log_weight`.
#' @noRd
get_cum_log_z <- function(log_weight) {
  if (is.matrix(log_weight)) {
    logspace_cumsum_mat(log_weight)
  } else {
    logspace_cumsum(log_weight)
  }
}

#' Compute information (KL divergence) for nested sampling
#'
#' @param log_lik Numeric vector of log-likelihoods.
#' @param log_volume Numeric vector of log-volumes.
#' @param log_evidence Numeric vector of log-evidence values.
#'
#' @return A numeric vector of information values for each iteration.
#' @noRd
get_information <- function(log_lik, log_volume, log_evidence) {
  log_lik <- drop(log_lik)
  log_volume <- drop(log_volume)
  log_evidence <- drop(log_evidence)

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

#' Compute standard error of log-evidence
#'
#' @param information Numeric vector of information values.
#' @param log_volume Numeric vector of log-volumes for each iteration.
#'
#' @return Numeric vector of variance for log-evidence at each iteration.
#' @noRd
get_log_zvar <- function(information, log_volume) {
  dh <- c(information[1], diff(information))
  abs(cumsum(dh * -diff(c(0, log_volume))))
}

#' Log-space subtraction for nested sampling
#'
#' @param a,b Numeric vectors of equal length.
#'
#' @return `log(exp(a) - exp(b))`, computed in log-space to avoid numerical
#' underflow. A warning is issued and `NaN` is returned when `b > a`.
#' @noRd
logspace_sub <- function(a, b) {
  a + log1p(-exp(b - a))
}
