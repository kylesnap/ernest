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
#' * `log_lik`: [[rvar]] The log-likelihood of the model.
#' * `log_volume`: [[rvar]] The log-volume of the prior space.
#' * `log_weight`: [[rvar]] The log weights of the points in the live set.
#' * `log_evidence`: [[rvar]] The log-evidence of the model.
#'
#' If `ndraws = 0`, an additional column is included:
#' * `log_evidence_err`: [[rvar]] The standard error of the log-evidence.
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
  nlive <- x$nlive
  log_vol <- drop(get_logvol(x$nlive, niter = x$niter))
  log_vol_rng <- range(log_vol)
  dead_log_vol <- log_vol[x$niter]

  if (ndraws == 0L) {
    integration <- compute_integral(x$weights$log_lik, log_vol)
    return(tibble::new_tibble(
      list(
        "log_lik" = posterior::as_rvar(integration$log_lik),
        "log_volume" = posterior::as_rvar(integration$log_volume),
        "log_weight" = posterior::as_rvar(integration$log_weight),
        "log_evidence" = posterior::as_rvar(integration$log_evidence),
        "log_evidence_err" = posterior::as_rvar(sqrt(
          integration$log_evidence_var
        ))
      ),
      ndraws = ndraws,
      nlive = nlive,
      log_vol_rng = log_vol_rng,
      dead_log_vol = dead_log_vol,
      class = "ernest_estimate"
    ))
  }

  log_lik <- x$weights$log_lik
  log_volume <- get_logvol(x$nlive, x$niter, ndraws = ndraws)
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
    nlive = nlive,
    log_vol_rng = log_vol_rng,
    dead_log_vol = dead_log_vol,
    class = "ernest_estimate"
  )
}

#' @export
#' @noRd
print.ernest_estimate <- function(x, ...) {
  cli::cli_text("Nested sampling uncertainty estimates:")
  log_z <- round(x$log_evidence[length(x$log_evidence)], 4)
  log_vol <- round(x$log_volume[length(x$log_volume)], 4)
  cli::cli_bullets(c(
    "# of Simulated Draws: {attr(x, 'ndraws')}",
    "Log-volume: {log_vol}",
    "Log-evidence: {log_z}"
  ))
}

# HELPERS FOR CALCULATING EVIDENCE ------

#' Simulate log-volumes for nested sampling
#'
#' Simulates log-volumes for points in a nested sampling run.
#'
#' @param nlive The number of points in the prior space.
#' @param niter The number of iterations in the nested sampling run.
#' @param ndraws The number of draws to simulate for each volume, or NULL.
#'
#' @return A matrix of simulated log-volumes with dimensions `ndraws` by
#' `niter + nlive`. If ndraws is NULL, these are the expected values.
#' @noRd
get_logvol <- function(nlive, niter, ndraws = NULL) {
  points <- vctrs::vec_c(rep(nlive, niter), seq(nlive, 1, -1))

  if (is.null(ndraws)) {
    vol <- -1 * (points^-1)
    return(matrix(cumsum(vol), nrow = 1))
  }
  vol <- matrix(
    log(stats::runif(ndraws * length(points))) / rep(points, each = ndraws),
    nrow = ndraws
  )
  matrixStats::rowCumsums(vol)
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
