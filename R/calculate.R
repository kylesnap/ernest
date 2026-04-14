#' Estimate Evidence using a Nested Sampling Run
#'
#' Computes evidence and related quantities from a nested
#' sampling run, optionally by simulating the volumes of each nested
#' likelihood shell.
#'
#' @param x [[ernest_run]]\cr Results from a nested sampling run.
#' @param ndraws `[integer(1)]`\cr The number of log-volume sequences to
#' simulate. If equal to zero, log-volume simulation is skipped and error in
#' the log-evidence estimates is approximated with analytical error
#' estimates.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns An `ernest_estimate` object, which inherits from `tbl_df`, `tbl`,
#' and `data.frame`.
#'
#' The iterative estimates from the nested sampling run. Contains the following
#' columns:
#' * `log_lik`: `[[double()]]` The log-likelihood of the model.
#' * `log_volume`: `[[posterior::rvar()]]` The log-volume of the prior space.
#' * `log_weight`: `[[posterior::rvar()]]` The log weights of the points in the
#' live set.
#' * `log_evidence`: `[[posterior::rvar()]]` The log-evidence of the model.
#'
#' If `ndraws > 0`, `log_volume`, `log_weight`, and `log_evidence` each contain
#' `ndraws` simulated draws per iteration.
#'
#' If `ndraws = 0`, `log_volume` and `log_weight` contain a single
#' deterministic draw per iteration, and `log_evidence` contains draws
#' from a normal approximation based on analytical variance estimates (see
#' the package vignetttes for more information). The  number of draws is
#' controlled with getOption("posterior.rvar_ndraws"), with a default of 1000.
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
#' # View results and analytical evidence errors.
#' calculate(example_run, ndraws = 0)
#'
#' # Simulate 100 log-volume shrinkage sequences across the run.
#' calculate(example_run, ndraws = 100)
#'
#' @aliases ernest_estimate
#' @export
calculate.ernest_run <- function(x, ndraws = 1000L, ...) {
  check_dots_empty()
  check_number_whole(ndraws, min = 0)
  x_rcrd <- x$rcrd
  est_volume <- get_log_vol(x_rcrd)
  dead_log_vol <- est_volume[x$niter]

  log_lik <- field(x_rcrd, "log_lik")
  log_volume <- get_log_vol(x_rcrd, ndraws = ndraws)
  log_weight <- get_log_w(log_lik, log_volume)

  if (ndraws == 0) {
    withr::local_preserve_seed()
    check_installed("distributional", "for evidence error estimation")
    information <- get_information(log_lik, log_volume, log_weight$log_evidence)
    log_z_dist <- distributional::dist_normal(
      mu = log_weight$log_evidence[1, ],
      sd = sqrt(get_log_zvar(information, log_volume))
    )
    log_z <- posterior::rvar(t(do.call(
      rbind,
      generate(log_z_dist, times = getOption("posterior.rvar_ndraws", 1000))
    )))
    log_volume <- posterior::as_rvar(drop(log_volume))
    log_weight <- posterior::as_rvar(drop(log_weight$log_weight))
  } else {
    log_z_dist <- NULL
    log_z <- posterior::rvar(log_weight$log_evidence)
    log_volume <- posterior::rvar(log_volume)
    log_weight <- posterior::rvar(log_weight$log_weight)
  }
  result <- vctrs::df_list(
    "log_lik" = log_lik,
    "log_volume" = log_volume,
    "log_weight" = log_weight,
    "log_evidence" = log_z
  )

  new_tibble0(
    x = result,
    ndraws = as.integer(ndraws),
    log_z_dist = log_z_dist,
    dead_log_vol = dead_log_vol,
    class = "ernest_estimate"
  )
}

#' Compute the nested sampling integral and statistics
#'
#' Calculates the nested sampling integral and related statistics from
#' an `ernest_rcrd` object.
#'
#' @param x_rcrd Nested sampling samples as an `ernest_rcrd` object.
#'
#' @return A list containing log-likelihoods, log-volumes, log-weights,
#' log-evidence, log-evidence variance, and information.
#' @noRd
compute_integral <- function(x_rcrd) {
  log_lik <- field(x_rcrd, "log_lik")
  log_volume <- get_log_vol(x_rcrd)
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
#' @param x_rcrd The nested sampling record.
#' @param ndraws The number of draws to simulate for each volume.
#' @param call Error information.
#'
#' @return A matrix of simulated log-volumes.
#' @noRd
get_log_vol <- function(x_rcrd, ndraws = 0, call = caller_env()) {
  if (is.unsorted(field(x_rcrd, "log_lik"))) {
    cli::cli_warn(
      c(
        "Log-weight estimates are unreliable.",
        "!" = "`log_lik` values are not sorted in increasing order."
      ),
      call = call
    )
  }
  points <- field(x_rcrd, "nlive")

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
    logspace_cumsum_mat(log_weight)
  } else {
    matrixStats::rowLogSumExps(log_weight)
  }

  list(
    "log_weight" = log_weight,
    "log_evidence" = log_evidence
  )
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
