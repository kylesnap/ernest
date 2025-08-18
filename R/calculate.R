#' Estimate Evidence using a Nested Sampling Run
#'
#' Computes evidence and related quantities from a nested
#' sampling run, optionally by simulating the volumes of each nested
#' likelihood shell.
#'
#' @param x (ernest_run) An `ernest_run` object.
#' @inheritParams rlang::args_dots_empty
#' @param ndraws (positive integer or zero, optional) The number of log volume
#' sequences to simulate. If equal to zero, no simulations will be made, and a
#' one draw vector of log volumes are produced from the estimates contained in
#' `x`. If `NULL`, `getOption("posterior.rvar_ndraws")` is used (default 4000).
#'
#' @returns A tibble, containing `run$n_iter + run$n_points` rows and the
#' following columns:
#' * `log_lik`: The log-likelihood of the model.
#' * `log_volume`: The log volume of the prior space.
#' * `log_weight`: The log weights of the live points.
#' * `log_evidence`: The log evidence of the model.
#' * `log_evidence_err`: The standard error of the log evidence (only available
#' when `ndraws = 0`).
#'
#' The tibble has the additional class `ernest_estimate`, which has its own
#' [plot][plot.ernest_estimate()] method.
#'
#' Each column is returned as an [posterior::rvar()] vector.
#'
#' @details
#' Use `calculate()` to simulate the estimation error nested sampling runs
#' caused by approximating the change in log volume between iterations. Given
#' the use of ordered log likelihood restricted prior sampling, these volumes
#' can be modelled as the order statistics of a uniform random variable.
#' Ernest uses the following distributions to perform this simulation:
#'
#' * Constant number of live points: At each iteration, the point with the
#' lowest likelihood is replaced by a new sample constrained to a higher
#' likelihood region. Under this setup, the shrinkage in prior volume at
#' iteration `i` can be shown to follow the Beta distribution.
#' * Decreasing number of live points: At the end of the sampling run, the
#' remaining live points are added to the dead point set. This introduces a
#' discrete stepwise behavior in volume shrinkage. Ernest uses a transformation
#' of variables drawn from the exponential distribution to simulate the
#' corresponding changes in volume.
#'
#' @references See Appendix A6 of Speagle, J. (2020).
#
#' @examples
#' # Load an example run
#' data(example_run)
#'
#' # View results as a tibble with `ndraws = FALSE` (the default).
#' calculate(example_run)
#'
#' # Generate 100 simulated log volume values for each iteration.
#' calculate(example_run, ndraws = 100)
#'
#' @method calculate ernest_run
#' @export
calculate.ernest_run <- function(x, ..., ndraws = NULL) {
  check_dots_empty()
  ndraws <- ndraws %||% getOption("posterior.rvar_ndraws", 4000L)
  check_number_whole(ndraws, lower = 0L)

  if (ndraws == 0) {
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
    ndraws = ndraws,
    class = "ernest_estimate"
  )
}

#' Summarise an evidence estimate from nested sampling
#'
#' Provides a summary of an `ernest_estimate` object.
#'
#' @param object An `ernest_estimate` object.
#' @inheritParams rlang::args_dots_empty
#'
#' @return
#' A list with summary statistics for the evidence estimate:
#' * `n_iter`: Number of iterations.
#' * `n_points`: Number of live points.
#' * `log_volume`: Mean log volume at the final iteration.
#' * `log_evidence`: Mean log evidence at the final iteration.
#' * `log_evidence_err`: Standard error of the log evidence.
#' ** If `ndraws = 0`, this is computed analytically.
#' ** If `ndraws` is 1 or greater, it is estimated as the standard deviation
#' of `log_evidence` across the draws.
#' * `run`: A [posterior::draws_rvars] of all draws and variables.
#'
#' @seealso [calculate()]
#' @examples
#' data(example_run)
#' calc <- calculate(example_run, ndraws = 2000)
#' summary(calc)
#' @export
summary.ernest_estimate <- function(object, ...) {
  check_dots_empty()
  n_rows <- length(object$log_lik)
  n_draws <- attr(object, "ndraws")

  run_df <- posterior::draws_rvars(
    "log_lik" = object$log_lik,
    "log_volume" = object$log_volume,
    "log_weight" = object$log_weight,
    "log_evidence" = object$log_evidence,
    "log_evidence_err" = if ("log_evidence_err" %in% names(object)) {
      object$log_evidence_err
    } else {
      NULL
    }
  )

  log_volume <- mean(run_df$log_volume[n_rows])
  log_evidence <- mean(run_df$log_evidence[n_rows])
  log_evidence_err <- if ("log_evidence_err" %in% names(object)) {
    mean(run_df$log_evidence_err[n_rows])
  } else {
    posterior::sd(run_df$log_evidence[n_rows])
  }

  structure(
    list(
      "n_draws" = n_draws,
      "log_volume" = log_volume,
      "log_evidence" = log_evidence,
      "log_evidence_err" = log_evidence_err %|% Inf,
      "run" = run_df
    ),
    class = "summary.ernest_estimate"
  )
}

#' Format method for `ernest_estimate`
#'
#' @param x An `ernest_estimate` object.
#' @param ... Additional arguments (not used).
#'
#' @export
#' @noRd
format.ernest_estimate <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_div(theme = list(.val = list(digits = 3)))
    ndraws <- attr(x, "ndraws")
    iter <- length(x$log_lik)
    cli::cli_bullets(c(
      "An {.cls ernest_calculate}: {ndraws} draws x {iter} iterations"
    ))
  })
}

#' Print method for `ernest_estimate`
#'
#' @param x An `ernest_estimate` object.
#' @param ... Forwarded to `format.ernest_estimate()`.
#'
#' @export
#' @noRd
print.ernest_estimate <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Format method for `summary.ernest_estimate`
#'
#' @param x A `summary.ernest_estimate` object.
#' @param ... Additional arguments (not used).
#'
#' @export
#' @noRd
format.summary.ernest_estimate <- function(x, ...) {
  log_z <- formatC(x$log_evidence, digits = 4, format = "fg")
  log_z_sd <- formatC(x$log_evidence_err, digits = 4, format = "fg")
  cli::cli_format_method({
    cli::cli_h1(
      if (x$n_draws == 0L) {
        "Analytical Evidence Estimate from {.cls ernest_estimate}"
      } else {
        "Simulated Evidence Estimate from {.cls ernest_estimate}"
      }
    )
    cli::cli_dl(c(
      "No. Draws" = "{x$n_draws}",
      "Log. Evidence" = "{log_z} (\U00B1 {log_z_sd})"
    ))
  })
}

#' Print method for `summary.ernest_estimate`
#'
#' @param x A `summary.ernest_estimate` object.
#' @param ... Forwarded to `format.summary.ernest_estimate()`.
#'
#' @export
#' @noRd
print.summary.ernest_estimate <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# HELPERS FOR CALCULATING EVIDENCE ------

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
