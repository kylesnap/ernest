#' Estimate Evidence using a Nested Sampling Run
#'
#' @param x An `ernest_run` object.
#' @inheritParams rlang::args_dots_empty
#' @param ndraws A positive integer or a boolean. If `FALSE`, the log volumes
#' will be derived from their expected values. If not `FALSE`, the log volumes
#' will be simulated using `ndraws` samples from each volume's
#' joint distribution.
#' @param approx_live If `ndraws` is not `FALSE`, whether to approximate the
#' distribution of log volumes of the live points using their associated
#' marginals.
#'
#' @returns A tibble.
#' @export
calculate.ernest_run <- function(x, ..., ndraws = FALSE, approx_live = FALSE) {
  check_dots_empty()
  if (is.logical(ndraws)) ndraws <- as.integer(ndraws)
  check_number_whole(ndraws, min = 0)
  check_bool(approx_live)

  log_volume <- if (ndraws == 0) {
    x$log_volume
  } else {
    t(replicate(ndraws, sim_volume(x$n_points, x$n_iter, approx_live)))
  }
  log_lik <- x$log_lik

  log_weight <- if (is.matrix(log_volume)) {
    t(apply(log_volume, 1, get_logweight, log_lik = log_lik))
  } else {
    get_logweight(log_lik, log_volume)
  }
  log_evidence <- if (is.matrix(log_weight)) {
    t(apply(log_weight, 1, get_logevid))
  } else {
    get_logevid(log_weight)
  }

  if (ndraws == 0) {
    tibble::tibble(
      "log_lik" = log_lik,
      "log_volume" = log_volume,
      "log_weight" = log_weight,
      "log_evidence" = log_evidence,
      "information" = get_information(log_lik, log_volume, log_evidence)
    )
  } else {
    tibble::tibble(
      "log_lik" = posterior::as_rvar(log_lik),
      "log_volume" = posterior::rvar(log_volume),
      "log_weight" = posterior::rvar(log_weight),
      "log_evidence" = posterior::rvar(log_evidence)
    )
  }
}

sim_volume <- function(n_points, n_iter, approx_live = FALSE) {
  volumes <- numeric(n_iter + n_points)

  if (approx_live) {
    points <- c(rep(n_points, n_iter), seq(n_points, 1))
    volumes <- rbeta(n_iter + n_points, points, 1)
  } else {
    vctrs::vec_slice(volumes, 1:n_iter) <- rbeta(
      n_iter,
      n_points,
      1
    )
    live_vol <- rexp(n_points + 1, rate = 1)
    live_vol <- cumsum(live_vol)
    live_vol <- live_vol / tail(live_vol, 1)
    live_vol <- live_vol[c(n_points, n_points:1)]
    vctrs::vec_slice(volumes, (n_iter + 1):length(volumes)) <-
      live_vol[-1] / live_vol[-length(live_vol)]
  }

  cumsum(log(volumes))
}

get_logweight <- function(log_lik, log_volume) {
  diff_volume <- diff(c(0, log_volume))
  vol_term <- log_volume - diff_volume + log(-expm1(diff_volume))

  log_lik <- c(-1e300, log_lik)
  idx <- seq(2, length(log_lik))
  lik_term <- logaddexp_vec(log_lik[idx], log_lik[idx - 1])
  lik_term + vol_term + log(0.5)
}

get_logevid <- function(log_weight) {
  logcumsumexp(log_weight)
}

get_information <- function(log_lik, log_volume, log_evidence) {
  loglstar_pad <- c(-1e300, log_lik)
  dlogvol <- diff(c(0, log_volume))
  logdvol <- log_volume - dlogvol + log1p(-exp(dlogvol))
  logdvol2 <- logdvol + log(0.5)
  max_logz <- log_evidence[length(log_evidence)]

  exp1 <- exp(loglstar_pad[-1] - max_logz + logdvol2)
  exp2 <- exp(loglstar_pad[-length(loglstar_pad)] - max_logz + logdvol2)
  h_part1 <- cumsum(exp1 * loglstar_pad[-1] + exp2 * loglstar_pad[-length(loglstar_pad)])
  h_part1 - max_logz * exp(log_evidence - max_logz)
}
