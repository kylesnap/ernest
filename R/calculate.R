#' Estimate Evidence using a Nested Sampling Run
#'
#' @param x An `ernest_run` object.
#' @inheritParams rlang::args_dots_empty
#' @param ndraws A positive integer or a boolean. If `FALSE`, the log volumes
#' will be derived from their expected values. If not `FALSE`, the log volumes
#' will be simulated using `ndraws` samples from each volume's
#' joint distribution.
#'
#' @returns A tibble, containing the following columns:
#' * `log_lik`: The log-likelihood of the model.
#' * `log_volume`: The log volume of the prior space.
#' * `log_weight`: The log weights of the live points.
#' * `log_evidence`: The log evidence of the model.
#'
#' These are all returned as `posterior::rvar()` vectors, which allows one vector
#' to store `ndraws` samples.
#'
#' Additionally, in the case of `ndraws = 0`, the following column is also provided:
#' * `log_evidence.err`: The standard error of the log evidence.
#'
#' @export
calculate.ernest_run <- function(x, ..., ndraws = FALSE) {
  check_dots_empty()
  if (is.logical(ndraws)) ndraws <- as.integer(ndraws)
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
  log_volume <- posterior::rdo(sim_volume(!!x$n_points, !!x$n_iter), ndraws = ndraws)

  log_weight <- posterior::rdo(get_logweight(log_lik, log_volume))
  log_evidence <- posterior::rdo(get_logevid(log_weight))

  tibble::new_tibble(
    list(
      "log_lik" = posterior::as_rvar(log_lik),
      "log_volume" = log_volume,
      "log_weight" = log_weight,
      "log_evidence" = log_evidence
    ),
    ndraws = ndraws,
    class = "ernest_estimates"
  )
}

sim_volume <- function(n_points, n_iter) {
  volumes <- numeric(n_iter + n_points)

  vctrs::vec_slice(volumes, 1:n_iter) <- rbeta(n_iter, n_points, 1)

  nstart <- 500
  bound <- c(n_iter, n_iter+n_points)
  sn <- seq(n_points, 1)

  y_arr <- rexp(nstart + 1, rate = 1.0)
  append <- c(nstart, sn - 1)
  ycsum <- cumsum(y_arr)
  ycsum <- ycsum / ycsum[length(ycsum)]
  uorder <- ycsum[append + 1] # R is 1-indexed

  rorder <- uorder[-1] / uorder[-length(uorder)]
  vctrs::vec_slice(volumes, (bound[1] + 1):bound[2]) <- rorder

  cumsum(log(volumes))
}

get_logweight <- function(log_lik, log_volume) {
  diff_volume <- diff(c(0, log_volume))
  vol_term <- log_volume - diff_volume + log(-expm1(diff_volume))

  log_lik <- c(-1e300, log_lik)
  idx <- seq(2, length(log_lik))
  lik_term <- logaddexp_vec(log_lik[idx], log_lik[idx - 1])
  log_weight <- lik_term + vol_term + log(0.5)
  log_weight
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
