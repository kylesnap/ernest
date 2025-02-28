# Helper Functions -----

validate_integer_parameter <- function(x, multiplicand, min = NULL) {
  x <- if (is.integer(x)) {
    x
  } else {
    check_number_decimal(x, allow_infinite = FALSE)
    as.integer(x * multiplicand)
  }
  check_number_whole(x, min = min, allow_infinite = FALSE)
  as.integer(x)
}

#' Compute the nested sampling integral
#'
#' @param log_lik A vector of log-likelihoods in descending order.
#' @param log_vol A vector of log-volumes in ascending order.
#'
#' @return A list with the following components:
#' log_z: Log. evidence
#' log_z_var: Variance of the log. evidence
#' h: Information
#' dh: Differential information
#'
compute_integral <- function(log_lik, log_vol) {
  if (length(log_lik) != length(log_vol)) {
    cli::cli_abort("`log_lik` and `log_vol` must have the same length.")
  }
  if (is.unsorted(log_lik)) {
    cli::cli_abort("`log_lik` should be a vector in ascending order.")
  }
  if (is.unsorted(rev(log_vol), strictly = TRUE)) {
    cli::cli_abort("`log_vol` should be a vector in strictly ascending order.")
  }

  pad_log_lik <- c(-1e300, log_lik)
  d_log_vol <- diff(c(0, log_vol))
  log_d_vol <- log_vol - d_log_vol + log(-expm1(d_log_vol))
  log_d_vol2 <- log_d_vol - log(2)
  d_log_vol <- -diff(c(0, log_vol))

  log_wt <- mapply(
    \(lik1, lik2, ldv) logaddexp(lik1, lik2) + ldv,
    tail(pad_log_lik, -1),
    head(pad_log_lik, -1),
    log_d_vol2
  )

  log_z <- accumulate(log_wt, \(cur, nxt) logaddexp(cur, nxt))
  log_z_max <- tail(log_z, 1)
  h_term <- cumsum(
    exp(tail(pad_log_lik, -1) - log_z_max + log_d_vol2) * tail(pad_log_lik, -1) +
      exp(head(pad_log_lik, -1) - log_z_max + log_d_vol2) * head(pad_log_lik, -1)
  )
  h <- h_term - log_z_max * exp(log_z - log_z_max)
  dh <- diff(c(0, h))
  log_z_var <- abs(cumsum(dh * d_log_vol))

  list(
    "log_lik" = log_lik,
    "log_vol" = log_vol,
    "log_weight" = log_wt,
    "log_z" = log_z,
    "log_z_var" = log_z_var,
    "h" = h
  )
}
