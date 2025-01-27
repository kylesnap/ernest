#' Compute the integral table
#'
#' @param log_lik Vector of log likelihoods
#' @param log_vol Vector of log volumes
#'
#' @return The integral
#' @noRd
compute_integral <- function(log_lik, log_vol) {
  pad_log_lik <- c(-1e300, log_lik)
  d_log_vol <- diff(c(0, log_vol))
  log_d_vol <- log_vol - d_log_vol + log1p(-exp(d_log_vol))
  log_d_vol2 <- log_d_vol + log(0.5)

  d_log_vol <- -diff(c(0, log_vol))

  log_wt <- map2_dbl(
    tail(pad_log_lik, -1),
    head(pad_log_lik, -1),
    \(x, y) logaddexp(x, y)
  ) + log_d_vol2

  log_z <- accumulate(log_wt, \(x, y) logaddexp(x, y))
  log_z_max <- tail(log_z, 1)

  h_part1 <- cumsum(
    sum(
      exp(tail(pad_log_lik, -1) - log_z_max + log_d_vol2) *
        tail(pad_log_lik, -1),
      exp(head(pad_log_lik, -1) - log_z_max + log_d_vol2) *
        head(pad_log_lik, -1)
    )
  )
  h <- h_part1 - log_z_max * exp(log_z - log_z_max)
  dh <- diff(c(0, h))
  log_z_var <- abs(cumsum(dh * d_log_vol))

  tibble::tibble(
    "log_lik" = log_lik,
    "log_vol" = log_vol,
    "log_weight" = log_wt,
    "log_z" = log_z,
    "log_z_var" = log_z_var,
    "information" = h
  )
}

#' Check that the args in 'x' are unique (from hardhat!)
#' @noRd
check_unique_names <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  nms <- names(x)
  if (length(nms) != length(x) || any(is.na(nms) | nms == "")) {
    cli::cli_abort("All elements of {.arg {arg}} must be named.")
  }
  if (anyDuplicated(nms)) {
    cli::cli_abort("All elements of {.arg {arg}} must have unique names.")
  }
  !anyDuplicated(nms)
  return(invisible(NULL))
}
