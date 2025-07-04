# Helpers for validating user input -----

#' Round a number to an integer after scaling by a multiplicand.
#'
#' @param x A numeric value to round.
#' @param multiplicand A scaling factor to apply before rounding.
#' @return An integer value after rounding.
#' @noRd
round_to_integer <- function(x, multiplicand = 1) {
  if (is.integer(x)) {
    x
  } else if (is.numeric(x)) {
    as.integer(round(x * multiplicand))
  } else {
    stop_input_type(x, "numeric")
  }
}

#' Style a potentially long vector of doubles
#' @noRd
style_vec <- function(vec) {
  vec <- cli::cli_vec(
    prettyNum(vec),
    style = list(
      "vec-sep" = ", ",
      "vec-sep2" = ", ",
      "vec-last" = ", ",
      "vec-trunc" = 3
    )
  )
}

# Helpers for generating and validating the live points -----

# Helpers for running nested sampling ---

#' Find the indices of the smallest `n` values in a vector.
#'
#' @param x A numeric vector to search.
#' @param n The number of smallest values to find.
#' @return An integer vector of indices corresponding to the smallest values.
#' @noRd
which_minn <- function(x, n = 1L) {
  if (n == 1L) {
    which.min(x)
  } else {
    order(x)[seq_len(min(n, length(x)))]
  }
}

# Helpers for computing and reporting results -----

#' Compute the nested sampling integral and related statistics.
#'
#' @param log_lik A vector of log-likelihoods in descending order.
#' @param log_volume A vector of log-volumes in ascending order.
#'
#' @return A list of calculations.
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
  log_weight <- get_logweight(log_lik, log_volume)
  log_evidence <- get_logevid(log_weight)
  information <- get_information(log_lik, log_volume, log_evidence)

  # Estimate the error around logz
  dh <- c(information[1], diff(information))
  log_evidence_var <- abs(cumsum(dh * -diff(c(0, log_volume))))

  vctrs::vec_cast_common(
    log_lik = log_lik,
    log_volume = log_volume,
    log_weight = log_weight,
    log_evidence = log_evidence,
    log_evidence_var = log_evidence_var,
    information = information,
    .to = double()
  )
}

#' Estimate log vol for the live points
#'
#' @param dead_log_vol A vector of log volumes for dead points.
#' @param n_points The number of live points to estimate log volume for.
#' @return A vector of log volumes for the live points.
#' @noRd
get_live_vol <- function(dead_log_vol, n_points) {
  last_vol <- dead_log_vol[[vctrs::vec_size(dead_log_vol)]]
  last_vol + log1p((-1 - n_points)^(-1) * seq_len(n_points))
}
