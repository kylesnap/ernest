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

#' Create a live sample with `n` live points.
#'
#' @param lrps An object containing the likelihood-restricted prior sampler.
#' @param n_points The number of live points to generate.
#' @param n_dim The number of dimensions for each point.
#' @param call The calling environment for error handling.
#' @return A list containing `unit`, `point`, and `log_lik` matrices/vectors.
#' @noRd
create_live <- function(lrps, n_points, n_dim, call = caller_env()) {
  i <- 0L
  unit <- matrix()
  try_fetch(
    {
      unit <- lrps$propose_uniform(criteria = rep(-1e300, n_points))
    },
    error = function(cnd) {
      cli::cli_abort(
        "Problem while generating initial live points.",
        parent = cnd,
        call = call
      )
    }
  )
  unit
}

#' Validate an existing nested sample for correctness.
#'
#' @param live A list containing live points (`unit`, `point`, `log_lik`).
#' @param n_points The expected number of live points.
#' @param n_var The expected number of dimensions for each point.
#' @param call The calling environment for error handling.
#'
#' @return Throws an error or warning if validation fails; otherwise, returns
#' NULL.
#' @noRd
check_live <- function(unit, log_lik, n_points, n_var, call = caller_env()) {
  tmplate <- matrix(nrow = n_points, ncol = n_var)
  # unit: Must be a matrix of points with dim [n_points, n_var],
  # all points must be finite and within [0, 1]
  if (!is.matrix(unit)) {
    cli::cli_abort("Internal error: Unit points must be stored as a matrix.")
  }
  if (!identical(dim(unit), dim(tmplate))) {
    cli::cli_abort(
      "Unit points must be stored as a matrix with dim ({n_points}, {n_var})."
    )
  }
  if (any(!is.finite(unit))) {
    cli::cli_abort(
      "Unit points must contain only finite values."
    )
  }
  if (any(unit < 0) || any(unit > 1)) {
    cli::cli_abort(
      "Unit points must contain values within [0, 1]."
    )
  }
  # loglik: Must be numeric vector of length n_points. Must contain only values
  # that are either finite or -Inf. Abort if log_lik contains no
  # unique values, warn if 10% of the values are duplicates.
  vctrs::vec_check_size(log_lik, size = n_points, call = caller_env)
  idx <- intersect(which(!is.finite(log_lik)), which(log_lik != -Inf))
  if (length(idx) > 0L) {
    len <- length(idx)
    first_logl <- log_lik[idx[1]]
    cli::cli_abort(c(
      "Couldn't avoid calculating non-finite log-likelihood values.",
      "i" = "Log-likelihood values can only be finite or `-Inf`.",
      "x" = "There {?is/are} {len} non-finite value{?s}."
    ))
  }
  idx <- which(log_lik == -Inf)
  if (length(idx) > 0L) {
    len <- length(idx)
    cli::cli_warn(
      "Found {len} log-likelihood value{?s} equal to `-Inf`."
    )
  }
  unique_logl <- unique(log_lik)
  if (length(unique_logl) == 1) {
    cli::cli_abort(c(
      "Couldn't generate unique log-likelihood values for each point.",
      "x" = "Every point had a calculated log-lik. value of {unique_logl}.",
      "i" = "This generally indicates an error with your log-lik. function."
    ))
  }
  if (length(unique_logl) < length(log_lik) * 0.25) {
    perc <- prettyNum(length(unique_logl) / length(log_lik))
    cli::cli_warn(c(
      "Suspected flatness in the log-likelihood surface.",
      "x" = "Only {perc}% of the live points have unique log-lik. values.",
      "i" = "Consider reviewing your model or adjusting your prior."
    ))
  }
  NULL
}

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
