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

#' Check for duplicate names in a character vector.
#'
#' @param names A character vector of names to check.
#' @return Throws an error if duplicates are found; otherwise, returns NULL.
#' @noRd
check_unique_names <- function(names, arg = caller_arg(), call = caller_env()) {
  if (anyDuplicated(names)) {
    duplicates <- unique(names[duplicated(names)])
    cli::cli_abort(
      "The following names are duplicated: {duplicates}."
    )
  }
}

#' Style a potentially long vector of doubles
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
#' @param log_vol A vector of log-volumes in ascending order.
#'
#' @return A list of calculations.
#' @noRd
compute_integral <- function(log_lik, log_vol) {
  if (vctrs::vec_size_common(log_lik, log_vol) == 0L) {
    return(list(
      log_lik = double(0),
      log_vol = double(0),
      log_weight = double(0),
      log_evidence = double(0),
      log_evidence_var = double(0),
      information = double(0)
    ))
  }
  log_lik_pad <- c(-1e300, log_lik)
  diff_log_vol <- diff(c(0, log_vol))
  log_diff_vol <- log_vol - diff_log_vol + log(-expm1(diff_log_vol))
  log_diff_vol2 <- log_diff_vol - log(2)
  diff_log_vol <- -diff(c(0, log_vol))

  log_weight <- mapply(
    \(lik1, lik2, ldv) logaddexp(lik1, lik2) + ldv,
    tail(log_lik_pad, -1),
    head(log_lik_pad, -1),
    log_diff_vol2
  )
  log_evidence <- accumulate(log_weight, \(cur, nxt) logaddexp(cur, nxt))
  max_log_evidence <- tail(log_evidence, 1)

  # Compute information
  exp1 <- exp(log_lik_pad[-1] - max_log_evidence + log_diff_vol2)
  exp2 <- exp(
    log_lik_pad[-length(log_lik_pad)] - max_log_evidence + log_diff_vol2
  )
  h_part1 <- cumsum(
    exp1 * log_lik_pad[-1] + exp2 * log_lik_pad[-length(log_lik_pad)]
  )
  information <- h_part1 -
    max_log_evidence *
      exp(log_evidence - max_log_evidence)
  dh <- c(information[1], diff(information))

  # Compute logz variance
  log_evidence_var <- abs(cumsum(dh * diff_log_vol))

  vctrs::vec_cast_common(
    log_lik = log_lik,
    log_volume = log_vol,
    log_weight = log_weight,
    log_evidence = log_evidence,
    log_evidence_var = log_evidence_var,
    information = information,
    .to = double()
  )
}

simulate_volume <- function(points) {
  non_decreasing <- c(TRUE, diff(points) >= 0)

  # Contraction from nondecreasing live points follows Beta(npoints, 1)
  vctrs::vec_slice(log_vol, non_decreasing) <- rbeta(
    sum(non_decreasing),
    points[non_decreasing],
    1
  )

  # Contraction between decreasing points follows X_(j) / X_N =
  # (Y_1 + ... + Y_j) / (Y_1 + ... + Y_{K+1}) where X_(j) is the prior volume of
  # j-th best live point
  compressed <- vctrs::vec_unrep(non_decreasing)
  start <- cumsum(compressed$times)[compressed$key]
  end <- cumsum(compressed$times)[!compressed$key]
  for (i in seq_along(end)) {
    s <- start[i]
    e <- end[i]
    start_points <- points[s]
    y <- rexp(start_points + 1, rate = 1)
    y_norm <- cumsum(y) / sum(y)
    unif_order <- y_norm[c(start_points + 1, points[(s + 1):e])]
    log_vol[(s + 1):e] <- unif_order[-1] / unif_order[-length(unif_order)]
  }

  # Compute logvol
  cumsum(log(log_vol))
}

#' Calculating weights
get_weight <- function(log_lik, log_volume) {
  n_samp <- vec_size_common(log_lik, log_volume)
  log_lik <- c(-1e300, log_lik)
  diff_volume <- diff(c(0, log_volume))
  vol_term <- log_volume - diff_volume + log(-expm1(diff_volume))

  idx <- c(2:(n_samp + 1))
  lik_term <- logaddexp(log_lik[idx], log_lik[idx-1])
  lik_term + vol_term + log(0.5)
}

#' Math utilities
logaddexp <- function(a, b) {
  m <- pmax(a, b)
  m + log1p(exp(-abs(a - b)))
}

#' Estimate log vol for the live points
get_live_vol <- function(dead_log_vol, n_points) {
  last_vol <- dead_log_vol[[vctrs::vec_size(dead_log_vol)]]
  last_vol + log1p((-1 - n_points)^(-1) * seq_len(n_points))
}

#' Fix a vector of cumulative increments into a proper cumulative sum.
#'
#' Given a vector of (possibly non-monotonic) cumulative iteration counts,
#' returns a vector where each element is the running sum, treating any
#' decrease as a new increment.
#'
#' @param x Integer vector of cumulative iteration counts.
#' @return Integer vector of corrected cumulative sums.
#' @noRd
fix_cumulative_increments <- function(x) {
  inc <- diff(c(0, x))
  inc[inc <= 0] <- x[inc <= 0]
  cumsum(inc)
}
