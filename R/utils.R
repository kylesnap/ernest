# Helpers for validating user input -----

#' Round a number to an integer after scaling by a multiplicand.
#'
#' @param x A numeric value to round.
#' @param multiplicand A scaling factor to apply before rounding.
#' @param min Optional minimum value for validation.
#' @return An integer value after rounding.
#' @noRd
round_to_integer <- function(x, multiplicand, min = NULL) {
  x <- if (is.integer(x)) {
    x
  } else {
    check_number_decimal(x, allow_infinite = FALSE)
    as.integer(x * multiplicand)
  }
  check_number_whole(x, min = min, allow_infinite = FALSE)
  as.integer(x)
}

#' Check for duplicate names in a character vector.
#'
#' @param names A character vector of names to check.
#' @return Throws an error if duplicates are found; otherwise, returns NULL.
#' @noRd
check_unique_names <- function(names) {
  if (anyDuplicated(names)) {
    duplicates <- unique(names[duplicated(names)])
    cli::cli_abort(
      "The following names are duplicated: {duplicates}."
    )
  }
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
  unit <- matrix(numeric(n_points * n_dim), nrow = n_points, ncol = n_dim)
  point <- matrix(numeric(n_points * n_dim), nrow = n_points, ncol = n_dim)
  log_lik <- numeric(n_points)
  i <- 0L
  try_fetch(
    for (i in seq(nrow(unit))) {
      proposed <- lrps$propose_uniform()
      unit[i, ] <- proposed$unit
      point[i, ] <- proposed$parameter
      log_lik[i] <- proposed$log_lik
    },
    error = function(cnd) {
      cli::cli_abort(
        sprintf("Problem while mapping element %d.", i),
        parent = cnd
      )
    },
    warning = function(cnd) {
      cli::cli_abort(
        sprintf("Problem while mapping element %d.", i),
        parent = cnd
      )
    }
  )
  list(
    unit = unit,
    point = point,
    log_lik = log_lik
  )
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
check_live <- function(live, n_points, n_var, call = caller_env()) {
  if (is_empty(live)) {
    cli::cli_abort("No live points found.")
  }
  tmplate <- matrix(nrow = n_points, ncol = n_var)
  # live$unit: Must be a matrix of points with dim [n_points, n_var],
  # all points must be finite and within [0, 1]
  if (!is.matrix(live$unit)) {
    cli::cli_abort("Internal error: Unit points must be stored as a matrix.")
  }
  if (!identical(dim(live$unit), dim(tmplate))) {
    cli::cli_abort("Internal error: Unit points must be stored as a matrix with dim ({n_points}, {n_var}).")
  }
  if (any(!is.finite(live$unit))) {
    cli::cli_abort("Internal error: Unit points must contain only finite values.")
  }
  if (any(live$unit < 0) || any(live$unit > 1)) {
    cli::cli_abort("Internal error: Unit points must contain values within [0, 1].")
  }
  # live$point: Must be a matrix of points with dim [n_points, n_var],
  # all points should be finite (warn if not)
  if (!is.matrix(live$point)) {
    cli::cli_abort("Live points could'nt be stored as a matrix.")
  }
  if (!identical(dim(live$point), dim(tmplate))) {
    cli::cli_abort("Live points couldn't be stored as a matrix with dim ({n_points}, {n_var}).")
  }
  if (any(!is.finite(live$point))) {
    idx <- which(rowSums(!is.finite(live$point) > 0))
    len <- length(idx)
    first_unit <- live$unit[idx[1]]
    first_point <- live$point[idx[1]]
    cli::cli_warn(c(
      "Live points currently contain finite values.",
      "i" = "This may be intentional, but double-check the behaviour of {.arg prior}.",
      "x" = "There {?is/are} {len} non-finite value{?s}.",
      "x" = "First non-finite element: {first_unit} -> {first_point}."
    ))
  }
  # loglik: Must be numeric vector of length n_points. Must contain only values
  # that are either finite or -Inf. Abort if log_lik contains no
  # unique values, warn if 10% of the values are duplicates.
  vctrs::vec_check_size(live$log_lik, size = n_points, call = caller_env)
  idx <- intersect(which(!is.finite(live$log_lik)), which(live$log_lik != -Inf))
  if (length(idx) > 0L) {
    len <- length(idx)
    first_point <- live$point[idx[1]]
    first_logl <- live$log_lik[idx[1]]
    cli::cli_abort(c(
      "Couldn't avoid calculating non-finite log-likelihood values.",
      "i" = "Log-likelihood values can only be finite or `-Inf`.",
      "x" = "There {?is/are} {len} non-finite value{?s}.",
      "x" = "First non-finite value: f({first_point}) = {.val {first_logl}}"
    ))
  }
  idx <- which(live$log_lik == -Inf)
  if (length(idx) > 0L) {
    len <- length(idx)
    first_point <- live$point[idx[1]]
    first_logl <- live$log_lik[idx[1]]
    cli::cli_warn(c(
      "Found {len} log-likelihood value{?s} equal to `-Inf`.",
      "i" = "First point: f({first_point}) = {.val {first_logl}}"
    ))
  }
  unique_logl <- unique(live$log_lik)
  if (length(unique_logl) == 1) {
    cli::cli_abort(c(
      "Couldn't generate unique log-likelihood values for each point.",
      "x" = "Every point had a calculated log-likelihood value of {unique_logl}.",
      "i" = "This generally indicates an error with your log-likelihood function."
    ))
  }
  if (length(unique_logl) < length(live$log_lik) * 0.25) {
    perc <- prettyNum(length(unique_logl) / length(live$log_lik))
    cli::cli_warn(c(
      "Suspected flatness in the log-likelihood surface.",
      "x" = "Only {perc}% of the live points have unique log-likelihood values.",
      "i" = "Consider reviewing your model or adjusting your prior for more efficient sampling."
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
    order(x)[1:min(n, length(x))]
  }
}

# Helpers for computing and reporting results -----

#' Compute the nested sampling integral and related statistics.
#'
#' @param log_lik A vector of log-likelihoods in descending order.
#' @param log_vol A vector of log-volumes in ascending order.
#' @return A tibble containing integration results, including log weights, evidence, and information.
#' @noRd
compute_integral <- function(log_lik, log_vol) {
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

  tibble::tibble(
    "log_likelihood" = log_lik,
    "log_volume" = log_vol,
    "log_weight" = log_wt,
    "log_evidence" = log_z,
    "log_evidence.var" = log_z_var,
    "information" = h,
  )
}
