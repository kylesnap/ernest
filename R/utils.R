#' INPUT VALIDATION FUNCTIONS --------
#'
#' @srrstats {G2.1, G2.2, G2.4, G2.4a, G2.4b} `These checkers validate and cast
#' unidimensional integer and double input.
#' @noRd
NULL

#' Validate and cast a scalar integer.
#'
#' @param x Input to check.
#' @param min Minimum allowed value (inclusive).
#' @param max Maximum allowed value (inclusive).
#' @param allow_null Whether NULL is allowed.
#' @param allow_na Whether NA is allowed.
#' @param arg Argument name for error reporting.
#' @param call Calling environment for error reporting.
#' @return An integer scalar or error.
#' @noRd
as_scalar_integer <- function(
  x,
  min = -Inf,
  max = Inf,
  allow_null = FALSE,
  allow_na = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  msg <- checkmate::check_int(
    x,
    na.ok = allow_na,
    lower = min,
    upper = max,
    null.ok = allow_null
  )

  if (isTRUE(msg)) {
    storage.mode(x) <- "integer"
    x
  } else {
    format_checkmate(sub("^Element \\d+ ", "", msg), arg, call)
  }
}

##' Validate and cast a scalar count (non-negative integer).
##'
##' @param x Input to check.
##' @param positive Whether only positive values are allowed.
##' @param allow_null Whether NULL is allowed.
##' @param allow_na Whether NA is allowed.
##' @param arg Argument name for error reporting.
##' @param call Calling environment for error reporting.
##' @return An integer scalar or error.
##' @noRd
as_scalar_count <- function(
  x,
  positive = TRUE,
  allow_null = FALSE,
  allow_na = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  msg <- checkmate::check_count(
    x,
    positive = positive,
    na.ok = allow_na,
    null.ok = allow_null
  )
  if (isTRUE(msg)) {
    storage.mode(x) <- "integer"
    x
  } else {
    format_checkmate(msg, arg, call)
  }
}

##' Validate and cast a scalar double.
##'
##' @param x Input to check.
##' @param min Minimum allowed value (inclusive).
##' @param max Maximum allowed value (inclusive).
##' @param allow_null Whether NULL is allowed.
##' @param allow_na Whether NA is allowed.
##' @param arg Argument name for error reporting.
##' @param call Calling environment for error reporting.
##' @return A double scalar or error.
##' @noRd
as_scalar_double <- function(
  x,
  min = -Inf,
  max = Inf,
  allow_null = FALSE,
  allow_na = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  msg <- checkmate::check_number(
    x,
    na.ok = allow_na,
    lower = min,
    upper = max,
    null.ok = allow_null
  )
  if (isTRUE(msg)) {
    storage.mode <- "double" # nolint
    if (!is.null(x) && is.na(x)) {
      x <- NA_real_
    }
    x
  } else {
    format_checkmate(sub("^Element \\d+ ", "", msg), arg, call)
  }
}

##' Validate and cast a scalar logical.
##'
##' @param x Input to check.
##' @param allow_null Whether NULL is allowed.
##' @param allow_na Whether NA is allowed.
##' @param arg Argument name for error reporting.
##' @param call Calling environment for error reporting.
##' @return A logical scalar or error.
##' @noRd
as_scalar_logical <- function(
  x,
  allow_null = FALSE,
  allow_na = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  msg <- checkmate::check_logical(
    x,
    any.missing = allow_na,
    null.ok = allow_null,
    len = 1L
  )
  if (isTRUE(msg)) {
    as.logical(x)
  } else {
    format_checkmate(msg, arg, call)
  }
}

##' Validate and coerce to a univariate function.
##'
##' @param fn Function or object coercible to function.
##' @param arg Argument name for error reporting.
##' @param call Calling environment for error reporting.
##' @return A function of one argument or error.
##' @noRd
as_univariate_function <- function(
  fn,
  arg = caller_arg(fn),
  call = caller_env()
) {
  fn <- rlang::as_function(fn, env = rlang::global_env())
  msg <- checkmate::check_function(fn, nargs = 1L)
  if (isTRUE(msg)) {
    fn
  } else {
    format_checkmate(msg, arg, call)
  }
}

##' Check if input is a function.
##'
##' @param fn Object to check.
##' @param arg Argument name for error reporting.
##' @param call Calling environment for error reporting.
##' @return TRUE if function, error otherwise.
##' @noRd
is_function <- function(fn, arg = caller_arg(fn), call = caller_env()) {
  msg <- checkmate::check_function(fn)
  if (isTRUE(msg)) {
    TRUE
  } else {
    format_checkmate(msg, arg, call)
  }
}

#' Check if input inherits from a class.
#'
#' @param x Object to check.
#' @param class Class name(s) to check against.
#' @param arg Argument name for error reporting.
#' @param call Calling environment for error reporting.
#' @return TRUE if inherits, error otherwise.
#' @noRd
is_class <- function(x, class, arg = caller_arg(x), call = caller_env()) {
  msg <- checkmate::check_class(x, class)
  if (isTRUE(msg)) {
    TRUE
  } else {
    format_checkmate(msg, arg, call)
  }
}

#' Format a message from checkmate.
#' @param msg The message to format.
#' @param arg Argument name for error reporting.
#' @param call Calling environment for error reporting.
#' @return A formatted error message.
#' @noRd
format_checkmate <- function(msg, arg, call) {
  cli::cli_abort("`{arg}` {tolower(msg)}.", call = call)
}

#' Inject a list into a vector.
#' @param x A list to inject.
#' @return A vector with the elements of the list.
#' @noRd
list_c <- function(x) {
  inject(c(!!!x))
}

#' Set the random seed for reproducibility.
#'
#' @param seed Either a single value, interpretted as an integer, or `NA` or
#' `NULL`.
#' @param results The results object that contains the last seed.
#' @param call The calling environment for error messages.
#'
#' @return The set seed value (.Random.seed).
#' @noRd
set_random_seed <- function(
  seed,
  results = NULL,
  call = caller_env()
) {
  if (!is.na(seed)) {
    seed <- as_scalar_integer(seed, allow_null = TRUE, call = call)
    set.seed(seed)
  } else {
    if (!is.null(attr(results, "seed"))) {
      .Random.seed <- attr(results, "seed") # nolint
    }
  }
  if (!exists(".Random.seed")) {
    set.seed(NULL)
  }
  .Random.seed
}

#' Calculate the HDI of an rvar
#'
#' @param object An rvar object containing draws.
#' @param width The width of the HDI to compute (default is 0.
#' @returns A data.frame with the median, lower and upper bounds of the HDI,
#'
#' @author Based on implementation from
#' https://github.com/mikemeredith/HDInterval
#' @noRd
hdci <- function(object, width = 0.95, ...) {
  if (!inherits(object, "rvar")) {
    stop("Input must be of class 'rvar'.")
  }
  # Compute HDI for each element
  hdis <- t(vapply(
    object,
    function(x) {
      draws <- as.numeric(posterior::draws_of(x))
      if (!is.numeric(draws)) {
        return(c(NA, NA))
      }
      x_sorted <- sort.int(draws, method = "quick")
      n <- length(x_sorted)
      if (n < 3) {
        return(vctrs::vec_recycle(x, size = 2L))
      }
      exclude <- n - floor(n * width)
      lowest <- x_sorted[1:exclude]
      largest <- x_sorted[(n - exclude + 1):n]
      best <- which.min(largest - lowest)
      if (length(best)) {
        c(lowest[best], largest[best])
      } else {
        c(NA, NA)
      }
    },
    numeric(2L)
  ))
  colnames(hdis) <- c(".lower", ".upper")
  data.frame(
    ".var" = stats::median(object),
    hdis,
    ".width" = width
  )
}

#' Estimate the density of the posterior weights across the log volumes
#' @param log_volume,weight rvars of log_volume draws and corresponding
#' normalized posterior weights.
#'
#' @returns A data.frame containing 128 values of log volume and an rvar of
#' densities
#' @noRd
get_density <- function(log_volume, weight) {
  min_vol <- min(mean(log_volume))
  log_vol_draws <- posterior::draws_of(log_volume)
  w_draws <- posterior::draws_of(weight)

  log_vol_spaced <- stats::density(
    log_vol_draws[1, ],
    weight = w_draws[1, ],
    warnWbw = FALSE,
    from = min_vol,
    to = 0,
    n = 128L
  )$x

  density <- vapply(
    seq_len(nrow(w_draws)),
    \(i) {
      stats::density(
        log_vol_draws[i, ],
        weight = w_draws[i, ],
        warnWbw = FALSE,
        from = min_vol,
        to = 0,
        n = 128L
      )$y
    },
    double(128L)
  )
  density_rvar <- posterior::rvar(t(density))
  data.frame("log_volume" = log_vol_spaced, "density" = density_rvar)
}

#' Interpolate evidence across a range of log volumes
#' @param log_volume,weight rvars of log_volume draws and corresponding
#' normalized posterior weights.
#' @param The breaks of log_volume at which to interpolate the evidence
#' estimates.
#'
#' @returns An rvar with the same ndraws as `log_volume` and length of
#' `log_volume_out`.
#' @noRd
interpolate_evidence <- function(log_volume, log_evidence, log_volume_out) {
  log_vol_draws <- posterior::draws_of(log_volume)
  log_evid_draws <- posterior::draws_of(exp(log_evidence))

  interp <- vapply(
    seq_len(nrow(log_evid_draws)),
    \(i) {
      stats::approx(
        x = log_vol_draws[i, ],
        y = log_evid_draws[i, ],
        xout = log_volume_out,
        rule = 2L
      )$y
    },
    double(128L)
  )
  posterior::rvar(t(interp))
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
