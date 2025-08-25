#' Check the class of an object
#'
#' Validates that an object inherits from at least one of the specified classes.
#'
#' @param x An object to check.
#' @param class A character vector of allowed classes.
#' @param ... Additional arguments passed to error handlers.
#' @param allow_null Logical. If TRUE, allows NULL values.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if `x` inherits from one of the specified
#' classes, otherwise throws an informative error.
#' @noRd
check_class <- function(
  x,
  class,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if ((allow_null && is.null(x)) || inherits_any(x, class)) {
    return(invisible(NULL))
  }

  cls_format <- cli::pluralize(
    "an object with {?class/at least one class from} {class}"
  )
  stop_input_type(
    x,
    cls_format,
    ...,
    allow_na = FALSE,
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

#' Check that an object is a double matrix
#'
#' Validates that an object is a double matrix with specified dimensions
#' and within given bounds.
#'
#' @param x An object to check.
#' @param nrow Expected number of rows.
#' @param ncol Expected number of columns.
#' @param lower Numeric. Exclusive lower bounds, recycled to length `ncol`.
#' @param upper Numeric. Exclusive upper bounds, recycled to length `ncol`.
#' @param ... Additional arguments passed to error handlers.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if `x` is a valid double matrix,
#' otherwise throws an informative error.
#' @noRd
check_matrix <- function(
  x,
  nrow,
  ncol,
  lower = -Inf,
  upper = Inf,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_number_whole(nrow, min = 1, call = "check_matrix")
  check_number_whole(ncol, min = 1, call = "check_matrix")
  bounds <- vctrs::vec_recycle_common(lower, upper, .size = ncol)

  if (!is.matrix(x) || !is_double(x)) {
    stop_input_type(
      x,
      "a matrix",
      ...,
      allow_na = FALSE,
      allow_null = FALSE,
      arg = arg,
      call = call
    )
  }
  if (!isTRUE(all.equal(dim(x), c(nrow, ncol)))) {
    cli::cli_abort(
      c(
        "`{arg}` must have dimensions {nrow} x {ncol}.",
        "x" = "`{arg}` instead has dimensions {nrow(x)} x {ncol(x)}"
      ),
      call = call
    )
  }

  for (i in seq(nrow(x))) {
    if (any(is.na(x[i, ]) | is.nan(x[i, ]))) {
      cli::cli_abort(
        "`{arg}` must not contain missing or `NaN` values.",
        call = call
      )
    }
    if (any(x[i, ] <= lower)) {
      cli::cli_abort(
        "`{arg}` must respect the lower boundary ({lower}).",
        call = call
      )
    }
    if (any(x[i, ] >= upper)) {
      cli::cli_abort(
        "`{arg}` must respect the upper boundary ({upper}).",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' Check that an object is a double vector
#'
#' Validates that an object is a double vector of a specified length and value
#' constraints.
#'
#' @param x An object to check.
#' @param size Expected length of the vector.
#' @param allow_neg_inf Logical. If FALSE, `-Inf` values are not allowed.
#' @param ... Additional arguments passed to error handlers.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if `x` is a valid double vector, otherwise
#' throws an informative error.
#' @noRd
check_double <- function(
  x,
  size,
  allow_neg_inf = TRUE,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_number_whole(size, min = 0, call = "check_vector")

  if (!is_bare_double(x)) {
    stop_input_type(
      x,
      glue::glue("a double vector"),
      ...,
      allow_na = FALSE,
      allow_null = FALSE,
      arg = arg,
      call = call
    )
  }
  if (vctrs::vec_size(x) != size) {
    act <- vctrs::vec_size(x)
    cli::cli_abort("`{arg}` must be a double vector of size {size}, not {act}.")
  }
  if (size == 0L) {
    return(invisible(NULL))
  }

  if (any(is.na(x) | is.nan(x) | x == Inf)) {
    cli::cli_abort("`{arg}` must not contain missing, `NaN`, or `Inf` values.")
  }
  if (!allow_neg_inf && any(x == -Inf)) {
    cli::cli_abort("`{arg}` must not contain `-Inf` values.")
  }
  invisible(NULL)
}

#' Transform a function for rowwise application
#'
#' Converts a function so it can be applied rowwise to a matrix or directly to
#' a vector.
#'
#' @param fn A function to transform.
#'
#' @return A function that applies `fn` to each row of a matrix or to a vector.
#' @noRd
as_rowwise_fn <- function(fn) {
  x <- NULL
  new_function(
    exprs(x = ),
    expr({
      if (is.vector(x)) {
        (!!fn)(x)
      } else if (is.matrix(x)) {
        tmp <- apply(x, 1, (!!fn))
        if (is.matrix(tmp)) t(tmp) else tmp
      } else {
        stop_input_type(x, "a double vector or matrix", arg = "x")
      }
    })
  )
}

#' Check that a list has unique, non-empty names
#'
#' Validates that all elements of a list are named and that names are unique.
#'
#' @param x A list to check.
#' @param ... Additional arguments passed to error handlers.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if all names are unique and non-empty,
#' otherwise throws an informative error.
#' @noRd
check_unique_names <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  nms <- vctrs::vec_names(x)
  if (is.null(nms) != any(nms == "")) {
    cli::cli_abort(
      "All elements of `{arg}` must have unique names.",
      call = call
    )
  }

  if (vctrs::vec_duplicate_any(nms)) {
    idx <- vctrs::vec_duplicate_id(nms) |> unique()
    cli::cli_abort(
      c(
        "All elements of `{arg}` must have unique names.",
        "x" = "Repeated names: {nms[idx]}"
      ),
      call = call
    )
  }

  invisible(NULL)
}

#' Inject a list into a vector
#'
#' Concatenates the elements of a list into a single vector.
#'
#' @param x A list to concatenate.
#'
#' @return A vector containing all elements of the list.
#' @noRd
list_c <- function(x) {
  inject(c(!!!x))
}

#' Calculate the highest density credible interval (HDI) of an rvar
#'
#' Computes the HDI for each element of an rvar object containing posterior
#' draws.
#'
#' @param object An rvar object containing draws.
#' @param width Numeric. The width of the HDI to compute (default is 0.95).
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with the median, lower, and upper bounds of the HDI
#' for each element.
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

#' Estimate the density of posterior weights across log-volumes
#'
#' Computes the density of posterior weights for a range of log-volume values.
#'
#' @param log_volume An rvar of log-volume draws.
#' @param weight An rvar of normalized posterior weights.
#'
#' @return A data frame with 128 log-volume values and an rvar of corresponding
#' densities.
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

#' Interpolate evidence across log-volume values
#'
#' Interpolates evidence estimates across a specified range of log-volume
#' values.
#'
#' @param log_volume An rvar of log-volume draws.
#' @param log_evidence An rvar of log-evidence draws.
#' @param log_volume_out Numeric vector of log-volume values at which to
#' interpolate.
#'
#' @return An rvar with interpolated evidence values at each specified
#' log-volume.
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

#' Find indices of the smallest n values in a vector
#'
#' Returns the indices of the smallest `n` values in a numeric vector.
#'
#' @param x A numeric vector to search.
#' @param n Integer. The number of smallest values to find.
#'
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

#' Compute the nested sampling integral and statistics
#'
#' Calculates the nested sampling integral and related statistics from
#' log-likelihoods and log-volumes.
#'
#' @param log_lik Numeric vector of log-likelihoods in descending order.
#' @param log_volume Numeric vector of log-volumes in ascending order.
#'
#' @return A list containing log-likelihoods, log-volumes, log-weights,
#' log-evidence, log-evidence variance, and information.
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
  log_weight <- drop(get_logweight(log_lik, log_volume))
  log_evidence <- drop(get_logevid(log_weight))
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

#' CLI formatter for doubles
#'
#' @param x An object.
#' @returns x, unless x is a number, in which case x is rounded to a set
#' number of decimal places.
pretty <- function(x) {
  if (!is.numeric(x)) {
    x
  }
  round(x, digits = max(3L, getOption("digits") - 3L))
}
