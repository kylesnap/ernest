#' INPUT VALIDATION FUNCTIONS --------
#'
#' @srrstats {G2.1, G2.2, G2.4, G2.4a, G2.4b} `These checkers validate and cast
#' unidimensional integer and double input.
#' @noRd
NULL

#' Check class of an object
#'
#' @param x An object.
#' @param class A character vector of classes.
#' @param allow_null,arg,call See stop_input_type.
#' @returns NULL if `x` contains class, or an informative error message.
#' @importFrom rlang inherits_any
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

#' Check that `x` is a double matrix.
#' @param x An object.
#' @param nrow Expected number of columns.
#' @param ncol Expected number of columns.
#' @param arg,call See stop_input_type.
#' @param lower,upper Exclusive boundaries; recycled to ncol-length vector.
#' @returns NULL if `x` conforms, or an informative error message.
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

#' Check that `x` is a double vector
#' @param x An object.
#' @param size Expected length.
#' @param arg,call See stop_input_type.
#' @param allow_neg_inf Can `x` contain `-Inf`?
#' @returns NULL if `x` conforms, or an informative error message.
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

#' Transform a function so that it can be applied over a matrix of inputs.
#' @param fn Function.
#' @return A function that can be applied over vectors.
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

#' Check that a list is all named and that names are unique
#' @param x The list
#' @param arg,call See stop_input_type.
#' @return Either NULL, invisibly, or an error message.
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

#' Inject a list into a vector.
#' @param x A list to inject.
#' @return A vector with the elements of the list.
#' @noRd
list_c <- function(x) {
  inject(c(!!!x))
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
