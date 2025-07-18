#' Cast a number to an integer after boundary checking
#'
#' @param x The number to cast to integer.
#' @param min,max The minimum and maximum values for the rounded number.
#' @param arg The name of the argument being checked, for error messages.
#' @param call The calling environment for error messages.
#'
#' @return An integer value.
#' @srrstats {G2.1, G2.2, G2.4, G2.4a} `check_integer()` validates unidimensional integer input.
#' @noRd
check_integer <- function(
  x,
  min = NULL,
  max = NULL,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_number_whole(
    x,
    min = min,
    max = max,
    allow_infinite = FALSE,
    allow_na = FALSE,
    allow_null = FALSE,
    arg = arg,
    call = call
  )
  as.integer(x)
}

#' Cast a number to a double after boundary checking
#'
#' @param x The number to cast to double.
#' @param min,max The minimum and maximum values for the number.
#' @param arg The name of the argument being checked, for error messages.
#' @param call The calling environment for error messages.
#'
#' @return A double value.
#' @srrstats {G2.1, G2.2, G2.4, G2.4b} `check_double()` validates unidimensional double input.
#' @noRd
check_double <- function(
  x,
  min = NULL,
  max = NULL,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_number_decimal(
    x,
    min = min,
    max = max,
    allow_infinite = FALSE,
    allow_na = FALSE,
    allow_null = FALSE,
    arg = arg,
    call = call
  )
  as.double(x)
}

#' Calculate the HDI of an rvar
#'
#' @author Based on implementation from
#' https://github.com/mikemeredith/HDInterval
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
#' @param log_volume,weight Both rvar!
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
