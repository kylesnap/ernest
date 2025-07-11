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

#' Calculate the HDI of an rvar
#' @noRd
#' @author Based on implementation from https://github.com/mikemeredith/HDInterval
hdci <- function(object, width = 0.95, ...) {
  if (!inherits(object, "rvar")) {
    stop("Input must be of class 'rvar'.")
  }
  # Compute HDI for each element
  hdis <- t(vapply(
    object,
    function(x) {
      draws <- as.numeric(posterior::draws_of(x))
      if (!is.numeric(draws)) return(c(NA, NA))
      x_sorted <- sort.int(draws, method = "quick")
      n <- length(x_sorted)
      if (n < 3) return(vctrs::vec_recycle(x, size = 2L))
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
  draws <- posterior::draws_rvars(
    "log_vol" = log_volume,
    "weight" = weight
  )
  min_vol <- min(mean(log_volume))
  dens_list <- list()
  log_vol <- NULL
  .draw <- NULL
  density <- posterior::for_each_draw(draws, {
    dens <- stats::density(
      log_vol,
      weights = weight,
      warnWbw = FALSE,
      from = min_vol,
      to = 0,
      n = 128
    )
    dens_list[[.draw]] <<- data.frame("log_vol" = dens$x, "weights" = dens$y)
  })
  density <- vctrs::vec_rbind(!!!dens_list)
  density_rvar <- matrix(density$weights, nrow = 128) |>
    t() |>
    posterior::rvar()
  data.frame("log_volume" = unique(density$log_vol), "density" = density_rvar)
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
