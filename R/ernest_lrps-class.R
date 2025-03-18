#' Likelihood Restricted Prior Samplers for ernest
#'
#' Likelihood-restricted prior sampling (or LRPS) aims to generate points within
#' the parameter space defined by the prior. Many options exist for conducting
#' LRPS, though two currently are implemented in ernest: uniform sampling and
#' a random walk with step evolution.
#'
#' @param log_lik A function that returns the log-likelihood of a point in the
#' parameter space.
#' @param prior_transform A function that transforms a unit cube to the prior
#' space.
#'
#' @rdname ernest_lrps
#' @noRd
NULL

#' @rdname ernest_lrps
#' @noRd
new_ernest_lrps <- function(name = "Ernest LRPS Abstract",
                            log_lik = NULL,
                            prior_transform = NULL,
                            n_dim = NULL,
                            update_interval = NULL,
                            ...,
                            class = NULL) {
  elems <- list(
    name = name,
    log_lik = log_lik,
    prior_transform = prior_transform,
    n_dim = n_dim,
    update_interval = update_interval %||% 0L
  )

  extra_elems <- list2(...)

  x <- structure(
    c(elems, extra_elems),
    class = c(class, "ernest_lrps")
  )
  validate_ernest_lrps(x)
}

#' Validate an Ernest Sampler Object
#' @param x An object to validate.
#'
#' @returns x
#' @noRd
validate_ernest_lrps <- function(x) {
  check_function(x$log_lik, allow_null = TRUE)
  check_function(x$prior_transform, allow_null = TRUE)
  check_number_whole(x$n_dim, min = 1, allow_null = TRUE, allow_infinite = FALSE)
  check_number_whole(
    x$update_interval,
    min = 0,
    allow_infinite = FALSE
  )
  x
}

#' Refresh an ernest_lrps
#'
#' @param x An ernest_lrps object
#' @noRd
refresh_lrps <- function(x) {
  UseMethod("refresh_lrps")
}

#' Format method
#' @noRd
#' @export
format.ernest_lrps <- function(x, digits = getOption("digits"), ...) {
  cli::cli_format_method({
    cli::cli_dl(c(
      "LRPS Method" = "{x$name}",
      "No. Dimensions" = "{x$n_dim}"
    ))
  })
}

#' Print Method for ErnestSampler
#' @noRd
#' @export
print.ernest_lrps <- function(x, digits = getOption("digits"), ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Uniform Sampling in Unit Cube Subclass
#' @rdname ErnestSampler
#' @noRd
new_uniform_cube <- function(log_lik = NULL,
                             prior_transform = NULL,
                             n_dim = NULL,
                             update_interval = NULL,
                             ...) {
  new_ernest_lrps(
    name = "Uniform Sampling in Unit Cube",
    log_lik = log_lik,
    prior_transform = prior_transform,
    n_dim = n_dim,
    class = "uniform_cube"
  )
}

#' @export
#' @noRd
refresh_lrps.uniform_cube <- function(x) {
  do.call(new_uniform_cube, as.list(x))
}

#' Random Walk within the Unit Cube
#' @rdname ErnestSampler
#' @param steps The minimum number of steps to take in the random walk.
#' @param epsilon The initial step size of the random walk.
#' @noRd
new_rwmh_cube <- function(log_lik = NULL,
                          prior_transform = NULL,
                          n_dim = NULL,
                          update_interval = NULL,
                          num_steps = NULL,
                          target_acceptance = NULL,
                          epsilon = NULL,
                          ...) {
  check_number_whole(num_steps, min = 2, allow_infinite = FALSE)
  check_number_decimal(
    target_acceptance,
    min = 0,
    max = 1,
    allow_infinite = FALSE
  )
  epsilon <- unlist(tail(epsilon, 1))
  check_number_decimal(epsilon, min = 0, allow_infinite = FALSE)
  new_ernest_lrps(
    name = "Random Walk Metropolis-Hastings in Unit Hypercube",
    log_lik = log_lik,
    prior_transform = prior_transform,
    n_dim = n_dim,
    update_interval = update_interval,
    num_steps = num_steps,
    args = rlang::new_environment(data = list(
      steps = 0,
      accept = 0
    )),
    target_acceptance = target_acceptance,
    epsilon = list(epsilon),
    class = "rw_cube"
  )
}

#' @export
#' @noRd
refresh_lrps.rw_cube <- function(x) {
  do.call(new_rwmh_cube, as.list(x))
}
