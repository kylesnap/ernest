#' A likelihood-restricted prior sampler for nested sampling
#'
#' Likelihood-restricted prior sampling (or LRPS) aims to generate points within
#' the parameter space defined by the prior. Many options exist for conducting
#' LRPS, though two currently exist in ernest: uniform sampling and a random
#' walk. In ernest, LRPS objects contain the necessary information to conduct
#' sampling, and is assigned to an  R enviroment where the actual nested
#' sampling algorithm is conducted.
#'
#' @param log_lik A function that returns the log-likelihood of a point in the
#' parameter space.
#' @param prior_transform A function that transforms a unit cube to the prior
#' space.
#' @param n_dim The number of dimensions in the parameter space.
#' @param n_points The number of live points to use during the nested sampling
#' run.
#' @param first_update The number of likelihood calls that are performed using
#' the uniform sampling cube. Defaults to 300L.
#' @param between_update The number of iterations to run between updates to the
#' evidence integral. Defaults to 750L.
#' @param verbose Whether to print progress messages to the console. Defaults to
#' the value of `getOption("verbose")`.
#' @param wrk Either `NULL`, or a reference to the environment where the nested
#' sampling variables are held.
#' @param ... Extra variables that belong to some sampling method specified by
#' `ErnestLRPS`
#'
#' @name ErnestSampler
NULL

#' Construct an ernest sampler
#' @noRd
new_ernest_sampler <- function(log_lik, prior_transform, n_dim, n_points,
                               first_update, between_update,
                               verbose, wrk = NULL, ..., class = character()) {
  elems <- list(
    log_lik = log_lik,
    prior_transform = prior_transform,
    n_dim = n_dim,
    n_points = n_points,
    first_update = first_update,
    between_update = between_update,
    verbose = verbose,
    wrk = wrk
  )

  extra_elems <- list2(...)

  x <- structure(
    c(elems, extra_elems),
    class = c(class, "ErnestSampler")
  )
  validate_ernest_sampler(x)
  x
}

#' Validate an Ernest Sampler Object
#' @param x An object to validate.
#'
#' @returns x, invisibly.
validate_ernest_sampler <- function(x) {
  check_function(x$log_lik)
  check_function(x$prior_transform)
  check_number_whole(x$n_dim, min = 1, allow_infinite = FALSE)
  check_number_whole(x$n_points, min = 1, allow_infinite = FALSE)
  check_number_whole(x$first_update, min = 0, allow_infinite = FALSE)
  check_number_whole(x$between_update, min = 0, allow_infinite = FALSE)
  check_logical(x$verbose)
  check_environment(x$wrk, allow_null = TRUE)
  invisible(x)
}

#' Format method
#' @noRd
#' @export
format.ErnestSampler <- function(x, digits = getOption("digits"), ...) {
  cli::cli_format_method({
    cli::cli_h1("Nested Sampling Run from {.pkg ernest}")
    cli::cli_dl(c(
      "No. Live Points" = "{x$n_points}",
      "No. Dimensions" = "{x$n_dim}"
    ))
    if (!is.null(x$wrk)) {
      cli::cli_h3("Results")
      res <- calculate(x, progress = FALSE)
      log_evid <- prettyunits::pretty_signif(
        res$log_evidence,
        digits = digits
      )
      log_evid_err <- prettyunits::pretty_signif(
        res$log_evidence_err,
        digits = digits
      )
      eff <- prettyunits::pretty_signif(
        100 * (x$wrk$n_iter / x$wrk$n_call),
        digits = digits
      )
      cli::cli_dl(c(
        "No. Iterations" = "{x$wrk$n_iter}",
        "No. Calls" = "{x$wrk$n_call}",
        "Efficiency" = "{eff}%",
        "Log. Evidence" = "{log_evid} \u00B1 {log_evid_err}"
      ))
    } else {
      cli::cli_alert_info("Sampler has not been run.")
    }
  })
}

#' Print Method for ErnestSampler
#' @noRd
#' @export
print.ErnestSampler <- function(x, digits = getOption("digits"), ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Uniform Sampling in Unit Cube Subclass
#' @rdname ErnestSampler
#' @noRd
new_uniform_cube <- function(log_lik, prior_transform, n_dim, n_points,
                             first_update, between_update,
                             verbose, wrk = NULL) {
   new_ernest_sampler(
      log_lik = log_lik,
      prior_transform = prior_transform,
      n_dim = n_dim,
      n_points = n_points,
      first_update = first_update,
      between_update = between_update,
      verbose = verbose,
      wrk = wrk,
      class = "UniformCube"
   )
}

#' Random Walk within the Unit Cube
#' @rdname ErnestSampler
#' @param steps The minimum number of steps to take in the random walk.
#' @param epsilon The initial step size of the random walk.
#' @noRd
new_rwmh_cube <- function(log_lik, prior_transform, n_dim, n_points,
                           first_update, between_update,
                           verbose, steps, epsilon, wrk = NULL) {
  x <- new_ernest_sampler(
    log_lik = log_lik,
    prior_transform = prior_transform,
    n_dim = n_dim,
    n_points = n_points,
    first_update = first_update,
    between_update = between_update,
    verbose = verbose,
    wrk = wrk,
    steps = steps,
    epsilon = epsilon,
    class = "RandomWalkCube"
  )
  check_number_decimal(x$epsilon, min = 0, allow_infinite = FALSE)
  check_number_whole(x$steps, min = 1, allow_infinite = FALSE)
  x
}
