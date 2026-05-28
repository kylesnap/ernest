#' Run nested sampling to estimate Bayesian evidence
#'
#' Executes the nested sampling algorithm, iteratively replacing the worst live
#' point with a new sample from a likelihood-restricted prior until a stopping
#' criterion is met.
#'
#' @param x [[ernest_sampler]] or [[ernest_run]]\cr A nested sampling
#' specification.
#' @inheritDotParams compile.ernest_run -object
#' @param max_iterations `[integer(1)]`\cr The maximum number of iterations to
#' perform. Optional; if `NULL` this criterion is ignored.
#' @param max_evaluations `[integer(1)]`\cr The maximum number of times the run
#' can evaluate the likelihood function. Optional; if `NULL` this
#' criterion is ignored.
#' @param min_logz `[double(1)]`\cr The minimum log-ratio between the
#' current estimated evidence and the remaining evidence. Must be non-negative;
#' if set to zero, this criterion is ignored.
#' @param show_progress `[logical(1)]`\cr If `TRUE`, displays a progress spinner
#' and iteration counter during sampling. Optional; if `NULL` the global option
#' `rlib_message_verbosity` is used to determine whether to show progress.
#' @param parallel `[logical(1) or integer(1)]`\cr
#' `r lifecycle::badge("experimental")` Specifies whether the run should be
#' performed in-parallel across multiple [mirai::daemons()]. If `TRUE`,
#' the run is split across the current number of daemons.
#' See [parallelization] for more details.
#'
#' @returns An `[ernest_run]` object with the nested sampling results.
#'
#' This inherits from [ernest_sampler] and adds:
#' * `niter`: `[integer(1)]` Number of iterations performed.
#' * `neval`: `[integer(1)]` Number of times the likelihood function was
#' evaluated.
#' * `log_evidence`: `[double(1)]` The log-evidence estimate.
#' * `log_evidence_err`: `[double(1)]` The standard error of the log-evidence
#' estimate, derived using `information`.
#' * `information`: `[double(1)]` The estimated Kullback-Leibler divergence.
#' * `log_weight`: `[double(nsample)]` Each sample's contribution to the
#' log-evidence estimate, computed from its log-likelihood and prior volume.
#' * `rcrd`: [[ernest_rcrd]] An object storing an internal record of each point
#' generated during the run.
#'
#' @details
#' At least one of `max_iterations`, `max_evaluations`, or `min_logz`
#' must specify a valid stopping criterion. Setting `min_logz` to zero while
#' leaving `max_iterations` and `max_evaluations` at their defaults will
#' result in an error.
#'
#' If `x` is an `ernest_run` object, the stopping criteria are checked against
#' the current state of the run. An error is thrown if the stopping criteria
#' have already been satisfied by `x`.
#'
#' The `min_logz` parameter controls the relative tolerance for the remaining
#' evidence in the unexplored parameter space. Sampling stops when the estimated
#' remaining evidence is sufficiently small compared to the accumulated
#' evidence.
#'
#' `rcrd` will have size `niter + nlive`: The first `niter` entries correspond
#' to the points removed during the run, the last `nlive` entries correspond to
#' the points within the live set at the end of the run.
#'
#' @references Skilling, J. (2006). Nested Sampling for General Bayesian
#' Computation. Bayesian Analysis, 1(4), 833–859. \doi{10.1214/06-BA127}
#'
#' @srrstats {BS2.12} The `show_progress` indicator controls whether a simple
#' spinner bar is shown during sampling.
#' @srrstats {BS4.0} References the paper containing the sampling algorithm.
#'
#' @seealso [calculate.ernest_run()] [summary.ernest_run()]
#' [weights.ernest_run()]
#'
#' @examples
#' prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, nlive = 100)
#' sampler
#'
#' # Stop sampling after a set number of iterations or likelihood evaluations.
#' generate(sampler, max_iterations = 100)
#'
#' # Use the default stopping criteria
#' \dontrun{ generate(sampler) }
#' @aliases ernest_run
#' @rdname generate-ernest
#' @export
generate.ernest_sampler <- function(
  x,
  max_iterations = NULL,
  max_evaluations = NULL,
  min_logz = 0.05,
  show_progress = NULL,
  parallel = FALSE,
  ...
) {
  if (is.null(show_progress)) {
    show_progress <- getOption("rlib_message_verbosity", "default") != "quiet"
  }
  check_bool(show_progress)
  control <- generate_control(
    max_iterations,
    max_evaluations,
    min_logz,
    seed = attr(x, "seed"),
    nlive = x$nlive,
    first_update = x$first_update,
    update_interval = x$update_interval
  )
  x <- compile(x, ...)

  if (!isFALSE(parallel)) {
    results <- nested_sampling_parallel(
      x,
      control = control,
      show_progress = show_progress,
      parallel = parallel
    )
    new_ernest_run(x, results$results, .parallel = results$.parallel)
  } else {
    results <- nested_sampling_impl(
      live_env = x$live_env,
      lrps = x$lrps,
      control = control,
      show_progress = show_progress
    )
    new_ernest_run(x, results)
  }
}

#' @srrstats {BS2.8} Calling generate on an ernest_run will continue the run
#' from the last known live set.
#'
#' @rdname generate-ernest
#' @export
generate.ernest_run <- function(
  x,
  max_iterations = NULL,
  max_evaluations = NULL,
  min_logz = 0.05,
  show_progress = NULL,
  parallel = FALSE,
  ...
) {
  if (is.null(show_progress)) {
    show_progress <- getOption("rlib_message_verbosity", "default") != "quiet"
  }
  check_bool(show_progress)
  x <- compile(x, ...)
  if (inherits_only(x, "ernest_sampler")) {
    return(NextMethod())
  }

  x_rcrd <- x$rcrd
  control <- generate_control(
    max_iterations,
    max_evaluations,
    min_logz,
    seed = attr(x, "seed"),
    nlive = x$nlive,
    first_update = x$first_update,
    update_interval = x$update_interval,
    rcrd = x_rcrd
  )
  prev_run <- x$rcrd[get_dead_idx(x$rcrd)]

  if (!isFALSE(parallel)) {
    results <- nested_sampling_parallel(
      x,
      control = control,
      show_progress = show_progress,
      parallel = parallel
    )
    new_ernest_run(
      x,
      c(prev_run, results$results),
      .parallel = results$.parallel
    )
  } else {
    results <- nested_sampling_impl(
      live_env = x$live_env,
      lrps = x$lrps,
      control = control,
      show_progress = show_progress
    )
    new_ernest_run(x, c(prev_run, results))
  }
}

#' Generate a list of control parameters for nested sampling
#'
#' @param max_iterations,max_evaluations,min_logz User-requested stopping
#' parameters.
#' @param seed,nlive,first_update,update_interval Parameters from the sampler
#' object.
#' @param rcrd An optional `ernest_rcrd` object from a previous run, used to
#' initialize the control parameters if continuing a run.
#' @param parallel Signals whether a run is being conducted in parallel,
#' so that the nlive can be adjusted before recomputing from an already
#' completed run.
#'
#' @return A named list containing:
#' * Run meta info: `seed`, `nlive`, `first_update`, `update_interval`
#' * Validated stopping criteria: `max_iterations`, `max_evaluations`,
#' `min_logz`.
#' * Run state: `last_criterion`, `log_z`, `log_vol`, `cur_iter`, `cur_eval`.
#' @noRd
generate_control <- function(
  max_iterations,
  max_evaluations,
  min_logz,
  seed,
  nlive,
  first_update,
  update_interval,
  rcrd = NULL,
  parallel = FALSE,
  call = caller_env()
) {
  # Initialize control parameters to default.
  control <- list(
    seed = seed,
    nlive = nlive,
    first_update = first_update,
    update_interval = update_interval
  )
  last_criterion <- -1e300
  log_vol <- 0
  log_z <- -1e300
  cur_iter <- 0L
  cur_eval <- 0L
  d_log_z <- Inf

  if (!is.null(rcrd)) {
    # If nlive > number of unique points, then refactor the rcrd to avoid issues
    act_nlive <- max(field(rcrd, "nlive"))
    if (act_nlive < control$nlive && parallel) {
      vctrs::field(rcrd, "nlive") <- get_points(
        field(rcrd, "log_lik"),
        control$nlive,
        TRUE
      )
    }
    prev_integration <- compute_integral(rcrd)
    cur_iter <- vctrs::vec_size(rcrd) - control$nlive
    cur_eval <- sum(field(rcrd, "neval"))
    last_criterion <- prev_integration$log_lik[[cur_iter]]
    log_z <- prev_integration$log_evidence[[cur_iter]]
    log_vol <- prev_integration$log_vol[[cur_iter]]
    max_lik <- max(field(rcrd, "log_lik"))
    d_log_z <- logspace_add_c(0, max_lik + log_vol - log_z)
  }

  # Check stopping criteria
  no_stopping <- all(
    identical(min_logz, 0),
    is.null(max_iterations),
    is.null(max_evaluations)
  )
  if (no_stopping) {
    cli::cli_abort(
      c(
        "At least one of `max_iterations`, `max_evaluations`, or `min_logz` must",
        "specify a valid stopping criterion."
      ),
      call = call
    )
  }
  max_iterations <- max_iterations %||% .Machine$integer.max
  max_evaluations <- max_evaluations %||% .Machine$integer.max

  check_number_whole(
    max_iterations,
    min = cur_iter + 1.0,
    allow_null = TRUE,
    call = call
  )
  check_number_whole(
    max_evaluations,
    min = cur_eval + 1.0,
    allow_null = TRUE,
    call = call
  )
  check_number_decimal(
    min_logz,
    min = 0,
    max = round(d_log_z, getOption("digits", 7L)),
    call = call
  )

  list2(
    !!!control,
    max_iterations = as.integer(max_iterations),
    max_evaluations = as.integer(max_evaluations),
    min_logz = as.double(min_logz),
    last_criterion = as.double(last_criterion),
    log_vol = as.double(log_vol),
    log_z = as.double(log_z),
    cur_iter = as.integer(cur_iter),
    cur_eval = as.integer(cur_eval)
  )
}
