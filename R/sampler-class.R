#' Internal: The Ernest Nested Sampler R6 Class
#'
#' @description
#' The `ernest_sampler` is an R6 class that implements the core logic for a
#' nested sampling run.
#'
#' @details
#' This class is primarily intended for internal use within the `ernest` package.
#' Users should interact with nested sampling samplers via the provided S3 generics,
#' rather than calling R6 methods directly.
#'
#' The R6 interface is documented here for developers and advanced users who
#' want to extend or debug ernest.
#'
#' @section Usage:
#'
#' Users are not able to instantiate this class, as it is not exported.
#' Instead, they use the following functions:
#' - To create a new sampler, use [nested_sampling()].
#' - To compile live points, use [compile()].
#' - To run nested sampling, use [generate()].
#' - To access the latest results, use the `$run` active binding.
#'
#' @keywords internal
#' @rdname ernest_sampler-class
#' @aliases ernest_sampler
#' @importFrom R6 R6Class
ernest_sampler <- R6Class(
  "ernest_sampler",
  public = list(
    #' @description
    #' (Internal) Creates a new `ernest_sampler`.
    #'
    #' @param log_lik_fn An `ernest_likelihood` object.
    #' @param prior An `ernest_prior` object.
    #' @param sampling An `ernest_sampling` object.
    #' @param n_points The number of live points.
    #' @param first_update The first update interval.
    #' @param update_interval The subsequent update interval.
    #'
    #' @return Itself, invisibly.
    initialize = function(
      log_lik_fn,
      prior,
      sampling,
      n_points,
      first_update,
      update_interval
    ) {
      if (!inherits(log_lik_fn, "ernest_likelihood")) {
        stop_input_type(log_lik_fn, "ernest_likelihood")
      }
      if (!inherits(prior, "ernest_prior")) {
        stop_input_type(prior, "ernest_prior")
      }
      if (!inherits(sampling, "ernest_sampling")) {
        stop_input_type(sampling, "ernest_sampling")
      }
      n_points <- check_integer(n_points, min = 1)
      first_update <- check_integer(first_update, min = 0)
      update_interval <- check_integer(update_interval, min = 0)

      private$log_lik <- log_lik_fn
      private$prior <- prior
      sampling_args <- pairlist2(
        log_lik_fn = log_lik_fn,
        prior_fn = prior$fn,
        n_dim = prior$n_dim,
        !!!sampling$parameters
      )
      private$lrps <- eval(call2(sampling$class$new, !!!sampling_args))
      private$n_points <- n_points
      private$first_update <- first_update
      private$update_interval <- update_interval
      invisible(self)
    },

    #' @description
    #' (Internal) Clears the previous runs from the sampler, including the
    #' sampler's live points.
    #'
    #' @return Itself, invisibly.
    clear = function() {
      private$lrps <- private$lrps$clear()
      private$live_unit <- matrix(double(0L))
      private$live_log_lik <- double(0L)
      private$live_birth <- double(0L)
      private$results <- NULL
      private$status <- "UNINITIALIZED"
      private$seed <- NULL
      invisible(self)
    },

    #' @description
    #' (Internal) Generates a sample of live points from the prior and
    #' validates them.
    #'
    #' @param seed An optional integer seed for reproducibility, `NULL` to
    #' reset the seed, and `NA` to use the current random seed.
    #' @param clear A logical value indicating whether to clear existing points.
    #'
    #' @return Itself, invisibly.
    compile = function(seed = NA, clear = FALSE) {
      check_bool(clear)
      private$seed <- set_random_seed(seed, private$results)

      if (clear) {
        self$clear()
      }
      if (identical(private$live_unit, matrix(double(0L)))) {
        live <- create_live(private$lrps, private$n_points)
        private$live_unit <- live$unit
        private$live_log_lik <- live$log_lik
        private$live_birth <- rep(0, private$n_points)
      }
      try_fetch(
        check_live(
          private$live_unit,
          private$live_log_lik,
          private$n_points,
          private$prior$n_dim
        ),
        error = function(cnd) {
          cli::cli_abort(
            c(
              "Live points validation failed.",
              "i" = "Should the sampler be reset with `clear`?"
            ),
            parent = cnd
          )
        }
      )
      invisible(self)
    },

    #' @description
    #' (Internal) Performs nested sampling until a stopping criterion is met.
    #'
    #' @param max_iterations,max_calls,min_logz,seed,verbose See [generate.ernest_sampler()].
    #'
    #' @return Itself, invisibly.
    generate = function(
      max_iterations = Inf,
      max_calls = Inf,
      min_logz = 0.05,
      seed = NA,
      verbose = FALSE
    ) {
      if (private$status == "RUNNING") {
        cli::cli_warn(c(
          "Sampler can't contain a half-completed run.",
          "i" = "Rolling-back sampler to {self$niterations} iteration{?s}."
        ))
        if (self$niterations > 0) {
          private$live_unit <-
            private$results$samples_u[attr(private$results, "live_loc"), ]
          private$live_log_lik <-
            private$results$log_lik[attr(private$results, "live_loc")]
          private$live_birth <-
            private$results$birth[attr(private$results, "live_loc")]
          private$status <- "MAX_IT"
        } else {
          private$status <- "UNINITIALIZED"
        }
      }

      if (identical(c(max_iterations, max_calls, min_logz), c(Inf, Inf, 0))) {
        cli::cli_abort(c(
          "Can't run `generate` without a stopping criteria.",
          "i" = "At least one of `max_iterations`, `max_calls` must be finite.",
          "i" = "Alternatively, `min_logz` must be greater than 0."
        ))
      }

      if (max_iterations == Inf) {
        max_iterations <- .Machine$integer.max
      }
      if (max_calls == Inf) {
        max_calls <- .Machine$integer.max
      }
      max_iterations <- check_integer(max_iterations, min = 1)
      max_calls <- check_integer(max_calls, min = 1)
      min_logz <- check_double(min_logz, min = 0)

      if (self$niterations != 0 && max_iterations <= self$niterations) {
        cli::cli_abort(c(
          "`max_iterations` must be greater than the current number of iterations.",
          "i" = "Can't set `max_iterations` to {max_iterations}.",
          "i" = "Already performed {self$niterations} iterations."
        ))
      }
      if (self$ncalls != 0 && max_calls <= self$ncalls) {
        cli::cli_abort(c(
          "`max_calls` must be greater than the current number of calls.",
          "i" = "Can't set `max_calls` to {max_calls}.",
          "i" = "Already performed {self$ncalls} calls."
        ))
      }
      if (!is_empty(private$results)) {
        log_vol <- private$results$log_vol[private$results$n_iter]
        log_z <- private$results$log_evidence[private$results$n_iter]
        d_log_z <- logaddexp(0, max(private$live_log_lik) + log_vol - log_z)
        if (d_log_z <= min_logz) {
          cli::cli_abort(c(
            "`min_logz` must be less than the estimated contribution of the remaining prior volume to the evidence.",
            "i" = "Can't set `min_logz` to {min_logz}.",
            "i" = "Current est. remaining contribution log volume is {round(d_log_z, 3)}."
          ))
        }
      }

      if (private$status == "UNINITIALIZED") {
        self$compile(seed = seed, clear = TRUE)
      } else {
        self$compile(seed = seed, clear = FALSE)
      }
      result <- nested_sampling_impl(
        self,
        private,
        as.integer(max_iterations),
        as.integer(max_calls),
        as.double(min_logz),
        verbose
      )
      private$results <- do.call(
        compile_results,
        list2(self = self, private = private, !!!result)
      )
      return(private$results)
    },

    #' @description
    #' (Internal) Prints a brief summary of the sampler.
    #'
    #' @param ... Ignored.
    #'
    #' @return Itself, invisibly.
    print = function(...) {
      log_z <- if (!is_empty(private$results)) {
        summary(private$results)$log_evidence
      } else {
        NULL
      }
      cli::cli_div(theme = list(.val = list(digits = 3)))
      cli::cli_bullets(
        "An {.cls ernest_sampler}: {private$n_points} points x {self$niterations} iter. x {self$ncalls} lik. calls"
      )
      invisible(self)
    }
  ),
  private = list(
    log_lik = NULL,
    prior = NULL,
    lrps = NULL,
    n_points = NULL,
    first_update = NULL,
    update_interval = NULL,
    status = "UNINITIALIZED",
    seed = NULL,

    live_unit = matrix(double(0L)),
    live_log_lik = double(0L),
    live_birth = double(0L),

    results = NULL
  ),
  active = list(
    #' @field niterations The total number of sampling iterations.
    niterations = function() {
      private$results$n_iter %||% 0L
    },

    #' @field ncalls The total calls made to the likelihood function.
    ncalls = function() {
      private$results$n_call %||% 0L
    },

    #' @field live_points A list, containing the matrix of live points currently
    #' in the sampler in unit-cube units, and a vector of their associated
    #' log likelihood values.
    live_points = function() {
      if (vctrs::vec_size(private$live_unit) == 0L) {
        list(
          "unit" = NULL,
          "log_lik" = NULL
        )
      } else {
        list(
          "unit" = private$live_unit,
          "log_lik" = private$live_log_lik
        )
      }
    },

    #' @field run The `ernest_run` object binding the results of the previous
    #' sampling runs. Returns `NULL` if no runs have been performed.
    run = function() {
      private$results
    }
  )
)
