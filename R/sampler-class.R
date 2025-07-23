#' Internal: The Ernest Nested Sampler R6 Class
#'
#' @description
#' The `ernest_sampler` is an R6 class that implements the core logic for a
#' nested sampling run.
#'
#' @details
#' This class is primarily intended for internal use within the `ernest`
#' package. Users should interact with nested sampling samplers via the
#' provided S3 generics rather than calling R6 methods directly.
#'
#' The object is documented for developers and advanced users who want to
#' extend or debug ernest.
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
    #' Create a new `ernest_sampler`.
    #'
    #' @param log_lik_fn,prior,sampling,n_points,first_update,update_interval
    #' See [nested_sampling()].
    #'
    #' @return The sampler, invisibly.
    initialize = function(
      log_lik_fn,
      prior,
      sampling,
      n_points,
      first_update,
      update_interval
    ) {
      is_class(log_lik_fn, "ernest_likelihood")
      is_class(prior, "ernest_prior")
      is_class(sampling, "ernest_sampling")
      n_points <- as_scalar_count(n_points)
      first_update <- as_scalar_count(n_points, positive = FALSE)
      update_interval <- as_scalar_count(n_points, positive = TRUE)

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
    #' Clears the previous runs from the sampler, including the sampler's
    #' live points.
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
    #' Generates a sample of live points from the prior and validates them.
    #'
    #' @param seed,clear See [compile.ernest_sampler()].
    #'
    #' @return Itself, invisibly.
    compile = function(seed = NA, clear = FALSE) {
      clear <- as_scalar_logical(clear)
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
    #' Performs nested sampling until a stopping criterion is met.
    #'
    #' @param max_iterations,max_calls,min_logz,seed See
    #' [generate.ernest_sampler()].
    #'
    #' @srrstats {BS2.6} generate checks that the stopping criteria are
    #' reasonable (e.g., if `x` has already been run, generate ensures that the
    #' user isn't entering already invalid criteria).
    #' @return Itself, invisibly.
    generate = function(
      max_iterations = Inf,
      max_calls = Inf,
      min_logz = 0.05,
      seed = NA
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
      max_iterations <- as_scalar_integer(max_iterations)
      max_calls <- as_scalar_count(max_calls)

      try_fetch(
        {
          max_iterations <- as_scalar_integer(
            max_iterations,
            min = self$niterations
          )
          max_calls <- as_scalar_integer(max_calls, min = self$ncalls)
        },
        error = function(cnd) {
          if (self$niterations > 0) {
            msg <- sprintf(
              "%s %s",
              "The sampler already has {self$niterations} iterations",
              "and {self$ncalls} likelihood calls."
            )
            cli::cli_abort(c("Invalid stopping criteria.", "i" = msg))
          } else {
            cli::cli_abort("Invalid stopping criteria.")
          }
        }
      )
      min_logz <- as_scalar_double(min_logz, min = 0)

      if (!is_empty(private$results)) {
        log_vol <- private$results$log_vol[private$results$n_iter]
        log_z <- private$results$log_evidence[private$results$n_iter]
        d_log_z <- logaddexp(0, max(private$live_log_lik) + log_vol - log_z)
        if (d_log_z <= min_logz) {
          cur_log_z <- round(d_log_z, 3)
          cli::cli_abort(c(
            "`min_logz` must be less than the estimated remaining evidence.",
            "!" = "Remaining log evidence is {cur_log_z}."
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
        as.double(min_logz)
      )
      private$results <- do.call(
        compile_results,
        list2(self = self, private = private, !!!result)
      )
      return(private$results)
    },

    #' @description
    #' Prints a brief description of the sampler.
    #'
    #' @param ... Ignored.volume
    #'
    #' @return Itself, invisibly.
    print = function(...) {
      log_z <- if (!is_empty(private$results)) {
        summary(private$results)$log_evidence
      } else {
        NULL
      }
      cli::cli_div(theme = list(.val = list(digits = 3)))
      line <- sprintf(
        "%s %s %s",
        "An {.cls ernest_sampler}:",
        "{private$n_points} points x {self$niterations} iter.",
        "x {self$ncalls} lik. calls"
      )
      cli::cli_bullets(c(line))
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

    #' @field live_points A list, containing `unit`, the current matrix of live
    #' points in the hypercube scale, and `log_lik` containing the corresponding
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
    #' sampling runs, or `NULL` if no results exist.
    run = function() {
      private$results
    }
  )
)
