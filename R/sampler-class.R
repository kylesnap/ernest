#' Statuses for ernest sampler
#' @noRd
statuses <- c("UNINITIALIZED", "RUNNING", "MAX_IT", "MAX_CALL", "MIN_LOGZ")

#' @name ernest_sampler-class
#' @title The Ernest Nested Sampler Object
#'
#' @description
#' An R6 class that contains a nested sampling run.
#'
#' This object is normally created by calling [nested_sampling()], and
#' interacted with by calling S3 methods like [generate()] and [calculate()].
#'
#' @importFrom R6 R6Class
ernest_sampler <- R6Class(
  "ernest_sampler",
  public = list(
    #' @description
    #' Creates a new `ernest_sampler`.
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
      check_number_whole(n_points, min = 1)
      first_update <- round_to_integer(first_update, n_points)
      check_number_whole(first_update, min = 0)
      update_interval <- round_to_integer(update_interval, n_points)
      check_number_whole(update_interval, min = 0)

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
    #' Clears the previous runs from the sampler, including the sampler's live
    #' points.
    #'
    #' @return Itself, invisibly.
    clear = function() {
      private$lrps = private$lrps$clear()
      private$live_unit = matrix(double(0L))
      private$live_log_lik = double(0L)
      private$live_birth = double(0L)
      private$results = NULL
      private$status = statuses[1L]
      invisible(self)
    },

    #' @description
    #' Generates a sample of live points from the prior and validates them.
    #'
    #' @param clear A logical value indicating whether to clear existing points.
    #'
    #' @return Itself, invisibly.
    compile = function(clear = FALSE) {
      check_bool(clear)
      if (clear) {
        self$clear()
      }
      if (any(map_lgl(self$live_points, is_null))) {
        live <- create_live(
          private$lrps,
          private$n_points,
          private$prior$n_dim,
          call = current_env()
        )
        private$live_unit <- live$unit
        private$live_log_lik <- live$log_lik
        private$live_birth <- rep(0, private$n_points)
      }
      check_live(
        private$live_unit,
        private$live_log_lik,
        private$n_points,
        private$prior$n_dim
      )
      invisible(self)
    },

    #' @description
    #' Performs nested sampling until a stopping criterion is met.
    #'
    #' @param max_iterations The maximum number of iterations to perform.
    #' @param max_calls The maximum number of calls to the likelihood function.
    #' @param min_logz The minimum log-evidence value to achieve.
    #' @param verbose Whether to print updates on the sampler's progress.
    #'
    #' @return Itself, invisibly.
    generate = function(
      max_iterations = Inf,
      max_calls = Inf,
      min_logz = 0.05,
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

      if (max_iterations == Inf) max_iterations <- .Machine$integer.max
      if (max_calls == Inf) max_calls <- .Machine$integer.max
      check_number_whole(max_iterations, min = 1)
      check_number_whole(max_calls, min = 1)
      check_number_decimal(min_logz, min = 0)

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
        self$compile(clear = TRUE)
      } else {
        self$compile(clear = FALSE)
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
    #' Prints a brief summary of the sampler.
    #'
    #' @param ... These dots are for future extensions and must be empty.
    #'
    #' @return Itself, invisibly.
    print = function(...) {
      check_dots_empty()
      cli::cli_h1("Ernest Nested Sampler")
      if (is_empty(private$results)) {
        cli::cli_dl(c(
          "No. Points" = "{private$n_points}",
          "No. Iterations" = "{self$niterations}",
          "No. Calls" = "{self$ncalls}"
        ))
        cli::cli_inform(c(
          "No results compiled yet.",
          "(see {.fn generate.ernest_sampler})."
        ))
      } else {
        cli::cli_text("Previous Run Results:")
        cli::cat_print(private$results)
      }
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
    status = statuses[1L],

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

    #' @field live_points The matrix of live points currently in the sampler,
    #' scaled to the unit-cube.
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
