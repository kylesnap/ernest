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
    #' @param log_lik_fn A function representing the log-likelihood.
    #' @param prior An `ernest_prior` object defining the prior distribution.
    #' @param sampler A likelihood-restricted prior sampler (LRPS).
    #' @param n_points The number of live points.
    #' @param first_update The first update interval.
    #' @param update_interval The subsequent update interval.
    #'
    #' @return Itself, invisibly.
    initialize = function(
      log_lik_fn,
      prior,
      sampler,
      n_points,
      first_update,
      update_interval
    ) {
      if (!inherits(sampler, "lrps_call")) {
        cli::cli_abort("sampler must be of class {.cls lrps_call}.")
      }
      check_number_whole(n_points, min = 1)
      first_update <- round_to_integer(first_update, n_points)
      update_interval <- round_to_integer(update_interval, n_points)

      sampler <- call_modify(
        sampler,
        log_lik_fn = log_lik_fn,
        prior_fn = compile(prior),
        n_dim = nvariables(prior)
      )
      private$lrps <- eval(sampler)
      private$log_lik <- log_lik_fn
      private$prior <- prior
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
      private$lrps <- private$lrps$clear()
      private$live <- list()
      private$dead <- list()
      private$integration <- tibble::tibble(
        "log_volume" = numeric(),
        "log_weight" = numeric(),
        "log_evidence" = numeric()
      )
      private$n_iter <- 0L
      private$n_call <- 0L
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
      if (is_empty(private$live)) {
        private$live <- create_live(
          private$lrps,
          private$n_points,
          nvariables(private$prior)
        )
      }
      check_live(private$live, private$n_points, nvariables(private$prior))
    },

    #' @description
    #' Performs nested sampling until a stopping criterion is met.
    #'
    #' @param max_iterations The maximum number of iterations to perform.
    #' @param max_calls The maximum number of calls to the likelihood function.
    #' @param min_logz The minimum log-evidence value to achieve.
    #'
    #' @return Itself, invisibly.
    generate = function(
      max_iterations = Inf,
      max_calls = Inf,
      min_logz = 0.05
    ) {
      check_number_whole(
        max_iterations,
        min = 1,
        allow_infinite = TRUE,
        allow_null = FALSE
      )
      check_number_whole(
        max_calls,
        min = 1,
        allow_infinite = TRUE,
        allow_null = FALSE
      )
      check_number_decimal(
        min_logz,
        min = 0,
        allow_infinite = FALSE,
        allow_null = FALSE
      )

      if (
        is.infinite(max_iterations) & is.infinite(max_calls) & is.null(min_logz)
      ) {
        cli::cli_abort(c(
          "No stopping condition set.",
          "i" = "Consider setting `max_iterations`, `max_calls`, or `min_logz`."
        ))
      }
      max_iterations <- if (max_iterations == Inf) {
        .Machine$integer.max
      } else {
        as.integer(max_iterations)
      }
      max_calls <- if (max_calls == Inf) {
        .Machine$integer.max
      } else {
        as.integer(max_calls)
      }
      min_logz <- as.double(min_logz)

      self$compile()
      nested_sampling_impl(self, private, max_iterations, max_calls, min_logz)
      invisible(self)
    },

    #' @description
    #' Accesses the sampler's live points.
    #'
    #' @param units A string, either `"original"` or `"unit"`.
    #' @param reorder Whether to reorder the points by increasing
    #' log-likelihood.
    #'
    #' @return A `tibble` containing the live points.
    get_live_points = function(units = c("original", "unit"), reorder = FALSE) {
      if (is_empty(private$live)) {
        inform("No live points have been generated yet.")
        return(tibble::tibble(
          !!!setNames(
            replicate(length(self$variables), double(0)),
            self$variables
          )
        ))
      }
      units <- arg_match(units)
      mat <- if (units == "original") {
        private$live$point
      } else {
        private$live$unit
      }
      if (reorder) {
        mat <- mat[order(private$live$log_lik), ]
      }
      colnames(mat) <- variables(private$prior)
      tibble::as_tibble(mat)
    },

    #' @description
    #' Accesses the sampler's discarded (dead) points.
    #'
    #' @param units A string, either `"original"` or `"unit"`.
    #'
    #' @return A `tibble` containing the dead points.
    get_dead_points = function(units = c("original", "unit")) {
      if (is_empty(private$dead)) {
        inform("No dead points have been generated yet.")
        return(tibble::tibble(
          !!!setNames(
            replicate(length(self$variables), double(0)),
            self$variables
          )
        ))
      }
      units <- arg_match(units)
      mat <- if (units == "original") {
        private$dead$point
      } else {
        private$dead$unit
      }
      colnames(mat) <- variables(private$prior)
      tibble::as_tibble(mat)
    },

    #' @description
    #' Calculates the evidence integral of the nested sampling run.
    #'
    #' @param include_live Whether to include live points in the calculation.
    #'
    #' @return A `tibble` with columns reporting the results of the run.
    calculate = function(include_live = TRUE) {
      dead_int <- private$integration
      if (nrow(dead_int) < 1L) {
        warn("No iterations have been performed yet.")
        return(NULL)
      }
      dead_lik <- list_c(private$dead$log_lik)

      if (!include_live) {
        tibble::tibble(
          "log_likelihood" = dead_lik,
          !!!dead_int
        )
      } else {
        last_vol <- tail(dead_int$log_volume, 1)
        live_vol <- last_vol +
          log1p(
            (-1 - private$n_points)^(-1) * seq_len(private$n_points)
          )
        log_lik <- c(dead_lik, sort(private$live$log_lik))
        log_vol <- c(dead_int$log_volume, live_vol)
        compute_integral(log_lik, log_vol)
      }
    },

    #' @description
    #' Summarize the nested sampling run.
    #'
    #' @return See [summary.ernest_sampler()]
    summary = function() new_es_summary(self, private),

    #' @description
    #' Prints a brief summary of the sampler.
    #'
    #' @param ... Additional arguments, forwarded to [prettyNum()].
    #'
    #' @return Itself, invisibly.
    print = function(...) {
      check_dots_used(...)
      cli::cli_h1("Ernest Nested Sampler")
      cli::cli_dl(c(
        "No. Points" = "{private$n_points}",
        "No. Iterations" = "{self$niterations}",
        "No. Calls" = "{self$ncalls}"
      ))
      if (is_empty(private$dead)) {
        cli::cli_alert_info("No iterations have been performed yet.")
      } else {
        cli::cli_h3("Results")
        calc <- self$calculate()
        log_range <- prettyNum(range(calc$log_likelihood))
        calc <- tail(calc, 1L)
        cli::cli_dl(c(
          "Ln. Likelihood" = "[{log_range[1]}, {log_range[2]}]",
          "Ln. Volume" = "{prettyNum(calc$log_volume, ...)}",
          "Ln. Evidence" = "{prettyNum(calc$log_evidence, ...)}"
        ))
      }
      invisible(self)
    }
  ),
  private = list(
    lrps = NULL,
    log_lik = NULL,
    prior = NULL,
    n_points = NULL,
    first_update = NULL,
    update_interval = NULL,

    live = list(),
    dead = list(),
    integration = tibble::tibble(
      "log_volume" = numeric(),
      "log_weight" = numeric(),
      "log_evidence" = numeric()
    ),
    n_iter = 0L,
    n_call = 0L
  ),
  active = list(
    #' @field niterations The total number of sampling iterations.
    niterations = function() {
      private$n_iter
    },

    #' @field ncalls The total calls made to the likelihood function.
    ncalls = function() {
      private$n_call
    },

    #' @field variables The variables associated with the prior.
    variables = function(value) {
      if (missing(value)) {
        return(variables(private$prior))
      }
      variables(private$prior) <- value
    }
  )
)
