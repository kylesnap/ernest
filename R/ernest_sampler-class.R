#' @name ernest_sampler-class
#' @title The Ernest Nested Sampler Object
#'
#' @description
#' An R6 class that contains a nested sampling run.
#'
#' This object is normally created by calling [nested_sampling()], and
#' interacted with by calling S3 methods like [generate()] and [calculate()].
#' @importFrom R6 R6Class
ernest_sampler <- R6Class(
  "ernest_sampler",
  public = list(
    #' @description
    #' Creates a new `ernest_sampler`.
    #'
    #' @return Itself, invisibly.
    initialize = function(log_lik_fn, prior, sampler, n_points, first_update, update_interval) {
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
    #' Clear the previous runs from the sampler, including the sampler's live points.
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
    #' Generate a sample of live points from the prior, and check the live
    #' points for finite values.
    #'
    #' @return Itself, invisibly.
    #' @seealso [compile.ernest_sampler()]
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
    #' Generate samples from nested sampling until a given criterion is met.
    #'
    #' @param max_iterations The maximum number of iterations to perform. If set to
    #' Inf, this stopping criterion is ignored.
    #' @param max_calls The maximum number of calls to the likelihood function.
    #' If set to Inf, this stopping criterion is ignored.
    #' @param min_logz The minimum log-evidence value to achieve. Must be a number
    #' strictly larger than zero.
    #' @param refresh Whether to clear existing points from the sampler, starting
    #' a run from scratch.
    #'
    #' @return itself, invisibly.
    #' @seealso [generate.ernest_sampler()]
    generate = function(max_iterations = Inf, max_calls = Inf, min_logz = 0.05) {
      check_number_whole(max_iterations, min = 1, allow_infinite = TRUE, allow_null = FALSE)
      check_number_whole(max_calls, min = 1, allow_infinite = TRUE, allow_null = FALSE)
      check_number_decimal(min_logz, min = 0, allow_infinite = FALSE, allow_null = TRUE)

      if (is.infinite(max_iterations) & is.infinite(max_calls) & is.null(min_logz)) {
        rlang::abort("At least one of `max_iterations`, `max_calls`, or `min_logz` must represent a stopping condition.")
      }
      max_iterations <- if (max_iterations == Inf) {
        .Machine$integer.max
      } else as.integer(max_iterations)
      max_calls <- if (max_calls == Inf) {
        .Machine$integer.max
      } else as.integer(max_calls)
      min_logz <- min_logz %||% 0

      self$compile()
      nested_sampling_impl(self, private, max_iterations, max_calls, min_logz)
      invisible(self)
    },

    #' @description
    #' Access the sampler's live points.
    #'
    #' @param units A string, either `"original"` or `"unit"`.
    #' If `"original"`, the original points are returned.
    #' If `"unit"`, the unit points are returned.
    #' @param reorder Whether to reorder the points so they appear in increasing
    #' log-likelihood values.
    #'
    #' @return A `tibble` containing the live points.
    get_live_points = function(units = c("original", "unit"), reorder = FALSE) {
      if (is_empty(private$live)) {
        inform("No live points have been generated yet.")
        return(tibble::tibble(!!!setNames(
          replicate(length(self$variables), double(0)),
          self$variables
        )))
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
    #' Access the discarded (or `dead`) points list.
    #'
    #' @param units A string, either `"original"` or `"unit"`.
    #' If `"original"`, the original points are returned.
    #' If `"unit"`, the unit points are returned.
    #'
    #' @return A `tibble` containing the dead points.
    get_dead_points = function(units = c("original", "unit")) {
      if (is_empty(private$dead)) {
        inform("No dead points have been generated yet.")
        return(tibble::tibble(!!!setNames(
          replicate(length(self$variables), double(0)),
          self$variables
        )))
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
    #' Calculate the evidence integral of the nested sampling run.
    #'
    #' @param include_live Whether to include the live points in the calculation.
    #'
    #' @return A `tibble` with columns reporting the results of the run.
    #' @seealso [calculate.ernest_sampler()]
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
        live_vol <- last_vol + log1p(
          (-1 - private$n_points)^(-1) * seq_len(private$n_points)
        )
        log_lik <- c(dead_lik, sort(private$live$log_lik))
        log_vol <-  c(dead_int$log_volume, live_vol)
        compute_integral(log_lik, log_vol)
      }
    },

    #' @description
    #' Calculate the evidence integral of the nested sampling run.
    summary = function()
      new_es_summary(self, private),

    #' @description
    #' Print a brief summary of the sampler to the string.
    #'
    #' @param ... Arguments forwarded to [`format()`]
    #'
    #' @return itself, invisibly.
    print = function(...) {
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
          "Ln. Volume" = "{prettyNum(calc$log_volume)}",
          "Ln. Evidence" = "{prettyNum(calc$log_evidence)}"
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
    #' @field n_iterations The total number of sampling iterations.
    niterations = function() {
      private$n_iter
    },

    #' @field n_calls The total calls made to the likelihood function,
    #' or `0L` if no calls have been made yet.
    ncalls = function() {
      private$n_call
    },

    #' @field prior The `ernest_prior` object associated with the sampler.
    variables = function(value) {
      if (missing(value)) {
        return(variables(private$prior))
      }
      variables(private$prior) <- value
    }
  )
)
