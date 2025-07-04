#' Likelihood-Restricted Prior Samplers (Internal)
#'
#' Internal R6 class for likelihood-restricted prior sampling (LRPS).
#' Provides a base for subclasses implementing specific LRPS strategies.
#'
#' @param log_lik_fn Function computing log-likelihood at a point.
#' @param prior_fn Function mapping unit cube to prior space.
#' @param n_dim Number of dimensions.
#' @keywords internal
#' @noRd
ernest_lrps <- R6Class(
  "ernest_lrps",
  class = FALSE,
  portable = FALSE,
  lock_class = TRUE,
  public = list(
    initialize = function(log_lik_fn, prior_fn, n_dim) {
      check_function(log_lik_fn)
      check_function(prior_fn)
      check_number_whole(n_dim, min = 1, allow_null = FALSE)
      private$log_lik_fn <- log_lik_fn
      private$prior_fn <- prior_fn
      private$n_dim <- n_dim
      private$unit_log_lik <- function(unit) {
        point <- try_fetch(private$prior_fn(unit), error = function(cnd) {
          cli::cli_abort(c(
            "Can't calculate the prior transformation.",
            "x" = cnd_message(cnd)
          ), parent = NA)
        })
        try_fetch(
          private$log_lik_fn(point),
          error = function(cnd) {
            cli::cli_abort(c(
              "Can't calculate the log. likelihood.",
              "x" = cnd_message(cnd)
            ), parent = NA)
          }
        )
      }
    },

    clear = function() {
      private$n_call <- 0L
      private$n_iter <- 0L
      private$hist_length <- 0L
      private$hist_iter <- vctrs::list_of(.ptype = integer())
      private$hist_call <- vctrs::list_of(.ptype = integer())
      invisible(self)
    },

    update = function(...) {
      private$n_call <- 0L
      return(self)
    },

    find_point = function(unit) {
      point <- private$prior_fn(unit)
      log_lik <- private$log_lik_fn(point)
      list("unit" = drop(unit), "point" = point, "log_lik" = log_lik)
    },

    propose_uniform = function(criteria) {
      res <- UniformCube(
        criteria,
        private$unit_log_lik,
        private$n_dim,
        private$max_loop
      )
      res
    },

    as_string = function() {
      cli::cli_fmt({
        cli::cli_text("Abstract LRPS Sampler")
      })
    }
  ),
  private = list(
    # Parameters
    log_lik_fn = NULL,
    prior_fn = NULL,
    n_dim = NULL,
    unit_log_lik = NULL,
    max_loop = max(
      as.integer(getOption("ernest.max_loop", 1e6L)),
      2L
    ),

    # Counters
    n_call = 0L,
    n_iter = 0L,
    hist_length = 0L,

    # History
    hist_iter = vctrs::list_of(.ptype = integer()),
    hist_call = vctrs::list_of(.ptype = integer()),

    # Helpers
    increment = function(res) {
      private$n_call <- private$n_call + res$n_call
      private$n_iter <- private$n_iter + vctrs::vec_size(res$log_lik)
      private$hist_length <- private$hist_length + 1L
      private$hist_iter[[private$hist_length]] <- private$n_iter
      private$hist_call[[private$hist_length]] <- private$n_call
      invisible(self)
    }
  ),
  active = list(
    history = function() {
      vctrs::df_list(
        "n_iter" = list_c(private$hist_iter) %||% integer(0L),
        "n_call" = list_c(private$hist_call) %||% integer(0L)
      )
    },

    since_update = function() {
      private$n_call
    }
  )
)

#' Uniform LRPS Sampler (Internal)
#'
#' Internal R6 subclass for uniform sampling in the unit cube under a likelihood constraint.
#'
#' @keywords internal
#' @noRd
uniform_lrps <- R6Class(
  "uniform_lrps",
  inherit = ernest_lrps,
  class = FALSE,
  portable = FALSE,
  lock_class = TRUE,
  public = list(
    initialize = function(log_lik_fn, prior_fn, n_dim) {
      super$initialize(log_lik_fn, prior_fn, n_dim)
    },

    clear = function() {
      super$clear()
    },

    update = function(...) {
      super$update(...)
    },

    propose_live = function(original, criteria) {
      res <- self$propose_uniform(criteria)
      super$increment(res)
      res
    },

    as_string = function() {
      cli::cli_fmt({
        cli::cli_text("Uniform Sampling in Unit Cube")
        cli::cli_dl(c(
          "No. Iter" = "{private$n_iter}",
          "No. Call" = "{private$n_call}"
        ))
      })
    }
  )
)

#' Random Walk LRPS Sampler (Internal)
#'
#' Internal R6 subclass for random walk sampling in the unit cube with adaptive step size.
#'
#' @param steps Number of random walk steps.
#' @param epsilon Initial step size.
#' @keywords internal
#' @noRd
rwcube_lrps <- R6::R6Class(
  "rwcube_lrps",
  inherit = ernest_lrps,
  class = FALSE,
  portable = FALSE,
  lock_class = TRUE,
  public = list(
    initialize = function(
      log_lik_fn,
      prior_fn,
      n_dim,
      steps = 25L,
      target_acceptance = 0.5
    ) {
      super$initialize(log_lik_fn, prior_fn, n_dim)
      check_number_whole(steps, min = 2)
      check_number_decimal(target_acceptance, max = 1)
      if (target_acceptance < 1 / steps) {
        cli::cli_abort("Target acceptance must be at least 1/{steps}.")
      }
      private$steps <- as.integer(steps)
      private$target_acceptance <- as.double(target_acceptance)
    },

    clear = function() {
      private$cur_epsilon <- 1.0
      private$n_accept <- 0L
      private$hist_accept <- vctrs::list_of(.ptype = integer())
      private$hist_epsilon <- vctrs::list_of(.ptype = double())
      super$clear()
    },

    update = function() {
      acc_ratio <- private$n_accept / private$n_call
      # Newton-Like Update to Target 0.5 Acceptance Ratio
      private$cur_epsilon <- private$cur_epsilon *
        exp(
          (acc_ratio - private$target_acceptance) /
            private$n_dim /
            private$target_acceptance
        )
      private$n_accept <- 0L
      super$update()
    },

    propose_live = function(original, criteria) {
      if (!is.matrix(original)) {
        dim(original) <- c(1, private$n_dim)
      }
      res <- RandomWalkMetropolis(
        original,
        criteria,
        private$unit_log_lik,
        private$n_dim,
        private$steps,
        private$cur_epsilon
      )
      if (any(is.na(res$log_lik))) {
        no_swaps <- which(is.na(res$log_lik))
        res$log_lik[no_swaps] <- apply(
          original[no_swaps, , drop = FALSE],
          1,
          private$unit_log_lik
        )
      }
      private$increment(res)
      res
    },

    as_string = function() {
      cli::cli_fmt({
        cli::cli_text("Random-Walk in Ubit Cube with Adaptive Step Size")
        cli::cli_dl(c(
          "No. Iter" = "{private$n_iter}",
          "No. Call" = "{private$n_call}",
          "No. Steps" = "{private$steps}",
          "Epsilon" = "{private$cur_epsilon}"
        ))
      })
    }
  ),
  private = list(
    # Parameters
    steps = NULL,
    target_acceptance = NULL,
    cur_epsilon = 1.0,

    # Counters
    n_accept = 0L,

    # History
    hist_accept = vctrs::list_of(.ptype = integer()),
    hist_epsilon = vctrs::list_of(.ptype = double()),

    # Helpers
    increment = function(res) {
      super$increment(res)
      private$n_accept <- private$n_accept + res$n_accept
      private$hist_epsilon[[private$hist_length]] <- private$cur_epsilon
      private$hist_accept[[private$hist_length]] <- private$n_accept
    }
  ),
  active = list(
    history = function() {
      vctrs::df_list(
        !!!super$history,
        "n_accept" = list_c(private$hist_accept) %||% integer(0L),
        "epsilon" = list_c(private$hist_epsilon) %||% double(0L)
      )
    },

    epsilon = function() {
      private$cur_epsilon
    },

    acceptance_ratio = function() {
      if (private$n_call == 0L) {
        return(NA_real_)
      }
      private$n_accept / private$n_call
    }
  )
)
