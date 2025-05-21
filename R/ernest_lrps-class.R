#' Likelihood-Restricted Prior Samplers for Ernest
#'
#' @description
#' The `ernest_lrps` class provides a framework for likelihood-restricted prior
#' sampling (LRPS), which generates points within the parameter space defined by
#' the prior while satisfying a likelihood constraint. This is a key component
#' of nested sampling algorithms.
#'
#' @details
#' The base class `ernest_lrps` defines the structure and common methods for
#' LRPS. Subclasses implement specific sampling strategies, such as uniform
#' sampling or random walks with adaptive step sizes.
#'
#' @param log_lik A function that computes the log-likelihood of a point in the
#' parameter space.
#' @param prior_transform A function that maps a unit cube to the prior space.
#' @param n_dim The number of dimensions in the parameter space.
#'
#' @rdname ernest_lrps
#' @section Internal
#' @noRd
NULL

#' @rdname ernest_lrps
#' @noRd
ernest_lrps <- R6Class(
  "ernest_lrps",
  public = list(
    initialize = function(log_lik_fn, prior_fn, n_dim) {
      check_function(log_lik_fn)
      check_function(prior_fn)
      check_number_whole(n_dim, min = 1, allow_null = FALSE)
      private$log_lik_fn <- log_lik_fn
      private$prior_fn <- prior_fn
      private$n_dim <- n_dim
    },
    clear = function() {
      private$n_call <- 0L
      private$n_iter <- 0L
      private$hist_iter <- list()
      private$hist_call <- list()
      invisible(self)
    },
    update = function() {
      list_length <- length(private$hist_iter)
      private$hist_iter[[list_length + 1]] <- private$n_iter
      private$hist_call[[list_length + 1]] <- private$n_call
      private$n_iter <- 0L
      private$n_call <- 0L
      invisible(self)
    },
    propose_uniform = function(criterion = -Inf) {
      new <- lrps_uniform(
        log_lik_f = private$log_lik_fn,
        prior_transform = private$prior_fn,
        num_dim = private$n_dim,
        criterion = criterion,
        maxtry = getOption("ernest.max_loop", default = 1e6)
      )
      private$increment_count(new$num_calls)
      new
    },
    propose_live = function(original, criterion) {
      cli::cli_abort(
        "{.fn propose_live} must be called on a subclass of
        {.cls propose_live}."
      )
    },
    print = function(...) {
      cli::cli_text("Likelihood Restricted Prior Sampler")
      invisible(self)
    }
  ),
  private = list(
    log_lik_fn = NULL,
    prior_fn = NULL,
    n_dim = NULL,
    n_call = 0L,
    n_iter = 0L,
    hist_iter = list(),
    hist_call = list(),
    increment_count = function(n_call) {
      private$n_iter <- private$n_iter + 1L
      private$n_call <- private$n_call + n_call
    }
  ),
  active = list(
    since_update = function() {
      private$n_call
    },
    history = function() {
      tibble::tibble(
        "n_iter" = cumsum(list_c(private$hist_iter)) %||% integer(0L),
        "n_call" = cumsum(list_c(private$hist_call)) %||% integer(0L)
      )
    },
    summary = function() {
      utils::tail(self$history, 1L)
    }
  )
)

#' Uniform Sampling in Unit Cube
#'
#' @description
#' The `uniform_lrps` subclass performs uniform sampling within the unit cube
#' while satisfying a likelihood constraint.
#'
#' @details
#' This subclass overrides the `propose_live` method to generate points
#' uniformly within the unit cube.
#'
#' @rdname ErnestSampler
#' @noRd
NULL

#' Uniform Sampling in Unit Cube Subclass
#' @rdname ErnestSampler
#' @noRd
uniform_lrps <- R6Class(
  "uniform_lrps",
  inherit = ernest_lrps,
  public = list(
    propose_live = function(original, criterion) {
      self$propose_uniform(criterion)
    },
    print = function(...) {
      cli::cli_text("Uniform Cube Random Sampling")
      invisible(self)
    }
  )
)

#' Random Walk within the Unit Cube
#'
#' @description
#' The `rwcube_lrps` subclass performs a random walk within the unit cube, with
#' step sizes that adapt based on the acceptance ratio.
#'
#' @details
#' This subclass introduces additional parameters for controlling the random
#' walk:
#' - `steps`: The number of steps to take in the random walk.
#' - `epsilon`: The initial step size, which is adjusted dynamically to target
#'   an acceptance ratio of 0.5.
#'
#' @param steps The minimum number of steps to take in the random walk.
#' @param epsilon The initial step size of the random walk.
#' @noRd
rwcube_lrps <- R6::R6Class(
  "rwcube_lrps",
  inherit = ernest_lrps,
  public = list(
    initialize = function(
      log_lik_fn,
      prior_fn,
      n_dim,
      steps = 20L,
      epsilon = 1
    ) {
      super$initialize(log_lik_fn, prior_fn, n_dim)
      check_number_whole(steps, min = 2, allow_null = FALSE)
      check_number_decimal(epsilon, min = 0, allow_null = FALSE)
      private$steps <- as.integer(steps)
      private$cur_epsilon <- as.double(epsilon)
    },
    clear = function() {
      private$cur_epsilon <- unlist(private$hist_epsilon[1]) %||%
        private$cur_epsilon
      private$n_accept <- 0L
      private$hist_epsilon <- list()
      private$hist_acc_ratio <- list()
      super$clear()
    },
    propose_live = function(original, criterion) {
      new <- lrps_rwcube(
        log_lik = private$log_lik_fn,
        prior_transform = private$prior_fn,
        original = original,
        criterion = criterion,
        steps = private$steps,
        epsilon = private$cur_epsilon
      )
      private$increment_count(new$num_calls)
      private$n_accept <- private$n_accept + new$num_acc
      new
    },
    update = function() {
      list_length <- length(private$hist_iter)
      acc_ratio <- private$n_accept / private$n_call
      private$hist_epsilon[[list_length + 1]] <- private$cur_epsilon
      private$hist_acc_ratio[[list_length + 1]] <- acc_ratio
      # Newton-Like Update to Target 0.5 Acceptance Ratio
      private$cur_epsilon <- private$cur_epsilon *
        exp((acc_ratio - 0.5) / private$n_dim / 0.5)
      private$n_accept <- 0L
      super$update()
    },
    print = function(...) {
      cli::cli_text("Random Walk in Unit Cube")
      cli::cli_dl(c(
        "Current Config." =
          "{private$steps} steps @ {prettyNum(private$cur_epsilon)} step-size",
        "Last Update" = "{private$n_call} call{?s} ago."
      ))
      invisible(self)
    }
  ),
  private = list(
    steps = NULL,
    cur_epsilon = NULL,
    n_accept = 0L,
    hist_epsilon = list(),
    hist_acc_ratio = list()
  ),
  active = list(
    history = function() {
      tibble::tibble(
        !!!super$history,
        "epsilon" = list_c(private$hist_epsilon) %||% numeric(0L),
        "acc_ratio" = list_c(private$hist_acc_ratio) %||% numeric(0L),
      )
    },
    epsilon = function() {
      private$cur_epsilon
    }
  )
)
