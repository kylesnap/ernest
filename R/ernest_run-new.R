#' An R6 Class Containing a Nested Sampling Run
#'
#' @description
#' When constructed with `nested_sampling()`, ernest creates an `ErnestRun` object. This
#' contains the variables necessary for running nested sampling runs, and exposes
#' methods for viewing information about the runs results.
#'
#' @details
#' TBD
ErnestRun <- R6::R6Class("ErnestRun",
  public = list(
    #' @description
    #' Create a new `ErnestRun` object
    #'
    #' @param sampler The `ernest::sampler` object to use for the run.
    #' @param num_points Number of live points to use while fitting.
    #' @param verbose Whether to print status messages while sampling.
    initialize = function(sampler, num_points, n_uniform, update_int, verbose) {
      if (!inherits(sampler, Sampler)) {
        cli::cli_abort("`sampler` must be a `Sampler` object.")
      }
      check_number_whole(num_points, min = 1)
      check_number_whole(n_uniform, min = 0)
      check_number_whole(update_int, min = 0)
      check_bool(verbose)

      private$.sampler <- sampler
      private$.num_points <- num_points
      private$.live_units <- matrix(
        stats::runif(private$.sampler@num_dim * private$.num_points),
        nrow = private$.num_points,
        ncol = private$.sampler@num_dim
      )
      private$.live_points <- matrix(nrow = private$.num_points, ncol = private$.sampler@num_dim)
      for (i in 1:private$.num_points) {
        private$.live_points[i, ] <- private$.sampler@prior_transform(private$.live_units[i, ])
        private$.live_lik[i] <- private$.sampler@log_lik(private$.live_points[i, ])
      }
      private$.d_log_vol <- log((private$.num_points + 1.) / private$.num_points)
      private$.n_uniform <- n_uniform
      private$.update_int <- update_int
      private$.verbose <- verbose
    },

    #' @description
    #' Print the run.
    #'
    #' @param digits The number of digits to print in the summary.
    print = function(digits = max(3, getOption("digits") - 3)) {
      glance <- self$glance()
      cli::cli_div(theme = list(.val = list(digits = digits)))
      cli::cli_h3("Nested Sampling Run with {.pkg ernest}")
      cli::cli_dl()
      cli::cli_li(c(
        "Live Points" = "{glance$n_points}",
        "Num. Parameters" = "{glance$n_par}",
        "Iterations" = "{glance$iterations}",
        "Calls" = "{glance$calls}"
      ))
      if ("time" %in% names(glance)) {
        cli::cli_li(c(
          "Time" = "{prettyunits::pretty_dt(glance$time)}",
          "Log Evidence" = "{.val {glance$log_z}} \U00B1 {.val {sqrt(glance$log_z_err)}}"
        ))
      }
      cli::cli_end()
      invisible(self)
    },

    #' @description
    #' Calculate the evidence integral.
    #'
    #' @param add_live Whether to include the live points in the calculation.
    calculate = function(add_live = TRUE) {
      if (private$.totit == 0) {
        cli::cli_warn("There are no samples within this run yet. Have you run `run()?`")
        return(NULL)
      }

      dead_lik <- list_c(private$.s_log_lik)
      dead_vol <- list_c(private$.s_log_vol)

      live_lik <- if (add_live) {
        sort(private$.live_lik)
      } else {
        NULL
      }

      live_vol <- if (add_live) {
        log1p((-1 - private$.num_points)^(-1) * seq_len(private$.num_points)) +
          private$.log_vol
      } else {
        NULL
      }

      log_lik <- c(dead_lik, live_lik)
      log_vol <- c(dead_vol, live_vol)
      compute_integral(log_lik, log_vol)
    },

    #' @description
    #' Extract the posterior weights of each sampled point.
    #'
    #' @param add_live Whether to include the live points in the calculation.
    weights = function(add_live = TRUE, exponentiate = FALSE) {
      weights <- self$calculate(add_live = add_live)$log_weight
      weights <- if (exponentiate) {
        exp(weights - sum(weights))
      } else {
        weights - sum(weights)
      }
    },

    #' @description
    #' Present the points as a draws object.
    #'
    #' @param resample_draws Whether to run `posterior::resample_draws` on the
    #' draws object, producing a sample from a resampled posterior.
    #' @param ... Additional arguments to pass to `posterior::resample_draws`
    #'
    #' @details When `resample_draws` is false, the draws object will be bound to
    #' the log importance weights from the nested sampling run.
    #'
    #' @returns An object of class `"draws_matrix"`.
    #'
    #' @export
    as_draws_matrix = function(add_live = TRUE, resample_draws = TRUE, unit_scale = FALSE, ...) {
      dead <- if (private$.totit != 0L) {
        if (unit_scale) do.call(rbind, private$.units) else do.call(rbind, private$.points)
      } else {
        NULL
      }

      live <- if (add_live) {
        lik_ord <- order(private$.live_lik)
        if (unit_scale) private$.live_units[lik_ord, ] else private$.live_points[lik_ord, ]
      } else {
        NULL
      }

      draws <- posterior::as_draws_matrix(rbind(dead, live))
      draws <- posterior::weight_draws(
        draws,
        self$weights(add_live = add_live, exponentiate = FALSE),
        log = TRUE
      )
      if (resample_draws) {
        posterior::resample_draws(draws, ...)
      } else {
        draws
      }
    },

    #' @description
    #' Report information about the run in a one-row tibble
    #' @param add_live Whether to include the live points in the calculation.
    glance = function(add_live = TRUE) {
      empty <- private$.totit == 0L

      integral <- if (empty) NULL else self$calculate()[nrow(self$calculate()),]
      tibble::tibble_row(
        "n_points" = private$.num_points,
        "n_par" = private$.sampler@num_dim,
        "iterations" = private$.totit,
        "calls" = private$.total_calls,
        "time" = if (empty) NULL else private$.time,
        "n_updates" = if (empty) NULL else private$.sampler_updates,
        "log_z" = if (empty) NULL else integral$log_z,
        "log_z_err" = if (empty) NULL else sqrt(integral$log_z_var),
        "information" = if (empty) NULL else integral$information
      )
    },

    #' @description
    #' Run the nested sampler to generate new samples.
    #'
    #' @param maxit Maximum number of iterations to run the sampler. Must
    #' represent a positive integer.
    #' @param maxcall Maximum number of calls to the likelihood function. Must
    #' represent a positive integer.
    #' @param dlogz The threshold for the remaining prior volume to the total
    #' evidence. Must represent a number larger or equal to zero.
    generate = function(maxit = Inf, maxcall = Inf, dlogz = 0.05) {
      if (maxit == Inf) {
        maxit <- .Machine$integer.max
      }
      if (maxcall == Inf) {
        maxcall <- .Machine$integer.max
      }
      check_number_whole(maxit, min = 1, allow_infinite = FALSE)
      check_number_whole(maxcall, min = 1, allow_infinite = FALSE)
      check_number_decimal(dlogz, min = 0, allow_infinite = FALSE)

      # --- Cleanup activities here --- #
      if (private$.totit != 0L) {
        cli::cli_warn("ErnestRun has already been run. Re-running is not supported yet.")
      }

      iter <- 0L
      time1 <- Sys.time()
      for (iter in c(1:maxit)) {
        if (private$.total_calls > maxcall) {
          # if (control$verbose) cli::cli_progress_done()
          break
        }
        d_log_z <- logaddexp(0, max(private$.live_lik) + private$.log_vol - private$.log_z)
        if (d_log_z < dlogz) {
          # if (control$verbose) cli::cli_progress_done()
          break
        }
        private$.pop()
        private$.push()
      }
      private$.totit <- private$.totit + iter
      private$.time <-  private$.time + difftime(Sys.time(), time1)
      invisible(self)
    }
  ),

  active = list(
    #' @field verbose Whether to print status messages while sampling.
    verbose = function(value = NULL) {
      if (!is.null(value)) {
        check_bool(value)
        private$.verbose <- value
      }
      private$.verbose
    }
  ),

  private = list(
    .live_units = NULL,
    .live_points = NULL,
    .live_lik = NULL,

    .sampler = NULL,
    .num_points = NULL,
    .total_calls = 0L,
    .totit = 0L,
    .worst_idx = NULL,
    .worst_lik = -1.e300,
    .d_log_vol = NULL,
    .log_vol = 0,
    .log_z = -1.e300,
    .time = difftime(0, 0),
    .sampler_updates = 0L,
    .n_uniform = NULL,
    .update_int = NULL,
    .n_since_update = NA,

    .verbose = getOption("verbose", default = FALSE),

    .units = list(),
    .points = list(),
    .s_log_lik  = list(),
    .s_log_vol = list(),
    .idx = list(),
    .calls = list(),
    .parent = list()
  )
)
