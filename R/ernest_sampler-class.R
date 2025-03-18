#' The Ernest Sampler Object
#'
#' Objects of class `ernest_sampler` are responsible for managing the objects
#' involved in nested sampling.
#' * Create a nested sampler with (compile)
#' * Generate nested samples with (generate)
#' * Calculate an estimated evidence integral with (calculate)
#'
#' @importFrom R6 R6Class
ernest_sampler <- R6Class(
  "ernest_sampler",
  public = list(
    #' @field verbose Whether to produce output during a run.
    verbose = NULL,

    #' @description
    #' Initialize an ernest sampler with an lrps, number of points, and
    #' verbosity
    #' @param lrps The likelihood-restricted prior sampler to use
    #' @param ptype Either a single integer, a vector of variable names, or a
    #' zero-row [tibble::tibble()] describing the dimensions of the prior space.
    #' @param n_points The number of live points to use
    #' @param verbose Whether to produce verbose output
    initialize = function(lrps, ptype, n_points, verbose = getOption("verbose")) {
      if (!rlang::inherits_any(lrps, "ernest_lrps")) {
        stop_input_type(lrps, "an object of class `ernest_lrps`")
      }
      check_number_whole(n_points, min = 1)
      if (n_points < 2 * lrps$n_dim) {
        cli::cli_warn(
          "The number of live points ({n_points}) is less than twice the number of dimensions ({lrps$n_dim})."
        )
      }
      check_bool(verbose)
      private$.lrps <- lrps
      private$.ptype <- ptype
      private$.n_points <- n_points
      self$verbose <- verbose
    },

    #' @description
    #' Create a starting set of live points
    #' @param refresh If `TRUE`, and if the `ernest_sampler` already contains
    #' a list of live points, the existing live points will be overwritten.
    #' @returns A copy of `self`, invisibly.
    compile = function(refresh = FALSE) {
      check_bool(refresh)
      new_live <- .compile_sampler(
        private$.live,
        private$.lrps,
        private$.n_points,
        refresh
      )
      if (is_empty(new_live) && !refresh) {
        cli::cli_alert_info("{.pkg ernest} will continue an existing run.")
      } else if (!is_empty(private$.live) && !is_empty(new_live)) {
        cli::cli_alert_info("{.pkg ernest} will overwrite an existing run.")
      }
      private$.live <- new_live %||% private$.live
      invisible(self)
    },

    #' @description
    #' Generate nested samples.
    #' @param max_iterations The maximum number of iterations to run.
    #' @param max_calls The maximum number of calls to the likelihood function.
    #' @param min_logz The minimum log-evidence to reach before stopping.
    #' @param refresh If `TRUE`, and if the `ernest_sampler` already contains
    #' a list of live points, the existing live points will be overwritten.
    #' @returns A single-row tibble reporting the state of the sample.
    generate = function(max_iterations = Inf, max_calls = Inf, min_logz = 0.05, refresh = TRUE) {
      max_it <- if (max_iterations == Inf) {
        .Machine$integer.max
      } else {
        max_iterations
      }
      max_c <- if (max_calls == Inf) .Machine$integer.max else max_calls
      check_number_whole(max_it, min = 1, allow_infinite = FALSE)
      check_number_whole(max_c, min = 1, allow_infinite = FALSE)
      check_number_decimal(min_logz, min = 0, allow_infinite = FALSE)
      check_bool(refresh)
      if (self$n_iterations == 0) {
        self$compile()
      } else {
        self$compile(refresh)
      }
      private$nested_sampling_impl(max_it, max_c, min_logz)
      invisible(self)
    },

    #' @description
    #' Calculate the evidence integral.
    calculate = function() {
      if (self$n_iterations == 0) {
        cli::cli_alert_info("No iterations performed yet.")
      }
      dead_lik <- list_c(private$.dead$log_lik)
      dead_vol <- list_c(private$.log_vol)
      last_vol <- tail(dead_vol, 1)
      live_vol <- log1p(
        (-1 - private$.n_points)^(-1) * seq_len(private$.n_points)
      ) + last_vol

      log_lik <- c(dead_lik, sort(private$.live$log_lik))
      log_vol <-  c(dead_vol, live_vol)

      if (is.unsorted(log_lik)) {
        cli::cli_warn("`log_lik` should be a vector in ascending order; cannot estimate integral.")
        return(tibble::tibble("log_lik" = log_lik, "log_vol" = log_vol))
      } else if (is.unsorted(rev(log_vol), strictly = TRUE)) {
        cli::cli_abort("`log_vol` should be a vector in strictly ascending order; cannot estimate integral.")
        return(tibble::tibble("log_lik" = log_lik, "log_vol" = log_vol))
      }

      compute_integral("log_lik" = log_lik, "log_vol" = log_vol)
    },

    #' @description
    #' Glance at an ernest_sampler object.
    glance = function() {
      integral_summary <- if (self$n_iterations > 0) {
        calc <- self$calculate()
        tibble::tibble_row(
          "log_z" = tail(calc$log_z, 1),
          "log_z_err" = sqrt(tail(calc$log_z_var, 1)),
          "information" = tail(calc$information, 1),
        )
      } else {
        NULL
      }
      vctrs::vec_cbind(
        tibble::tibble_row(
          "n_dim" = private$.lrps$n_dim,
          "n_points" = private$.n_points,
          "n_iterations" = self$n_iterations,
          "n_calls" = self$n_calls,
          "eff" = if (.data$n_calls == 0) NULL else .data$n_iterations/.data$n_calls,
        ),
        integral_summary
      )
    },

    #' @description
    #' Format an ernest_sampler object
    #' @param digits The number of digits to display
    format = function(digits = getOption("digits")) {
      cli::cli_format_method({
        cli::cli_h1("Nested Sampling Run from {.pkg ernest}")
        format(private$.lrps, digits = digits)
        cli::cli_dl(c(
          "No. Live Points" = "{.val {private$.n_points}}",
          "No. Iterations" = "{.val {self$n_iterations}}"
        ))
        if (self$n_iterations > 0) {
          cli::cli_dl(c(
            "No. Calls" = "{.val {self$n_calls}}",
            "Log Evidence" = "{.val {tail(self$calculate()$log_z, 1)}}"
          ))
        } else {
          cli::cli_alert_info("No samples generated yet.")
        }
      })
    },

    #' @description
    #' Print an ernest_sampler object
    #' @param ... Arguments forwarded to [`format()`]
    print = function(...) {
      cat(self$format(...), sep = "\n")
    }
  ),
  private = list(
    .lrps = NULL,
    .ptype = NULL,
    .n_points = NULL,
    .live = list(),
    .dead = list(),
    .progress = list(),

    .calls = list(),
    .log_vol = list(),
    .last_log_z = -1e300,

    .worst_idx = NULL,
    .since_update = 0,

    # Nested Sampling Implementation
    nested_sampling_impl = function(max_it, max_c, min_logz) {
      iter <- self$n_iterations
      calls <- self$n_calls
      log_z <- private$.last_log_z
      log_vol <- unlist(tail(private$.log_vol, 1)) %||% 0
      d_log_vol <- log((private$.n_points + 1) / private$.n_points)
      worst_log_lik <- unlist(tail(private$.dead$log_lik, 1)) %||% -1e300
      num_updates <- 0

      saved_units <- list()
      saved_points <- list()
      saved_log_lik <- list()
      saved_progress <- list()

      if (self$verbose) {
        cli::cli_progress_bar(
          type = "custom",
          format = "{cli::pb_spin} Sampling | ln(z): {pretty_signif(log_z)}"
        )
      }

      status <- NULL
      for (iter in seq2(iter + 1, max_it)) {
        # Check kill criteria
        if (calls > max_c) {
          if (self$verbose) cli::cli_progress_done()
          status <- "`max_calls` reached"
          break
        }
        d_log_z <- logaddexp(
          0,
          max(private$.live$log_lik) + log_vol - log_z
        )
        if (d_log_z < min_logz) {
          if (self$verbose) cli::cli_progress_done()
          status <- "`min_logz` reached"
          break
        }
        if (self$verbose) cli::cli_progress_update()

        # Find and remove the worst point in the live sampler
        worst_idx <- which.min(private$.live$log_lik)
        saved_units[[iter]] <- private$.live$units[worst_idx, ]
        saved_points[[iter]] <- private$.live$points[worst_idx, ]
        saved_log_lik[[iter]] <- private$.live$log_lik[worst_idx]
        new_worst_lik <- private$.live$log_lik[worst_idx]

        # Constrict the prior volume and push an update to the integral
        log_vol <- log_vol - d_log_vol
        log_d_vol <- log(0.5 * expm1(d_log_vol)) + log_vol
        log_wt <- logaddexp(new_worst_lik, worst_log_lik) + log_d_vol
        log_z <- logaddexp(log_z, log_wt)
        private$.log_vol[[iter]] <- log_vol
        worst_log_lik <- new_worst_lik

        # Update the LRPS
        if ((private$.lrps$update_interval != 0) &&
            (private$.since_update >= private$.lrps$update_interval)) {
          private$.lrps <- update_sampler(private$.lrps)
          private$.since_update <- 0
          num_updates <- num_updates + 1
        }

        # Create a new point and push it to the live set
        copy <- sample.int(private$.n_points, size = 1)
        while (copy == worst_idx) {
          copy <- sample.int(private$.n_points, size = 1)
        }
        new <- propose_live(
          private$.lrps,
          private$.live$unit[copy, ],
          worst_log_lik
        )
        if (is_empty(new$unit)) {
          cli::cli_abort(
            "Region-based sampler couldn't improve the worst point."
          )
        }
        private$.live$units[worst_idx, ] <- new$unit
        private$.live$points[worst_idx, ] <- new$parameter
        private$.live$log_lik[worst_idx] <- new$log_lik

        saved_progress[[iter]] <- list(
          ".calls" = new$num_calls,
          ".id" = worst_idx,
          "sampler" = num_updates
        )

        calls <- calls + new$num_calls
        private$.since_update <- private$.since_update + new$num_calls
      }
      if (self$verbose) cli::cli_progress_done()
      private$.dead <- push_dead_points(
        private$.dead,
        saved_units,
        saved_points,
        saved_log_lik
      )
      private$.progress <- push_progress(
        private$.progress,
        saved_progress
      )
      private$.last_log_z <- log_z
      status <- status %||% "`max_iterations` reached"
    }
  ),
  active = list(
    #' @field n_iterations The total number of sampling iterations.
    n_iterations = function() {
      length(private$.dead$log_lik) %||% 0L
    },

    #' @field n_calls The total calls made to the likelihood function,
    #' or `0L` if no calls have been made yet.
    n_calls = function() {
      sum(private$.progress$.calls) %||% 0L
    },

    #' @field variables The names of each variable in the prior space.
    variables = function(value) {
      if (is_missing(value)) {
        names(private$.ptype)
      } else {
        ptype_value <- value
        if (length(private$.ptype) != length(ptype_value)) {
          cli::cli_abort("The number of variables must match the original number of dimensions.")
        }
        private$.ptype <- ptype_value
        names(private$.ptype)
      }
    },

    #' @field live_points Access the live points list.
    live_points = function() {
      private$.live
    },

    #' @field dead_points Access the dead points list.
    dead_points = function() {
      private$.dead
    }
  )
)
