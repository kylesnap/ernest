#' Ernest Nested Sampler
#'
#' @description
#' An R6 class that contains a nested sampling run.
#'
#' This object is normally created by calling [nested_sampling()], and
#' interacted with by calling S3 methods like [generate()], [calculate()],
#' and [glance()].
#' @importFrom R6 R6Class
ernest_sampler <- R6Class(
  "ernest_sampler",
  public = list(
    #' @field verbose
    #' Should a progress bar be displayed during sampling?
    verbose = NULL,

    #' @description
    #' Creates a new `ernest_sampler`.
    #'
    #' @param lrps An `ernest_lrps` object describing the model and prior space.
    #' @param ptype The parameters involved in nested sampling. Possible values
    #' are:
    #' * A single integer, describing the dimensions of the prior space.
    #' * A character vector, naming each dimension in the prior space.
    #' * A zero-row [tibble::tibble()], where each column names a dimension of
    #' the prior space.
    #' @param n_points The number of live points to use during nested sampling.
    #' Higher values allow for more accurate estimates of the evidence integral
    #' at the cost of increased computational time.
    #' @param verbose Whether to display a progress bar during a run.
    #'
    #' @returns An `ernest_sampler` object.
    initialize = function(lrps, ptype, n_points = 500L, verbose = getOption("verbose"))
      es_init(self, private, lrps, ptype, n_points, verbose),

    #' @description
    #' Prepare the `ernest_sampler` for generating nested samples by validating
    #' the set of live points within the object, creating live points if none
    #' exist yet.
    #'
    #' @param refresh Whether to clear existing points from `object` and generating new ones.
    #' If `TRUE`, the function will clear both the live points and dead points gathered
    #' from previous runs.
    #'
    #' @returns `object`, invisibly.
    #' @seealso [compile.ernest_sampler()]
    compile = function(refresh = FALSE)
      es_compile(self, private, refresh, call = caller_env()),


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
    #' @return `x`, invisibly.
    #' @seealso [generate.ernest_sampler()]
    generate = function(max_iterations = Inf, max_calls = Inf, min_logz = 0.05, refresh = FALSE)
      es_generate(self, private, max_iterations, max_calls, min_logz, refresh),

    #' @description
    #' Calculate the marginal likelihood of a given model and return the estimates
    #' in a tidy [tibble()].
    #'
    #' @param add_points A string, either `"none"`, `"unit"`, `"parameter"`, or `"both"`.
    #' If `"none"`, no additional columns are added. If `"unit"`or `"parameter"`,
    #' the parameter values associated with each point are added, in their respective
    #' units. If `"both"`, both the unit and parameter values are added.
    #' @param add_progress Adds columns for the number of calls to the likelihood
    #' function between each iteration.
    #'
    #' @returns A `tibble` with columns reporting the results of the run.
    #' @seealso [calculate.ernest_sampler()]
    calculate = function(add_points = c("none", "unit", "parameter", "both"),
                         add_progress = FALSE) {
      es_calculate(self, private, add_points, add_progress)
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

    .log_vol = list(),
    .last_log_z = -1e300,

    .worst_idx = NULL,
    .since_update = 0,

    # Clear run data from the sampler
    .clear = function() {
      private$.live <- list()
      private$.dead <- list()
      private$.progress <- list()

      private$.log_vol <- list()
      private$.last_log_z <- -1e300

      private$.worst_idx <- NULL
      invisible(self)
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

#' @noRd
es_init <- function(self, private, lrps, ptype, n_points, verbose) {
  if (!rlang::inherits_any(lrps, "ernest_lrps")) {
    stop_input_type(lrps, "an object of class `ernest_lrps`")
  }
  check_number_whole(n_points, min = 1)
  if (n_points < 2 * lrps$n_dim) {
    cli::cli_warn(
      "The number of live points ({n_points}) is less than twice the
      number of dimensions ({lrps$n_dim})."
    )
  }
  check_bool(verbose)
  private$.lrps <- lrps
  private$.ptype <- make_ptype(ptype)
  private$.n_points <- n_points
  self$verbose <- verbose
  invisible(self)
}

#' @noRd
es_compile <- function(self, private, refresh, call) {
  check_bool(refresh)
  if (refresh) {
    private$.clear()
  }
  if (is_empty(private$.live)) {
    private$.live <- create_live(private$.lrps, private$.n_points, call = call)
  }
  try_fetch(
    check_live(private$.live, private$.lrps, private$.n_points),
    error = \(cmd) {
      cli::cli_alert_danger(
        "An error was encounter when validating the live points. This likely means
        that {.pkg ernest} encountered an internal error. Please consider filing a
        bug report."
      )
      cli::cli_abort("Encountered a fatal error when validating sampler. Please recompile
      the sampler from scratch.", parent = cmd, call = call)
    }
  )
  invisible(self)
}

#' @noRd
es_generate <- function(self, private, max_iterations, max_calls, min_logz, refresh) {
  max_iterations <- if (max_iterations == Inf) {
    .Machine$integer.max
  } else as.integer(max_iterations)
  max_calls <- if (max_calls == Inf) {
    .Machine$integer.max
  } else as.integer(max_calls)
  min_logz <- as.double(min_logz)

  check_number_whole(max_iterations, min = 1)
  check_number_whole(max_calls, min = 1)
  check_number_decimal(min_logz, min = 0, allow_infinite = FALSE)
  self$compile(refresh)

  nested_sampling_impl(self, private, max_iterations, max_calls, min_logz)
  invisible(self)
}

#' Calculate the integral and other statistics for the sampler.
#'
#' This function calculates the integral of the log-likelihood and other
#' statistics for the sampler, including unit and parameter points, and
#' efficiency if requested.
#'
#' @param self The reference to the current object.
#' @param private The reference to the private fields of the current object.
#' @param add_points A character string indicating whether to add points to the
#' output. Possible values are "none", "unit", "parameter", and "both".
#' @param add_progress A logical value indicating whether to add efficiency
#' progress to the output.
#' @return A tibble containing the calculated integral and other statistics.
#' @noRd
es_calculate <- function(self, private, add_points, add_progress) {
  add_points <- arg_match0(add_points, values = c("none", "unit", "parameter", "both"))
  if (is_empty(private$.dead)) {
    rlang::inform("No iterations have been performed yet.")
    return(NULL)
  }

  dead_vol <- list_c(private$.log_vol)
  last_vol <- tail(dead_vol, 1)
  live_vol <- log1p(
    (-1 - private$.n_points)^(-1) * seq_len(private$.n_points)
  ) + last_vol
  log_lik <- c(private$.dead$log_lik, sort(private$.live$log_lik))
  log_vol <-  c(dead_vol, live_vol)

  # Check whether log_lik and log_vol are ordered correctly.
  improper_integral <- if(length(log_lik) != length(log_vol)) {
    cli::cli_warn("Length of `log_lik` and `log_vol` must match.")
    TRUE
  } else if (is.unsorted(log_lik)) {
    cli::cli_warn("`log_lik` must be a vector in ascending order;
                  cannot estimate integral.")
    TRUE
  } else if (is.unsorted(rev(log_vol), strictly = TRUE)) {
    cli::cli_warn("`log_vol` must be a vector in strictly descending order.")
    TRUE
  } else FALSE

  if (improper_integral) {
    return(tibble::tibble(".iter" = seq_len(length(log_lik)), "log_lik" = log_lik, "log_vol" = log_vol))
  }

  integral <- compute_integral("log_lik" = log_lik, "log_vol" = log_vol)
  unit <- if (add_points %in% c("unit", "both")) {
    tmp <- rbind(
      private$.dead$unit,
      private$.live$unit[order(private$.live$log_lik),]
    )
    colnames(tmp) <- paste0("unit_", names(private$.ptype))
    tibble::as_tibble(tmp)
  } else NULL
  points <- if (add_points %in% c("parameter", "both")) {
    tmp <- rbind(
      private$.dead$point,
      private$.live$point[order(private$.live$log_lik),]
    )
    colnames(tmp) <- names(private$.ptype)
    tibble::as_tibble(tmp)
  } else NULL
  efficiency <- if (add_progress) {
    tibble::tibble(
      ".calls" = c(private$.progress$.calls, rep(NA_real_, private$.n_points)),
      ".id" = c(private$.progress$.id, order(private$.live$log_lik)),
      ".sampler" = c(private$.progress$.sampler, rep(NA_integer_, private$.n_points)),
    )
  } else NULL
  tibble::tibble(
    ".iter" = seq_len(length(log_lik)),
    unit,
    points,
    integral,
    efficiency
  )
}
