#' @include utils.R
NULL

#' A likelihood-restricted prior sampler for nested sampling
#'
#' Likelihood-restricted prior sampling (or LRPS) aims to generate points within
#' the parameter space defined by the prior. Many options exist for conducting
#' LRPS, though two currently exist in ernest: uniform sampling and a random
#' walk. In ernest, LRPS objects contain the necessary information to conduct
#' sampling, and is assigned to an  R enviroment where the actual nested
#' sampling algorithm is conducted.
#'
#' Users should not need to interface with these classes directly, and they are
#' not exported from ernest. Developers looking to extend ernest can build
#' samplers that conform to the template set by ErnestSampler, and set the appropriate
#' methods for the `propose_live` and `update_sampler` generics.
#'
#' @param log_lik A function that returns the log-likelihood of a point in the
#' parameter space.
#' @param prior_transform A function that transforms a unit cube to the prior
#' space.
#' @param n_dim The number of dimensions in the parameter space.
#' @param n_points The number of live points to use during the nested sampling
#' run.
#' @param first_update The number of likelihood calls that are performed using
#' the uniform sampling cube. Defaults to 300L.
#' @param between_update The number of iterations to run between updates to the
#' evidence integral. Defaults to 750L.
#' @param verbose Whether to print progress messages to the console. Defaults to
#' the value of `getOption("verbose")`.
#' @param wrk Either `NULL`, or a reference to the environment where the nested
#' sampling variables are held.
ErnestSampler <- new_class(
  "ErnestSampler",
  properties = list(
    log_lik = S7::class_function,
    prior_transform = class_function,
    n_dim = prop_natural(),
    n_points = prop_natural(default = 500L),
    first_update = prop_natural(include_zero = TRUE, default = 300L),
    between_update = prop_natural(include_zero = TRUE, default = 750L),
    n_iter = new_property(
      getter = function(self) {
        if (!is.null(self@wrk)) {
          self@wrk$n_iter
        } else {
          0L
        }
      }
    ),
    n_call = new_property(
      getter = function(self) {
        if (!is.null(self@wrk)) {
          self@wrk$n_call
        } else {
          0L
        }
      }
    ),
    verbose = new_property(
      class_logical,
      default = getOption("verbose")
    ),
    wrk = NULL | class_environment
  ),
  abstract = TRUE
)

#' Format method for ErnestSampler
#' @noRd
format.ErnestSampler <- new_external_generic("base", "format", "x")
method(format, ErnestSampler) <- function(x, digits = getOption("digits"), ...) {
  cli::cli_format_method({
    cli::cli_h1("Nested Sampling Run from {.pkg ernest}")
    cli::cli_dl(c(
      "No. Live Points" = "{x@n_points}",
      "No. Dimensions" = "{x@n_dim}"
    ))
    if (!is.null(x@wrk)) {
      cli::cli_h3("Results")
      res <- calculate(x, progress = FALSE)
      log_evid <- prettyunits::pretty_signif(
        res$log_evidence,
        digits = digits
      )
      log_evid_err <- prettyunits::pretty_signif(
        res$log_evidence_err,
        digits = digits
      )
      eff <- prettyunits::pretty_signif(
        100 * (x@n_iter / x@n_call),
        digits = digits
      )
      cli::cli_dl(c(
        "No. Iterations" = "{x@n_iter}",
        "No. Calls" = "{x@n_call}",
        "Efficiency" = "{eff}%",
        "Log. Evidence" = "{log_evid} \u00B1 {log_evid_err}"
      ))
    } else {
      cli::cli_alert_info("Sampler has not been run.")
    }
  })
}

#' Print Method for ErnestSampler
#' @noRd
method(print, ErnestSampler) <- function(x, digits = getOption("digits"), ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' Uniform Sampling in Unit Cube
#' @rdname ErnestSampler
UniformCube <- S7::new_class(
  "UniformCube",
  parent = ErnestSampler
)

#' Random Walk within the Unit Cube
#' @rdname ErnestSampler
#' @param steps The minimum number of steps to take in the random walk.
#' @param epsilon The initial step size of the random walk.
RandomWalkCube <- S7::new_class(
  "RandomWalkCube",
  properties = list(
    steps = prop_natural(include_zero = FALSE, default = 20L),
    epsilon = S7::new_property(
      S7::class_numeric,
      default = 0.1
    )
  ),
  parent = ErnestSampler
)
