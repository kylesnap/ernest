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
ErnestLRPS <- S7::new_class(
  "ErnestLRPS",
  properties = list(
    log_lik = S7::class_function,
    prior_transform = S7::class_function,
    n_dim = prop_natural(),
    n_points = prop_natural(default = 500L),
    first_update = prop_natural(include_zero = TRUE, default = 300L),
    between_update = prop_natural(include_zero = TRUE, default = 750L),
    n_iter = S7::new_property(
      getter = function(self) {
        if (!is.null(self@wrk)) {
          self@wrk$n_iter
        } else {
          0L
        }
      }
    ),
    n_call = S7::new_property(
      getter = function(self) {
        if (!is.null(self@wrk)) {
          self@wrk$n_call
        } else {
          0L
        }
      }
    ),
    verbose = S7::new_property(
      S7::class_logical,
      default = getOption("verbose")
    ),
    compiled = S7::new_property(
      getter = function(self) {
        !is.null(self@wrk)
      }
    ),
    wrk = NULL | S7::class_environment
  ),
  abstract = TRUE
)
S7::method(print, ErnestLRPS) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cat(format(x, digits = digits), sep = "\n")
    invisible(x)
  }
# S7::method(format, ErnestLRPS) <-
#   function(x, digits = max(3L, getOption("digits") - 3L), ...) {
#     cli::cli_format_method({
#       cli::cli_h1("Nested Sampling Run from {.pkg ernest}")
#       cli::cli_bullets_raw(format(x@sampler, digits = digits))
#       cli::cli_h3("Results")
#       cli::cli_dl(c(
#         "Iterations" = "{x@n_iter}",
#         "Calls" = "{x@n_call}",
#         "Time" = "{pretty_dt(attr(x@progress, 'time'))}",
#         "Log. Z" = "{pretty_signif(x@log_z)} \U00B1 {pretty_signif(sqrt(x@log_z_var))}"
#       ))
#     })
#   }

#' @rdname ErnestLRPS
UniformCube <- S7::new_class(
  "UniformCube",
  parent = ErnestLRPS
)
S7::method(format, UniformCube) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cli::cli_format_method({
      cli::cli_dl(c("Method" = "Uniform Hypercube", sampler_info))
    })
  }

#' @rdname ErnestLRPS
#'
#' @slot steps The minimum number of steps to take during the random walk. Must
#' be an integer larger than zero.
#' @slot epsilon The initial step size of the random walk. This is refined
#' during each run to target a 0.5 acceptance rate.
RandomWalkCube <- S7::new_class(
  "RandomWalkCube",
  properties = list(
    steps = prop_natural(include_zero = FALSE, default = 20L),
    epsilon = S7::new_property(
      S7::class_numeric,
      default = 0.1
    )
  ),
  parent = ErnestLRPS
)
S7::method(format, RandomWalkCube) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cli::cli_format_method({
      cli::cli_dl(c(
        "Sampler" = "Random Walk in the Unit Hypercube",
        sampler_info,
        "Steps" = x@steps,
        "Epsilon" = x@epsilon
      ))
    })
  }
