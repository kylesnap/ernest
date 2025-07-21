#' Prepare a nested sampler to evaluate Bayesian evidence
#'
#' Initialize a [ernest_sampler] object to perform nested sampling with a given
#' log-likelihood function, prior distribution, and likelihood-restricted prior
#' specification.
#'
#' @param log_lik Function, defining the log-likelihood of the model given
#' a vector of parameters. Passed to [create_likelihood()].
#' @param prior An [ernest_prior] object, created by [create_prior()] or
#' its specializations.
#' @param sampler An [ernest_sampling] object, declaring which
#' likelihood-restricted prior sampler (LRPS) to use. One of [rwmh_cube()]
#' (recommended) or [unif_cube()].
#' @param n_points Integer. The number of live points to use in the nested
#' sampling run.
#' @param first_update Integer or `NULL`. The number of likelihood calls before
#' the first update of the LRPS. If `NULL`, this is set to `n_points * 2.5`.
#' @param update_interval Integer or `NULL`. The number of likelihood calls
#' between subsequent updates of the LRPS. If `NULL`, this is set to
#' `n_points * 1.5`.
#' @param on_warning Character string specifying how to handle a warning during
#' the sanity checks (see Details). One of `"abort"` or `"warn"`, case
#' sensitive.
#' - `"abort"`: Throw an error and stop generating the sampler.
#'  - `"warn"`: Issue a warning, but continue generating the sampler.
#' @param ... Additional arguments passed to [create_likelihood()].
#'
#' @return An [ernest_sampler] object, prepared for nested sampling.
#'
#' @export
#' @examples
#' prior <- create_uniform_prior(n_dim = 2, lower = -1, upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- nested_sampling(ll_fn, prior, n_points = 100)
#' sampler
nested_sampling <- function(
  log_lik,
  prior,
  sampler = rwmh_cube(),
  n_points = 500,
  first_update = NULL,
  update_interval = NULL,
  on_warning = c("abort", "warn"),
  ...
) {
  check_dots_used()
  if (!inherits(prior, "ernest_prior")) {
    stop_input_type(prior, "an ernest_prior object")
  }
  log_lik <- create_likelihood(log_lik, ...)

  on_warning <- arg_match(on_warning)
  try_fetch(
    {
      sanity_prior <- prior$fn(rep(0.5, prior$n_dim))
      sanity_val <- log_lik(prior$fn(rep(0.5, prior$n_dim)))
      if (!is_scalar_double(sanity_val)) {
        cli_abort(c(
          "`log_lik(vec)` must return a scalar double for a vector input.",
          "!" = "Returned {obj_type_friendly(sanity_val)}."
        ))
      }
      if (!is.finite(sanity_val) && sanity_val != -Inf) {
        cli_abort(c(
          "`log_lik(vec)` must return a finite double or `-Inf`.",
          "!" = "Returned {obj_type_friendly(sanity_val)}."
        ))
      }
    },
    error = function(cnd) {
      cli_abort("Failed sanity checks.", parent = cnd)
    },
    warning = function(cnd) {
      if (on_warning == "abort") {
        cli_abort("Failed sanity check.", parent = cnd)
      } else {
        cli_warn(
          c(
            "Sanity check caused a warning.",
            "i" = "Did you set the error behaviour with [create_likelihood()]?"
          ),
          parent = cnd
        )
      }
    }
  )

  first_update <- first_update %||% n_points * 2.5
  update_interval <- update_interval %||% n_points * 1.5

  ernest_sampler$new(
    log_lik,
    prior,
    sampler,
    n_points,
    first_update,
    update_interval
  )
}
