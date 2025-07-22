#' Prepare a nested sampler to evaluate Bayesian evidence
#'
#' Initialize a [ernest_sampler] object to perform nested sampling with a given
#' log-likelihood function, prior distribution, and likelihood-restricted prior
#' specification.
#'
#' @param log_lik (function) A function that takes a vector of parameters and
#' returns the log-likelihood of the parameters. The function should return a
#' finite value or `-Inf` for invalid parameters (see [create_likelihood()]).
#' @param prior (ernest_prior) An object with class `ernest_prior`, created by
#' [create_prior()] or its specializations.
#' @param sampler (ernest_sampling) An [ernest_sampling] object, declaring which
#' likelihood-restricted prior sampler (LRPS) to use.
#' @param n_points (positive integer) The number of live points to use in the
#' nested sampling run.
#' @param first_update (optional positive integer) The number of likelihood
#' calls before adopting LRPS behaviour. If `NULL`, this is set to
#' `n_points * 2.5`.
#' @param update_interval (optional positive integer) The number of likelihood
#' calls between updates to the LRPS behaviour. If `NULL`, this is set to
#' `n_points * 1.5`.
#' @param on_warning (case-sensitive string) Action to perform when the sanity
#' test throws a warning message (see Details).
#' * `"abort"`: Throw an error.
#' * `"warn"`: Issue a warning and initialize the sampler.
#' @inheritDotParams create_likelihood.function -fn
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
  is_class(prior, "ernest_prior")
  log_lik <- create_likelihood(log_lik, ...)

  on_warning <- arg_match(on_warning)
  try_fetch(
    {
      sanity_val <- log_lik(prior$fn(rep(0.5, prior$n_dim)))
      msg <- checkmate::check_number(
        sanity_val,
        na.ok = FALSE,
        null.ok = FALSE
      )
      if (!isTRUE(msg)) {
        format_checkmate(msg, "log_lik", call = caller_env())
      }
      if (!is.finite(sanity_val) && sanity_val != -Inf) {
        cli_abort(
          "`log_lik` must return a finite value or `-Inf`, not {sanity_val}.",
          call = caller_env()
        )
      }
    },
    error = function(cnd) {
      cli_abort("Failed sanity check.", parent = cnd)
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
