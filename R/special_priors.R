#' @srrstats {BS2.5} Special priors perform checks to ensure that their
#' parameters are admissable to each distribution.

#' Normally-Distributed Prior (Possibly Truncated)
#'
#' @rdname ernest_prior
#' @inheritParams stats::qnorm
#' @export
create_normal_prior <- function(
  names = NULL,
  mean = 0,
  sd = 1,
  lower = -Inf,
  upper = Inf,
  repair = c(
    "unique",
    "universal",
    "check_unique",
    "unique_quiet",
    "universal_quiet"
  )
) {
  mean <- vctrs::vec_cast(mean, double())
  sd <- vctrs::vec_cast(sd, double())
  if (any(sd <= 0)) {
    cli::cli_abort(
      "All elements of {.arg sd} must be strictly positive and non-missing."
    )
  }
  params <- recycle_params(
    names = names,
    lower = lower,
    upper = upper,
    mean = mean,
    sd = sd
  )
  truncated <- any(is.finite(params$lower)) || any(is.finite(params$upper))
  names <- names %||%
    sprintf(
      "Normal_%s_%s",
      gsub("-", "m", prettyunits::pretty_round(params$mean, 1)),
      prettyunits::pretty_round(params$sd, 1)
    )

  x <- NULL
  fn <- if (truncated) {
    check_installed(
      "extraDistr",
      "calculate quantiles for the truncated normal distribution"
    )
    new_function(
      exprs(x = ),
      expr(
        (!!extraDistr::qtnorm)(
          x,
          mean = !!params$mean,
          sd = !!params$sd,
          a = !!params$lower,
          b = !!params$upper
        )
      ),
      env = empty_env()
    )
  } else {
    new_function(
      exprs(x = ),
      expr(
        (!!stats::qnorm)(
          x,
          mean = !!params$mean,
          sd = !!params$sd
        )
      ),
      env = empty_env()
    )
  }

  new_ernest_prior(
    fn = fn,
    names = names,
    lower = params$lower,
    upper = params$upper,
    .class = c(if (truncated) "trunc_prior", "normal_prior"),
    .repair = repair
  )
}

#' Uniform Prior
#'
#' @rdname ernest_prior
#' @inheritParams stats::qnorm
#' @export
create_uniform_prior <- function(
  names = NULL,
  lower = 0,
  upper = 1,
  repair = c(
    "unique",
    "universal",
    "check_unique",
    "unique_quiet",
    "universal_quiet"
  )
) {
  if (!is_double(lower, finite = TRUE)) {
    stop_input_type(lower, "a double vector with finite values")
  }
  if (!is_double(upper, finite = TRUE)) {
    stop_input_type(upper, "a double vector with finite values")
  }
  params <- recycle_params(names, lower = lower, upper = upper)
  names <- names %||%
    sprintf(
      "Uniform_%s_%s",
      gsub("-", "m", prettyunits::pretty_round(params$lower, 1)),
      gsub("-", "m", prettyunits::pretty_round(params$upper, 1))
    )
  diff <- params$upper - params$lower
  # Evaluating in the empty-environment requires prefix operators
  fn <- new_function(
    exprs(x = ),
    expr(
      (!!`+`)(!!params$lower, (!!`*`)(x, !!diff))
    ),
    env = empty_env()
  )

  new_ernest_prior(
    fn = fn,
    names = names,
    lower = params$lower,
    upper = params$upper,
    .class = "uniform_prior",
    .repair = repair
  )
}

#' Recycle prior parameters to the right length
recycle_params <- function(names, lower, upper, ..., .call = caller_env()) {
  n_dim <- if (is.null(names)) NULL else length(names)
  params <- vctrs::vec_recycle_common(
    lower = lower,
    upper = upper,
    ...,
    .size = n_dim,
    .call = .call
  )
  params$.n_dim <- n_dim %||% vctrs::vec_size_common(!!!params)
  params
}
