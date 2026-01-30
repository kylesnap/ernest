#' @srrstats {BS2.5} Special priors perform checks to ensure that their
#' parameters are admissable to each distribution.

#' Create a special prior distribution
#'
#' Constructs a prior transformation object for priors with indpendent marginals
#' that are either normally or uniformly distributed. Additionally, normal
#' marginals can be truncated using the [extraDistr] package.
#'
#' @param names `[character()]`\cr Optional names for each parameter.
#' If `NULL`, default names and indices are generated.
#' @param mean,sd `[double()]`\cr Mean and standard deviation for each marginal
#' normal distribution. `sd` must be strictly positive.
#' @param lower,upper `[double()]`\cr Lower and upper bounds for each parameter.
#' If used with `create_normal_prior()`, these define the truncation limits.
#' @inheritParams create_prior
#'
#' @returns An [ernest_prior], additionally inheriting from the specialized
#' class `uniform_prior` or `normal_prior`.
#'
#' @details
#' The provided transformations are vectorized: they accept a matrix of points
#' in the unit hypercube and return a matrix of transformed values.
#'
#' @seealso [create_prior()] for more on priors within nested sampling.
#' @family priors
#' @rdname special_priors
#' @example ./data-raw/EXAMPLE_PRIOR_CLASS.R
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
  names <- if (!is.null(names)) {
    names
  } else {
    sprintf("Normal_%d", seq(vctrs::vec_size_common("mean" = mean, "sd" = sd)))
  }
  c(mean, sd, lower, upper) %<-%
    vctrs::vec_recycle_common(
      "mean" = mean,
      "sd" = sd,
      "lower" = lower,
      "upper" = upper,
      .size = length(names)
    )

  truncated <- any(is.finite(lower)) || any(is.finite(upper))
  if (truncated) {
    check_installed("extraDistr", "calculating a truncated normal prior")
  }
  fn <- if (truncated) {
    function(x) {
      nrow <- nrow(x) %||% 1
      y <- extraDistr::qtnorm(
        c(x),
        a = rep(lower, each = nrow),
        b = rep(upper, each = nrow),
        mean = rep(mean, each = nrow),
        sd = rep(sd, each = nrow)
      )
      dim(y) <- dim(x)
      y
    }
  } else {
    function(x) {
      nrow <- nrow(x) %||% 1
      y <- stats::qnorm(
        c(x),
        mean = rep(mean, each = nrow),
        sd = rep(sd, each = nrow)
      )
      dim(y) <- dim(x)
      y
    }
  }

  do.call(
    new_ernest_prior,
    list2(
      fn = fn,
      names = names,
      mean = mean,
      sd = sd,
      lower = lower,
      upper = upper,
      interface = "vectorized_fn",
      .class = c(if (truncated) "trunc_prior", "normal_prior"),
      .repair = repair
    )
  )
}

#' Uniform distribution
#' @rdname special_priors
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
  names <- if (!is.null(names)) {
    names
  } else {
    sprintf(
      "Uniform_%d",
      seq(vctrs::vec_size_common("lower" = lower, "upper" = upper))
    )
  }
  c(lower, upper) %<-%
    vctrs::vec_recycle_common(
      "lower" = lower,
      "upper" = upper,
      .size = length(names)
    )

  fn <- function(x) {
    nrow <- nrow(x) %||% 1
    y <- stats::qunif(
      c(x),
      min = rep(lower, each = nrow),
      max = rep(upper, each = nrow)
    )
    dim(y) <- dim(x)
    y
  }

  new_ernest_prior(
    fn = fn,
    names = names,
    lower = lower,
    upper = upper,
    interface = "vectorized_fn",
    .repair = repair,
    .class = "uniform_prior"
  )
}
