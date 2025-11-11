#' Generate samples from the unconstrained prior distribution
#'
#' Use rejection sampling across the entire prior distribution to create new
#' live points. This is highly inefficient as an LRPS, but may be useful for
#' testing the behaviour of a nested sampling specification.
#'
#' @returns A list with class `c("unif_cube", "ernest_lrps")`. Can be used with
#' [ernest_sampler()] to specify the sampling behaviour of a nested sampling
#' run.
#'
#' @references Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package
#' for Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
#' Royal Astronomical Society, 493, 3132–3158.
#' \doi{10.1093/mnras/staa278}
#'
#' @srrstats {BS4.0} References the software containing the sampling algorithm.
#'
#' @examples
#' data(example_run)
#' lrps <- unif_cube()
#'
#' ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#' @family ernest_lrps
#' @export
unif_cube <- function() {
  new_unif_cube(
    unit_log_fn = NULL,
    n_dim = NULL,
    max_loop = getOption("ernest.max_loop", 1e6L)
  )
}

#' @noRd
#' @export
format.unif_cube <- function(x, ...) {
  NextMethod()
}

#' Create a new unif_cube LRPS
#'
#' Internal constructor for the uniform unit cube LRPS.
#'
#' @param unit_log_fn Function for computing log-likelihood in unit space.
#' @param n_dim Integer. Number of dimensions.
#' @param max_loop Integer. Maximum number of proposal attempts.
#' @param cache Optional cache environment.
#'
#' @srrstats {G2.4, G2.4a, G2.4b} Explicit conversion of inputs to expected
#' types or error messages for univariate inputs.
#'
#' @return An LRPS specification, a list with class
#' `c("unif_cube", "ernest_lrps")`.
#' @noRd
new_unif_cube <- function(
  unit_log_fn = NULL,
  n_dim = NULL,
  max_loop = 1e6L,
  cache = NULL
) {
  new_ernest_lrps(
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    cache = cache,
    .class = "unif_cube"
  )
}

#' @rdname propose
#' @export
propose.unif_cube <- function(
  x,
  original = NULL,
  criterion = -Inf
) {
  if (is.null(original)) {
    NextMethod(x)
  } else {
    res <- propose_cube(
      unit_log_fn = x$unit_log_fn,
      criterion = criterion,
      n_dim = x$n_dim,
      max_loop = x$max_loop
    )
    env_poke(x$cache, "n_call", x$cache$n_call + res$n_call)
    res
  }
}

#' @noRd
#' @export
update_lrps.unif_cube <- function(x, ...) {
  do.call(new_unif_cube, as.list(x))
}
