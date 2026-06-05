#' Generate new points from the unconstrained prior distribution
#'
#' Use rejection sampling across the entire prior distribution to create new
#' samples. This is highly inefficient as an LRPS, but may be useful for
#' testing the behaviour of a nested sampling specification.
#'
#' @returns `[unif_cube]`, a named list that inherits from [[ernest_lrps]].
#'
#' @references Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package
#' for Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
#' Royal Astronomical Society, 493, 3132–3158.
#' \doi{10.1093/mnras/staa278}
#'
#' @srrstats {BS4.0} References the software containing the sampling algorithm.
#'
#' @inheritSection new_ernest_lrps Loop safety
#'
#' @examples
#' data(example_run)
#' lrps <- unif_cube()
#'
#' ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#' @family ernest_lrps
#' @export
unif_cube <- function() {
  new_unif_cube()
}

#' @noRd
#' @export
format.unif_cube <- function(x, ...) {
  "Uniform unit cube sampling"
}

#' Create a new unif_cube LRPS
#'
#' Internal constructor for the uniform unit cube LRPS.
#'
#' @param ... Arguments forwarded to `new_ernest_lrps()`.
#'
#' @return An LRPS specification, a list with class
#' `c("unif_cube", "ernest_lrps")`.
#' @noRd
new_unif_cube <- function(...) {
  new_ernest_lrps(..., .class = "unif_cube")
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
      nvar = x$nvar,
      max_loop = x$max_loop
    )
    env_poke(x$cache, "neval", x$cache$neval + res$neval)
    res
  }
}

#' @noRd
#' @export
update_lrps.unif_cube <- function(x, ...) {
  do.call(new_unif_cube, as.list(x))
}
