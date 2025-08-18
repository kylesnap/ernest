#' Uniformly sample from the unconstrained prior distribution
#'
#' Generate new live points by performing rejection sampling across the entire
#' prior distribution. This is horribly inefficient as an LRPS, but you might
#' find this helpful for testing the behaviour of a nested sampling
#' specification.
#'
#' @returns A list, with class `c("unif_cube", "ernest_lrps")`. Can be used with
#' [ernest_sampler()] to specify the sampling behaviour of a nested sampling run.
#'
#' @references Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package
#' for Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
#' Royal Astronomical Society, 493, 3132–3158.
#' <https://doi.org/10.1093/mnras/staa278>
#'
#' @family ernest_lrps
#' @export
unif_cube <- function() {
  new_unif_cube(
    unit_log_fn = NULL,
    n_dim = NULL,
    max_loop = getOption("ernest.max_loop", 1e6L)
  )
}

#' Format method for unif_cube
#' @param x A `unif_cube`
#' @param ... Ignored.
#' @returns A string
#' @noRd
#' @export
format.unif_cube <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_text("Uniform Unit Cube LRPS {.cls {class(x)}}")
    cli::cli_text("No. Dimensions: {x$n_dim %||% 'Uninitialized'}")
    cli::cli_text("No. Calls Since Update: {x$cache$n_call %||% 0L}")
  })
}

#' Create a new unif_cube LRPS
#'
#' Internal constructor for the uniform unit cube LRPS.
#'
#' @param unit_log_fn,n_dim,max_loop,cache See [new_ernest_lrps].
#'
#' @returns An LRPS specification, a list with class
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
propose.unif_cube <- function(x, original = NULL, criteria = NULL, ...) {
  if (is.null(original)) {
    NextMethod()
  } else {
    cur_call <- env_cache(x$cache, "n_call", 0)
    res <- UniformCube(
      criteria = criteria,
      unit_log_lik = x$unit_log_fn,
      num_dim = x$n_dim,
      max_loop = x$max_loop
    )
    env_poke(x$cache, "n_call", cur_call + res$n_call)
    res
  }
}
