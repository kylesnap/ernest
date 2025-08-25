#' srr_stats
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.2} Life cycle statement found in CONTRIBUTING.md.
#' @srrstats {G1.4, G1.4a} Software uses `roxygen2`, with internal functions
#' documented using `@noRd` tags.
#' @srrstats {G2.2} The `rlang` package is used to check for illegal
#' multivariate inputs when they are not expected (see
#' `import-standalone-types-check.R`).
#' @srrstats {G2.6} Ernest uses the `vctrs` package to handle unidimensional
#' input, allowing Ernest to handle S3-classed objects by casting inputs into
#' numeric vectors (or failing informatively).
#' @srrstats {G3.0} Ernest does not use `==` to make equality comparisons
#' between non-integers.
#' @srrstats {G5.10, G5.12} Extended tests are controlled with an environment
#' option set in `setup.R`. A note has been added to `CONTRIBUTING.md`.
#' @srrstats {BS4.1} Ernest makes testing comparisons against Nestle. The
#' package vignettes demonstrate how Ernest uses S3 methods to produce nested
#' sampling run objects that work well with existing tools in R.
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G1.6} No comparisons against other R packages are made; instead,
#' we focus on comparing Ernest against the Python package Nestle.
#' @srrstatsNA {G4.0} Ernest does not save files to disk.
#' @srrstatsNA {G2.4d, G2.4e, G2.5} Ernest does not have functions that allow
#' for factor-type inputs.
#' @srrstatsNA {G2.7, G2.8, G2.9, G2.10, G2.11, G2.12} Ernest does not directly
#' accept tabular data. Instead, data are provided through the
#' `ernest_likelihood` object (see examples of `create_likelihood`).
#' @srrstatsNA {BS1.0} The term 'hyperparameter' is avoided.
#' @srrstatsNA {BS1.4, BS1.5} Ernest does not currently use an MCMC convergence
#' checker (due to NS's primary focus on estimating evidence).
#' @srrstatsNA {BS2.7, BS2.9, BS2.10, BS2.11} NS does not rely on setting
#' explicit starting values before generating samples.
#' @srrstatsNA {BS2.15} Ernest aims to gracefully report errors during a run,
#' but does not allow for them to be ignored as warnings. The best behaviour for
#' NS to "skip" invalid points when encountered is yet to be determined.
#' @srrstatsNA {G3.1, G3.1a} Ernest does not rely on covariance calculations.
#' @srrstatsNA {G5.4a, G5.4c} Ernest makes comparisons against the Python
#' package `nestle` to demonstrate its efficacy.
#' @srrstatsNA {G5.11, G5.11a} Ernest does not download data for external tests.
#' @srrstatsNA {BS5.3, BS5.4, BS5.5} Currently, no convergence checker is
#' implemented. As Ernest performs nested sampling, runs terminate once the
#' estimated log evidence falls below a threshold (or a plateau/stopping
#' condition is encountered). The behaviour of the log evidence estimates can be
#' examined through plots and summary functions.
#' @srrstatsNA {BS6.5} To avoid overcrowding ggplot objects with multiple
#' facets, side-by-side trace plots with posterior distributions are currently
#' unavailable.
#' @noRd
NULL
