#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.2} Life cycle statement found in CONTRIBUTING.md.
#' @srrstats {G1.4, G1.4a} Software uses `roxygen2`, with internal functions documented with `@noRd` tags.
#' @srrstats {G2.2} rlang is used to check for illegal multivariate inputs when they are not expected (see `import-standalone-types-check.R`)
#' @srrstats {G2.6} Ernest uses the `vctrs` package to handle unidimensional input, allowing ernest to handle S3-classed by casting inputs into numeric vector (or failing informatively).
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G1.6} No comparisons against other R packages are made; instead, we focus on comparing ernest against the python package nestle.
#' @srrstatsNA {G4.0} ernest does not save files to disk.
#' @srrstatsNA {G2.4d, G2.4e, G2.5} ernest does not have functions that allow for factor-type inputs.
#' @srrstatsNA {G2.7, G2.8, G2.9, G2.10, G2.11, G2.12} ernest does not directly accept tabular data. Instead, data is provided through the ernest_likelihood object (see examples of `create_likelihood`).
#' @srrstatsNA {BS1.0} The term 'hyperparameter' is avoided.
#' @srrstatsNA {BS1.4, BS1.5} ernest does not currently use an MCMC convergence checker (due to NS's primary focus on estimating evidence.)
#' @srrstatsNA {BS2.7, BS2.9, BS2.10, BS2.11} NS does not rely on setting explicit starting values before generating samples.
#' @srrstatsNA {BS2.15} ernest aims to gracefully report errors during a run, but does not allow for them to be ignored as warnings. Would need to find best behaviour for NS to "skip" invalid points when they are encountered.
#'
#' @noRd
NULL
