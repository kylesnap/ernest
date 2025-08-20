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
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G1.6} No comparisons against other R packages are made; instead, we focus on comparing ernest against the python package nestle.
#' @srrstatsNA {G4.0} ernest does not save files to disk.
#' @srrstatsNA {BS1.0} The term 'hyperparameter' is avoided.
#' @srrstatsNA {BS1.4, BS1.5} ernest does not currently use an MCMC convergence checker (due to NS's primary focus on estimating evidence.)
#'
#'
#' @noRd
NULL
