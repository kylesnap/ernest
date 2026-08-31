#' Preserve seed for a run
#'
#' @param seed A number, or `NA` to preserve the current seed.
#' @param .local_envir Forwarded to withr.
#'
#' @return `seed`, invisibly.
#' @noRd
preserve_seed <- function(seed, .local_envir = parent.frame()) {
  if (is.na(seed)) {
    withr::local_preserve_seed(.local_envir = .local_envir)
    return(invisible(NA))
  }
  withr::local_seed(seed, .local_envir = .local_envir)
  invisible(seed)
}

#' Check the class of an object
#'
#' Validates that an object inherits from at least one of the specified classes.
#'
#' @param x An object to check.
#' @param class A character vector of allowed classes.
#' @param ... Additional arguments passed to error handlers.
#' @param allow_null Logical. If TRUE, allows NULL values.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if `x` inherits from one of the specified
#' classes, otherwise throws an informative error.
#' @noRd
check_class <- function(
  x,
  class,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if ((allow_null && is.null(x)) || inherits_any(x, class)) {
    return(invisible(NULL))
  }

  cls_format <- cli::pluralize(
    "an object with {?class/at least one class from} {class}"
  )
  stop_input_type(
    x,
    cls_format,
    ...,
    allow_na = FALSE,
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

#' Log-space subtraction
#'
#' @param a,b Numeric vectors of equal length.
#'
#' @return `log(exp(a) - exp(b))`, computed in log-space to avoid numerical
#' underflow. A warning is issued and `NaN` is returned when `b > a`.
#' @noRd
logspace_sub <- function(a, b) {
  a + log1p(-exp(b - a))
}

#' Uniform sampling in a sphere
#'
#' @param n number of simulations
#' @param d dimension of the space
#' @param r radius of the sphere
#'
#' @source `uniformly` package, \doi{10.32614/CRAN.package.uniformly}
#'
#' @return The simulations in a \code{n} times \code{d} matrix.
#' @noRd
runif_in_sphere <- function(n, d, r = 1) {
  sims <- matrix(rnorm(n * d), nrow = n, ncol = d)
  radii <- r * runif(n)^(1 / d)
  radii * sims / sqrt(apply(sims, 1L, crossprod))
}

#' Helper to check merge quality for `merge` and parallel sampling
#'
#' @param observed A single `ernest_rcrd` object.
#' @param actual A tibble of rows from `glance` of the pre-merged runs.
#' @param loss_ess The max loss of ESS allowed between expected and observed
#' as a proportion of the expected ESS. Default is -0.05 (5% loss).
#' @param call Call environment for error messages.
#'
#' @return A warning is issued if the merged results may be biased due to
#' autocorrelation within subruns.
#' @noRd
check_merge_quality <- function(
  observed,
  actual,
  loss_ess = getOption("ernest.max_ess_loss", 0.05),
  call = caller_env()
) {
  observed_ess <- run_ess(observed)
  actual_ess <- sum(actual$ess)
  rel_diff <- (observed_ess - actual_ess) / actual_ess
  loss_ess <- abs(loss_ess)
  if (isTRUE(rel_diff > loss_ess)) {
    observed_ess <- formatC(observed_ess)
    actual_ess <- formatC(actual_ess)
    cli::cli_warn(
      c(
        "Merged results may be biased due to autocorrelation within subruns.",
        "!" = "Total ESS: {observed_ess}; Sum of sub-run ESS: {actual_ess}.",
        "i" = "Should you increase `nlive` within each sub-run?"
      ),
      call = call,
      class = "ernest.poor_quality_merge"
    )
  }
  invisible(NULL)
}

#' sum_ess <- sum(results$.parallel$ess)

#' Fast data frame creation
#'
#' @param ... Columns of the data frame.
#' @param .error_call Call environment for error messages.
#'
#' @source Internally used in ggdist.
#'
#' @return A data frame created with minimal name repair for speed.
#' @noRd
data_frame0 <- function(..., .error_call = current_env()) {
  vctrs::data_frame(..., .name_repair = "minimal", .error_call = .error_call)
}

#' Fast tibble creation
#'
#' @param x A data frame or list of columns.
#' @param ... Additional attributes to add to the tibble.
#' @param class Additional classes to add to the tibble.
#'
#' @source Recommended by ROpenSci standards.
#'
#' @return A tibble-ish object.
#' @noRd
new_tibble0 <- function(x, ..., class = NULL) {
  vctrs::new_data_frame(
    x = x,
    ...,
    class = c(class, "tbl_df", "tbl")
  )
}
