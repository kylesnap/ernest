#' Calculate statistics from an `ErnestSampler` object.
#'
#' @param x An `ErnestSampler` object.
#' @param ... Ignored.
#'
#' @returns A tibble with the following columns:
#' * `log_vol`: The estimated log volume of the prior.
#' * `lik`: The likelihood of the live points at that iteration.
#' * `evidence`: The evidence estimate.
#' * `log_evidence_err`: The error in the evidence estimate.
#'
#' Additionally, if `progress` is `TRUE`, add these additional columns:
#' * `lik_norm`: The normalized likelihood at each iteration.
#' * `weight`: The importance weight attributed to each discarded point
#' at each iteration.
#'
#' If `exponentiate` is `FALSE`, the columns `lik`, `lik_norm`, `weight`, and
#' `evidence` will be in log units and labelled appropriately.
#' The columns `log_vol` and `log_evidence_err` are always returned in the
#' log scale.
#'
#' @export
calculate.ErnestSampler <- function(x, ...) {
  check_dots_empty(...)
  integral <- x$wrk$integral_rcrd
  if (is.null(integral)) {
    return(NULL)
  }

  log_z <- if (!attr(integral, "partial")) {
    vctrs::field(integral, "log_z")
  } else {
    NULL
  }
  log_lik.norm <- vctrs::field(integral, "log_lik") -
    max(vctrs::field(integral, "log_lik"))
  log_weight <- if (!attr(integral, "partial")) {
    vctrs::field(integral, "log_weight") - tail(log_z, 1)
  } else {
    NULL
  }
  log_z_err <- if (!attr(integral, "partial")) {
    sqrt(vctrs::field(integral, "log_z_var"))
  } else {
    NULL
  }
  h <- if (!attr(integral, "partial")) {
    vctrs::field(integral, "h")
  } else {
    NULL
  }

  tibble::tibble(
    "log_lik" = vctrs::field(integral, "log_lik"),
    "log_lik.norm" = log_lik.norm,
    "log_vol" = vctrs::field(integral, "log_vol"),
    "log_weight" = log_weight,
    "log_z" = log_z,
    "log_z_err" = log_z_err,
    "h" = h
  )
}
