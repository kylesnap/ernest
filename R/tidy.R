#' @include ErnestRun_class.R

#' Turn the results of a nested sampling run into a tidy tibble
#'
#' @param x An object of class `ErnestRun`
#' @param exponentiate Whether to report likelihoods and evidence estimates
#' in exponentiated units.
#' @param ... Ignored.
#'
#' @returns A tibble, with the following columns:
#' * `log_vol`: The log prior volume of the live points.
#' * `lik`: The likelihood of the live points.
#' * `lik_norm`: The normalized likelihood of the live points.
#' * `weight`: The weight of the live points.
#' * `evidence`: The evidence estimate.
#' * `log_evidence_err`: The error in the evidence estimate.
#'
#' If `exponentiate` is `FALSE`, the columns `lik`, `lik_norm`, `weight`, and
#' `evidence` will be in log units. Note that `log_vol` and `log_evidence_err`
#' are always returned in the log scale.
NULL

#' @rdname tidy
#' @export
S7::method(tidy, ErnestLRPS) <- function(x, exponentiate = TRUE, ...) {
  int_rcrd <- x@wrk$integral_rcrd
  integration <- compute_integral(
    vctrs::field(int_rcrd, "log_lik"),
    vctrs::field(int_rcrd, "log_vol")
  )

  if (exponentiate) {
    tibble::tibble(
      "log_vol" = integration$log_vol,
      "lik" = exp(integration$log_lik),
      "lik_norm" = exp(integration$log_lik - max(integration$log_lik)),
      "weight" = exp(integration$log_weight - tail(integration$log_z, 1)),
      "evidence" = exp(integration$log_z),
      "log_evidence_err" = integration$log_z_var
    )
  } else {
    tibble::tibble(
      "log_vol" = integration$log_vol,
      "log_lik" = integration$log_lik,
      "log_lik_norm" = integration$log_lik - max(integration$log_lik),
      "log_weight" = integration$log_weight - tail(integration$log_z, 1),
      "log_evidence" = integration$log_z,
      "log_evidence_err" = integration$log_z_var
    )
  }
}


