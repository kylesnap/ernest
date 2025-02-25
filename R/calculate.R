#' Calculate statistics from an `ErnestSampler` object.
#'
#' @param x An `ErnestSampler` object.
#' @param exponentiate Whether to report likelihoods and evidence estimates
#' in exponentiated units.
#' @param progress Whether to return a tibble describing
#' each update to the evidence integral estimate at each iteration (`TRUE`,
#' the default), or to return a one-row `tibble` with the final estimates.
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
calculate.ErnestSampler <- function(x, exponentiate = FALSE,
                                 progress = TRUE, ...) {
  int_rcrd <- x@wrk$integral_rcrd
  integration <- compute_integral(
    vctrs::field(int_rcrd, "log_lik"),
    vctrs::field(int_rcrd, "log_vol")
  )

  expected_len <- x@n_iter + x@n_points

  if (!progress) {
    tibble::tibble_row(
      "log_vol" = integration$log_vol[expected_len],
      if (exponentiate) {
        tibble::tibble(
          "lik" = exp(integration$log_lik[expected_len]),
          "evidence" = exp(integration$log_z[expected_len]),
        )
      } else {
        tibble::tibble(
          "log_lik" = integration$log_lik[expected_len],
          "log_evidence" = integration$log_z[expected_len]
        )
      },
      "log_evidence_err" = sqrt(integration$log_z_var[expected_len])
    )
  }

  else {
    tibble::tibble(
      "log_vol" = integration$log_vol,
      if (exponentiate) {
        tibble::tibble(
          "lik" = exp(integration$log_lik),
          "lik_norm" = exp(integration$log_lik - max(integration$log_lik)),
          "weight" = exp(integration$log_weight - tail(integration$log_z, 1)),
          "evidence" = exp(integration$log_z),
        )
      } else {
        tibble::tibble(
          "log_lik" = integration$log_lik,
          "log_lik_norm" = integration$log_lik - max(integration$log_lik),
          "log_weight" = integration$log_weight - tail(integration$log_z, 1),
          "log_evidence" = integration$log_z,
        )
      },
      "log_evidence_err" = sqrt(integration$log_z_var)
    )
  }
}

#' S7 dispatch method
#' @noRd
calculate_ernest <- new_external_generic("generics", "calculate", "x")
method(calculate_ernest, ErnestSampler) <- calculate.ErnestSampler
