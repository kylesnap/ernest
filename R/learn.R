#' Bootstrap Resampling for Nested Sampling Uncertainty
#'
#' Computes bootstrap resamples of a nested sampling run to empirically
#' estimate uncertainty in evidence and posterior summaries.
#'
#' @param x [[ernest_run]]\cr An object containing the results of a nested
#' sampling run.
#' @param times `[[integer(1)]]`\cr Number of bootstrap resamples to generate.
#' Must be larger than zero.
#' @param include_weights `[[logical(1)]]`\cr If `TRUE`, include a list-column
#' of resampled weights for each resampled run in the output.
#' @param ... Reserved for future extensions; must be empty.
#'
#' @return A special class of [[tibble]] with one row per resample, containing:
#' * `niter`: Number of iterations in the resample.
#' * `log_evidence`: Estimated log-evidence for the resample.
#' * One column per parameter: weighted posterior mean for each parameter.
#' * If `include_weights = TRUE`, a list-column `weights` with the resampled
#' weights and iteration indices for each resampled run.
#'
#' @details
#' Higson et al. (2019) describes a bootstrap resampling procedure for nested
#' sampling. This involves spliting a run into `nlive` runs of one live point,
#' then merging runs together by sampling from these runs with replacement.
#' This provides an empirical estimate of the uncertainty due to the
#' stochastic nature of selecting likelihood shells from the parameter space.
#'
#' @seealso [calculate.ernest_run()] for evidence estimation without
#' bootstrapping.
#'
#' @references
#' * Higson, E., Handley, W., Hobson, M., & Lasenby, A. (2019).
#' Nestcheck: Diagnostic Tests for Nested Sampling Calculations. Monthly Notices
#' of the Royal Astronomical Society, 483(2), 2044–2056.
#' \doi{10.1093/mnras/sty3090}
#' * Speagle, J. S. (2020). dynesty: A Dynamic Nested Sampling Package for
#' Estimating Bayesian Posteriors and Evidences. Monthly Notices of the Royal
#' Astronomical Society, 493, 3132–3158. \doi{10.1093/mnras/staa278}
#'
#' @examples
#' data(example_run)
#' # Generate 100 bootstrap resamples
#' learn(example_run, times = 100)
#' # Include resampled weights for each draw
#' learn(example_run, times = 10, include_weights = TRUE)
#' @export
learn.ernest_run <- function(
  x,
  times = 100,
  include_weights = FALSE,
  ...
) {
  check_number_whole(times, min = 1)
  check_bool(include_weights)
  threads <- get_threads(x)
  vctrs::vec_rbind(
    !!!replicate(
      times,
      run_resample(x$nlive, threads, include_weights),
      simplify = FALSE
    )
  )
}

#' Custom header for ernest_resample
#' @importFrom tibble tbl_sum
#' @export
#' @noRd
tbl_sum.ernest_resample <- function(x, ...) {
  c("Nested sampling estimates" = sprintf("%d replications", attr(x, "times")))
}

#' Split run into live point threads
#'
#' @param x Results from a nested sampling run.
#'
#' @return A list with two elements:
#' * `thread`: A list of data frames, each containing the log-likelihoods and
#' iterations for a single thread of live points.
#' * `max_log_lik`: A numeric vector of the maximum log-likelihood for each
#' thread, used to determine when threads are active during resampling.
#' @noRd
get_threads <- function(x) {
  x_rcrd <- as_ernest_rcrd(x)
  threads <- vctrs::vec_split(x_rcrd, field(x_rcrd, "id"))
  threads$max_lik <- vapply(
    threads$val,
    \(x) max(field(x, "log_lik")),
    double(1)
  )
  threads
}

#' Aggregates summary statistics for resampled data by applying a summarization
#' function to each resampled iteration.
#'
#' @param nlive The number of live points in the sampler
#' @param threads The split thread object.
#' @param include_weights
#'
#' @return A list of the results: log_evidence, and means for each variable.
#' @noRd
run_resample <- function(nlive, threads, include_weights = FALSE) {
  resample <- sample.int(nlive, replace = TRUE)
  min_max_lik <- min(threads$max_lik[resample])
  sim <- vctrs::vec_c(
    !!!lapply(
      threads$val[resample],
      \(x) {
        x[seq(match(TRUE, field(x, "log_lik") >= min_max_lik))]
      }
    )
  ) |>
    sort()
  integral <- compute_integral(
    field(sim, "log_lik"),
    get_log_vol(nlive, length(sim) - nlive)
  )
  weight <- exp(integral$log_weight - integral$log_evidence[length(sim)])
  tibble::tibble_row(
    "log_evidence" = integral$log_evidence[length(sim)],
    !!!matrixStats::colWeightedMeans(as.list(sim)[["unit"]], w = weight),
    "weight" = if (include_weights) list(weight)
  )
}
