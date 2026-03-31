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
#' @inheritParams as_draws.ernest_run
#' @inheritParams rlang::args_dots_empty
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
#' sampling. This involves splitting a run into `nlive` runs of one live point,
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
  units = c("original", "unit_cube"),
  ...
) {
  check_dots_empty()
  check_number_whole(times, min = 1)
  check_bool(include_weights)
  units <- arg_match(units)
  est_volume <- get_log_vol(x$weights$log_lik, x$nlive)
  log_vol_rng <- range(est_volume)
  dead_log_vol <- est_volume[x$niter]

  x_rcrd <- as_ernest_rcrd(x)
  col_names <- attr(x_rcrd, "variables")
  if (units == "original") {
    field(x_rcrd, "unit") <- x$samples$original
  }
  threads <- get_threads(x_rcrd)
  res <- replicate(
    times,
    run_resample(x$nlive, threads, col_names, include_weights),
    simplify = FALSE
  )
  tibble::new_tibble(
    vctrs::vec_rbind(!!!res),
    log_vol_rng = log_vol_rng,
    dead_log_vol = dead_log_vol,
    class = "ernest_resample"
  )
}

#' Custom header for ernest_resample
#' @importFrom tibble tbl_sum
#' @export
#' @noRd
tbl_sum.ernest_resample <- function(x, ...) {
  c("Nested sampling estimates" = sprintf("%d replications", nrow(x)))
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
get_threads <- function(x_rcrd) {
  threads <- vctrs::vec_split(x_rcrd, field(x_rcrd, "id"))
  threads$val <- lapply(
    threads$val,
    \(x) as.list(x)[c("log_lik", "unit")]
  )
  threads$max_lik <- vapply(
    threads$val,
    \(x) max(x$log_lik),
    double(1)
  )
  threads
}

#' Aggregates summary statistics for resampled data by applying a summarization
#' function to each resampled iteration.
#'
#' @param nlive The number of live points in the sampler
#' @param threads The split thread object.
#' @param col_names Variable names within the sampler
#' @param include_weights Whether or not to append a list-column of weights
#'
#' @return A list of the results: log_evidence, and means for each variable.
#' @noRd
run_resample <- function(nlive, threads, col_names, include_weights = FALSE) {
  resample <- sample.int(nlive, replace = TRUE)
  min_max_lik <- min(threads$max_lik[resample])
  rel_n <- vapply(
    threads$val[resample],
    \(x) match(TRUE, x$log_lik >= min_max_lik),
    integer(1)
  )
  log_lik <- vctrs::vec_c(
    !!!.mapply(
      \(x, i) x$log_lik[seq_len(i)],
      dots = list(x = threads$val[resample], i = rel_n),
      MoreArgs = NULL
    )
  )
  unit <- vctrs::vec_c(
    !!!.mapply(
      \(x, i) x$unit[seq_len(i), , drop = FALSE],
      dots = list(x = threads$val[resample], i = rel_n),
      MoreArgs = NULL
    )
  )
  lik_order <- order(log_lik)
  log_lik <- log_lik[lik_order]
  unit <- unit[lik_order, , drop = FALSE]
  integral <- compute_integral(log_lik, nlive)

  weight <- exp(integral$log_weight - integral$log_evidence[length(log_lik)])
  means <- matrixStats::colWeightedMeans(unit, w = weight)
  names(means) <- col_names
  tibble::tibble_row(
    log_evidence = integral$log_evidence[length(log_lik)],
    !!!means,
    weights = if (include_weights) {
      list(vctrs::df_list(
        "log_volume" = integral$log_volume,
        "weight" = weight
      ))
    }
  )
}
