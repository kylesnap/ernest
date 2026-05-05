#' Confidence intervals for model evidence
#'
#' @inheritParams generate_bootstraps
#' @param parm `[character(1)]`\cr A specification describing whether to return
#' the model evidence in log-units.
#' @param level `[double(1)]`\cr The confidence level required.
#' @inheritParams rlang::check_dots_empty
#'
#' @returns A named vector with two elements, describing the lower and upper
#' confidence limits for each parameter.
#'
#' @seealso [generate_bootstraps()] for generating resampled nested sampling
#' runs from an `ernest_run` object.
#' @export
confint.ernest_run <- function(
  object,
  parm = c("log_evidence", "evidence"),
  level = 0.95,
  times = 100,
  ...
) {
  check_dots_empty()
  parm <- arg_match(parm)
  bootstraps <- generate_bootstraps(object, times = times)
  log_evidence <- vapply(bootstraps$run, log_evidence, double(1))
  if (parm == "evidence") {
    log_evidence <- exp(log_evidence)
  }
  stats::quantile(log_evidence, probs = c((1 - level) / 2, (1 + level) / 2))
}

#' Bootstrap resampling over a nested sampling run.
#'
#' @param object [[ernest_run]]\cr An object containing the results of a nested
#' sampling run.
#' @param times `[[integer(1)]]`\cr The number of bootstrap resamples.
#' @param units `[character(1)]`\cr The scale of the sampled points reported in
#' the resample; see [as_draws.ernest].
#' @param apparent `[[logical(1)]]`\cr Whether to perform an additional resample
#' using a copy of the original run data stored in `x`.
#' @inheritDotParams rlang::check_dots_empty
#'
#' @returns A tibble. The results include a column for an [ernest_rcrd] object
#' containing the resampled run, and a column called `id` that has a character
#' string with the resample identifier.
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
#' @srrstats {BS4.2} Allows for validating posterior estimates through
#' bootstrapping the importance weights.
#'
#' @examples
#' # Generate 100 bootstrap resamples
#' generate_bootstraps(example_run)
#' @export
generate_bootstraps <- function(
  x,
  times = 100,
  units = c("original", "unit_cube"),
  apparent = FALSE,
  ...
) {
  check_dots_empty()
  check_number_whole(times, min = 0)
  units <- arg_match(units)

  x_rcrd <- x$rcrd
  threads <- get_threads(x_rcrd)
  nthreads <- vctrs::vec_size(threads)
  sample_ids <- replicate(
    times,
    sample.int(nthreads, replace = TRUE),
    simplify = FALSE
  )

  field(x_rcrd, "unit") <- as_draws_matrix_(
    x,
    units = units,
    radial = FALSE
  )$points
  resampled <- resample_runs(x_rcrd, threads, x$nlive, sample_ids)
  sample_ids <- lapply(
    sample_ids,
    \(x) vctrs::vec_unrep(sort.int(x))
  )
  new_tibble0(
    data_frame0(
      "id" = seq_along(sample_ids),
      "split" = sample_ids,
      "run" = resampled
    )
  )
}

#' Split run into live point threads
#'
#' @param x Results object from a nested sampling run.
#'
#' @return A dataframe with three elements:
#' * key: The ID of the point
#' * loc: The location of each point death for the ID
#' * max_idx: The maximum IDX (i.e., the death iteration) of each ID.
#' @noRd
get_threads <- function(x) {
  threads <- vctrs::vec_group_loc(field(x, "id"))
  threads$max_idx <- vapply(
    threads$loc,
    \(idx) idx[[length(idx)]],
    integer(1)
  )
  threads
}

#' Aggregates summary statistics for resampled data by applying a summarization
#' function to each resampled iteration.
#'
#' @param x_rcrd The run rcrd object storing results.
#' @param nlive The number of live points within the run.
#' @param dead,live Two integer vectors indexing the resampled points from X.
#'
#' @return A list of the results: log_evidence, and means for each variable.
#' @noRd
resample_runs <- function(x_rcrd, threads, nlive, sample_ids) {
  # Index of the first dead point within each sample.
  sample_min_max <- vapply(
    sample_ids,
    \(ids) {
      min(threads$max_idx[ids])
    },
    integer(1)
  )

  # Indexes of the points from each run
  sample_idx <- mapply(
    \(ids, min_max) {
      d <- vctrs::vec_c(!!!threads$loc[ids], .ptype = integer())
      l <- vapply(
        threads$loc[ids],
        \(v) v[match(TRUE, v >= min_max)],
        integer(1)
      )
      vctrs::vec_c(d[d < min_max], l, .ptype = integer()) |> sort.int()
    },
    ids = sample_ids,
    min_max = sample_min_max
  )

  # Slice runs into samples
  sample_rcrd <- vctrs::vec_chop(x_rcrd, sample_idx)
  lapply(
    sample_rcrd,
    \(rcrd) {
      field(rcrd, "nlive") <- get_points(
        field(rcrd, "log_lik"),
        nlive = nlive,
        TRUE
      )
      live <- vctrs::num_as_location(-nlive:-1, n = vctrs::vec_size(rcrd))
      field(rcrd[live], "evals") <- rep(0L, length(live))
      rcrd
    }
  )
}

# sample_rcrd
# Repair the nlive
# log_volume <- apply(sample_log_lik, 2, \(col) {
#   get_points(col, nlive, TRUE)
# })
#   log_volume <- matrixStats::rowCumsums(-1 * t(log_volume^-1))
#   c(log_weight, log_evidence) %<-%
#     get_log_w(t(sample_log_lik), log_volume, FALSE)
#
#   mapply(
#     \(id, v, w, z) {
#       n <- vctrs::vec_size(id)
#       structure(
#         data_frame0(
#           "idx" = id,
#           "log_volume" = v[1:n],
#           "log_weight" = w[1:n]
#         ),
#         "log_evidence" = z
#       )
#     },
#     id = sample_idx,
#     v = asplit(log_volume, 1),
#     w = asplit(log_weight, 1),
#     z = log_evidence,
#     SIMPLIFY = FALSE
#   )
#
# summarize_resample <- function(samples, resampled) {
#   lapply(
#     seq_along(resampled$idx),
#     \(i) {
#       n <- length(resampled$idx[[i]])
#       slice <- samples[resampled$idx[[i]], ]
#       w <- exp(resampled$log_imp_weight[i, 1:n])
#       data_frame0(
#         "variable" = c("log_evidence", colnames(samples)),
#         "mean" = c(
#           resampled$log_evidence[[i]],
#           matrixStats::colWeightedMeans(slice, w, useNames = FALSE)
#         ),
#         "sd" = c(
#           NA,
#           matrixStats::colWeightedSds(slice, w * n, useNames = FALSE)
#         )
#       )
#     }
#   )
# }
