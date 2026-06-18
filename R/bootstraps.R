#' Bootstrap resample a nested sampling run
#'
#' Resamples the run's live-point threads with replacement to generate
#' synthetic runs and estimate uncertainty due to how the nested sampling
#' algorithm estimates contours within the likelihood surface.
#'
#' @param x [[ernest_run]]\cr Results from a nested sampling run.
#' @param times `[[integer(1)]]`\cr The number of bootstrap resamples to draw.
#' @inheritParams as_draws.ernest_run units
#' @param apparent `[[logical(1)]]`\cr Whether to include the original run as an
#' additional resample.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns A tibble with one row per resample and these columns:
#' * `id`: the resample identifier.
#' * `split`: the sampled thread IDs used to build the resample.
#' * `run`: an [ernest_rcrd] object containing the resampled run.
#'
#' If `apparent = TRUE`, an additional `Apparent` row is included for the
#' original run.
#'
#' @details
#' Higson et al. (2019) describe bootstrap resampling for nested sampling.
#' Here, the run is split into `nlive` threads, the threads are sampled with
#' replacement, and the selected points are merged into synthetic runs.
#' This gives an empirical estimate of uncertainty from the stochastic choice
#' of likelihood shells.
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
#' bootstraps(example_run)
#' @export
bootstraps <- function(
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
  threads <- vctrs::vec_group_loc(field(x_rcrd, "id"))
  nthreads <- vctrs::vec_size(threads)
  sample_ids <- replicate(
    times,
    sample.int(nthreads, replace = TRUE),
    simplify = FALSE
  )
  vctrs::field(x_rcrd, "unit") <- if (units == "original") {
    x$prior$fn(field(x_rcrd, "unit"))
  }

  resampled <- resample_runs(x_rcrd, threads, sample_ids, x$nlive)
  sample_ids <- lapply(
    sample_ids,
    \(x) vctrs::vec_unrep(sort.int(x))
  )
  id <- paste0("Bootstrap", seq_along(sample_ids))

  if (apparent) {
    id <- c(id, "Apparent")
    sample_ids <- c(sample_ids, list(seq(x$nlive)))
    resampled <- c(resampled, list(x$rcrd))
  }
  new_tibble0(
    data_frame0(
      "id" = id,
      "split" = sample_ids,
      "run" = resampled
    )
  )
}

#' Create a series of resampled runs by resampling the threads of a run.
#'
#' @param x_rcrd The run record of a nested sampling run, containing the history
#' of the live points.
#' @param threads A dataframe with three elements:
#' * key: The ID of the point
#' * loc: The location of each point death for the ID
#' * max_idx: The maximum IDX (i.e., the death iteration) of each ID.
#' @param sample_ids A list of integer vectors, each containing indices of
#' threads to include in a resampled run.
#' @param nlive The number of live points in the original run, used to determine
#' the number of threads and the structure of the resampled runs.
#'
#' @return A list of resampled runs, each containing the points from the threads
#' identified in `sample_ids`.
#' @noRd
resample_runs <- function(x_rcrd, threads, sample_ids, nlive) {
  # Get the indices for each resample
  sample_idx <- lapply(
    sample_ids,
    \(ids) {
      thread_subset <- vec_c(
        !!!vctrs::vec_slice(threads$loc, ids),
        to = integer()
      )
      sort(thread_subset)
    }
  )

  lapply(
    sample_idx,
    \(idx) {
      thread_subset <- vctrs::vec_slice(x_rcrd, idx)
      compile_rcrd(thread_subset)
    }
  )
}
