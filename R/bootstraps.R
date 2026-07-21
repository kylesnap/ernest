#' Bootstrap resample a nested sampling run
#'
#' Resamples the run's live-point threads with replacement to generate
#' synthetic runs and estimate uncertainty due to how the nested sampling
#' algorithm estimates contours within the likelihood surface.
#'
#' @param x [[ernest_run]]\cr Results from a nested sampling run.
#' @param times `[[integer(1)]]`\cr The number of bootstrap resamples to draw.
#' @param draws `[[character(1)]]`\cr Whether to return a
#' [posterior::draws_matrix] of the weighted posterior samples for each
#' resampled run.
#' * `"none"`: No samples are returned.
#' * `"original"`: Samples are returned and expressed on the original scale of
#' the parameter space.
#' * `"unit_cube"`: Samples are returned and expressed in the scale of the
#' (0, 1) unit hypercube.
#' @param apparent `[[logical(1)]]`\cr Whether to include the original run as an
#' additional resample.
#' @param in_parallel `[[logical(1)]]`\cr `r lifecycle::badge("experimental")`
#' Whether to run bootstrapping across workers set with [mirai::daemons()].
#'
#' @returns A tibble with one row per resample and these columns:
#' * `id`: the resample identifier.
#' * `split`: the sampled thread IDs used to build the resample.
#' * The columns from [glance.ernest_run()], summarising the resampled run.
#' * `draws`: If `draws != "none"`, the draws from the resampled run as a
#' [posterior::draws_matrix()] object, with a hidden `.weights` column
#' containing their importance weights.
#'
#' If `apparent = TRUE`, an additional `Apparent` row is included summarising
#' the original run.
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
  draws = c("none", "original", "unit_cube"),
  apparent = FALSE,
  in_parallel = FALSE
) {
  check_number_whole(times, min = 0)
  draws <- arg_match(draws)
  check_bool(in_parallel)
  if (in_parallel) {
    check_parallel_libs()
    mirai::require_daemons()
  }

  x_rcrd <- x$rcrd
  if (draws == "original") {
    vctrs::field(x_rcrd, "unit") <- x$prior$fn(field(x_rcrd, "unit"))
  }

  # Get resampled indicies
  threads <- vctrs::vec_group_loc(field(x_rcrd, "id"))
  nthreads <- vctrs::vec_size(threads)
  sample_ids <- replicate(
    times,
    sample.int(nthreads, replace = TRUE),
    simplify = FALSE
  )

  # Get glance summaries for each resample
  glances <- if (in_parallel) {
    get_bs_glances_mirai(
      x_rcrd,
      threads,
      sample_ids,
      x$nlive,
      draws = draws != "none"
    )
  } else {
    get_bs_glances(
      x_rcrd,
      threads,
      sample_ids,
      x$nlive,
      draws = draws != "none"
    )
  }
  glance_df <- vctrs::vec_rbind(!!!lapply(glances, `[[`, "glance"))
  draws_lst <- if (draws != "none") {
    var_names <- posterior::variables(as_draws(x))
    lapply(glances, \(g) {
      colnames(g$draws)[seq_along(var_names)] <- var_names
      g$draws
    })
  }

  sample_ids <- lapply(
    sample_ids,
    \(x) vctrs::vec_unrep(sort.int(x))
  )
  id <- paste0("Bootstrap", seq_along(sample_ids))

  if (apparent) {
    id <- c(id, "Apparent")
    sample_ids <- c(sample_ids, list(vctrs::vec_unrep(threads$key)))
    glance_df <- vctrs::vec_rbind(glance_df, glance(x_rcrd))
    draws_lst <- if (draws != "none") {
      c(draws_lst, list(as_draws_matrix(x, units = draws)))
    } else {
      NULL
    }
  }
  new_tibble0(
    data_frame0(
      "id" = id,
      "split" = sample_ids,
      !!!glance_df,
      "draws" = if (draws != "none") draws_lst else NULL
    )
  )
}

#' Create a series of resampled runs by resampling the threads of a run.
#'
#' @param x_rcrd The run record of a nested sampling run, containing the history
#' of the live points.
#' @param threads A dataframe with three elements:
#' * key: The ID of the point.
#' * loc: The location of each point death for the ID.
#' @param sample_ids A list of integer vectors, each containing indices of
#' threads to include in a resampled run.
#' @param nlive The number of live points in the original run, used to determine
#' the number of threads and the structure of the resampled runs.
#' @param draws If `TRUE`, the resampled runs will include draws.
#'
#' @return A list of resampled runs, each containing the points from the threads
#' identified in `sample_ids`.
#' @noRd
get_bs_glances_mirai <- function(
  x_rcrd,
  threads,
  sample_ids,
  nlive,
  draws = FALSE
) {
  m <- mirai::mirai_map(
    sample_ids,
    \(ids) {
      unique_ids <- vctrs::vec_as_names(
        threads_$key[ids],
        repair = "unique_quiet"
      )
      unique_ids <- vctrs::vec_rep_each(
        unique_ids,
        vctrs::list_sizes(vctrs::vec_slice(threads_$loc, ids))
      )
      locs <- vec_c(
        !!!vctrs::vec_slice(threads_$loc, ids),
        to = integer()
      )
      loc_order <- order(locs)
      idx <- data_frame0(
        new_id = unique_ids[loc_order],
        loc = locs[loc_order]
      )

      sampled <- vctrs::vec_slice(x_rcrd_, idx$loc)
      vctrs::field(sampled, "id") <- idx$new_id
      new_rcrd <- compile_rcrd_(sampled)
      draws <- if (draws_) {
        posterior::weight_draws(
          posterior::as_draws_matrix(field(new_rcrd, "unit")),
          weights = weights_(new_rcrd, log = TRUE),
          log = TRUE
        )
      } else {
        NULL
      }
      list("glance" = glance_(new_rcrd), "draws" = draws)
    },
    x_rcrd_ = x_rcrd,
    threads_ = threads,
    draws_ = draws,
    compile_rcrd_ = compile_rcrd,
    glance_ = glance,
    weights_ = weights
  )
  mirai::collect_mirai(m, options = ".stop")
}

#' Create a series of resampled runs by resampling the threads of a run.
#'
#' @param x_rcrd The run record of a nested sampling run, containing the history
#' of the live points.
#' @param threads A dataframe with three elements:
#' * key: The ID of the point.
#' * loc: The location of each point death for the ID.
#' @param sample_ids A list of integer vectors, each containing indices of
#' threads to include in a resampled run.
#' @param nlive The number of live points in the original run, used to determine
#' the number of threads and the structure of the resampled runs.
#' @param draws If `TRUE`, the resampled runs will include draws.
#'
#' @return A list of resampled runs, each containing the points from the threads
#' identified in `sample_ids`.
#' @noRd
get_bs_glances <- function(
  x_rcrd,
  threads,
  sample_ids,
  nlive,
  draws = FALSE
) {
  # Get the unique IDs and indices for each resample
  sample_idx <- lapply(
    sample_ids,
    \(ids) {
      unique_ids <- vctrs::vec_as_names(
        threads$key[ids],
        repair = "unique_quiet"
      )
      unique_ids <- vctrs::vec_rep_each(
        unique_ids,
        vctrs::list_sizes(vctrs::vec_slice(threads$loc, ids))
      )
      locs <- vec_c(
        !!!vctrs::vec_slice(threads$loc, ids),
        to = integer()
      )
      loc_order <- order(locs)
      data_frame0(
        new_id = unique_ids[loc_order],
        loc = locs[loc_order]
      )
    }
  )

  lapply(
    sample_idx,
    \(idx) {
      sampled <- vctrs::vec_slice(x_rcrd, idx$loc)
      vctrs::field(sampled, "id") <- idx$new_id
      new_rcrd <- compile_rcrd(sampled)
      draws <- if (draws) {
        posterior::weight_draws(
          posterior::as_draws_matrix(field(new_rcrd, "unit")),
          weights = weights(new_rcrd, log = TRUE),
          log = TRUE
        )
      } else {
        NULL
      }
      list("glance" = glance(new_rcrd), "draws" = draws)
    }
  )
}
