#' Generate nested sampling runs in parallel.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The cost of a nested sampling run depends on the distance between the prior
#' and posterior distributions and on the number of live points. With
#' `allow_par = TRUE`, [generate()] uses \CRANpkg{mirai} to split the live set
#' across workers, run sampling in parallel, then merge results before
#' estimating model evidence.
#'
#' To run in parallel, daemons must be set with [mirai::daemons()]. Otherwise,
#' [mirai::require_daemons()] throws an error when `allow_par = TRUE`.
#'
#' User-supplied [ernest_likelihood] and [ernest_prior] functions must be
#' self-contained and must not depend on objects in the global environment.
#' This keeps serialization predictable and avoids sending large, accidental
#' dependencies to workers.
#'
#' @section Creating self-contained functions:
#'
#' Self-contained likelihood and prior functions should follow these
#' guidelines:
#'
#' 1. Call package functions with explicit `::` namespaces, e.g.
#' `extraDistr::qtnorm()`. Alternatively, call `library()` inside the function
#' if you need to attach a package.
#'
#' 2. Declare all data dependencies explicitly, similar to the pattern used in
#' [create_likelihood].
#'
#' 3. Any helper functions (closures) called within a self-contained function
#' must itself be self-contained.
#'
#' @section Setting daemons:
#'
#' How and where parallelisation occurs is determined by [mirai::daemons()].
#' Daemons are persistent background processes that execute parallel
#' computations locally or across a network.
#'
#' Daemons must be set before parallel execution. Otherwise, calling
#' `generate(..., allow_par = TRUE)` throws an error.
#'
#' Usually, daemons are set once per session and can be left running while idle
#' because they use minimal resources. The following sets up 6 local daemons:
#'
#' ```r
#' mirai::daemons(6)
#' ```
#'
#' Function arguments:
#'
#' * `n`: the number of daemons to launch on your local machine, e.g.
#'   `mirai::daemons(6)`. As a rule of thumb, for maximum efficiency this should
#'   be (at most) one less than the number of cores on your machine, leaving one
#'   core for the main R process.
#' * `url` and `remote`: used to set up and launch daemons for distributed
#'   computing over the network. See [mirai::daemons()] for more details.
#'
#' Daemons persist for the duration of your session. To reset and shut them
#' down:
#'
#' ```r
#' mirai::daemons(0)
#' ```
#'
#' All daemons automatically terminate when your session ends. You do not need
#' to explicitly terminate daemons in this instance, although it is still good
#' practice to do so.
#'
#' @references
#' ernest's parallelisation is powered by \CRANpkg{mirai}. See the
#' [mirai website](https://mirai.r-lib.org/) for more details.
#'
#' \CRANpkg{crate} provides a simple method for creating self-contained
#' functions. Consult that package for more details.
#'
#' This documentation is based on the
#' [in_parallel](https://purrr.tidyverse.org/reference/in_parallel.html)
#' function from the \pkg{purrr} package.
#'
#' @seealso [generate()] for performing nested sampling runs.
#'
#' @examples
#' prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#' ll_fn <- function(x) -sum(x^2)
#' sampler <- ernest_sampler(ll_fn, prior, nlive = 300)
#' sampler
#'
#' # Initialise daemons
#' mirai::daemons(1, dispatcher = FALSE)
#'
#' # Automatically partition a run based on the number of daemons
#' generate(sampler, max_iterations = 100, allow_par = TRUE)
#'
#' # Stop daemons
#' mirai::daemons(0)
#' Sys.sleep(1)
#' @name ernest-parallel
#' @aliases parallelization
NULL

#' Parallel generate
#'
#' @param x The ernest_sampler or ernest_run object.
#' @param sampler_info A list containing information about the sampler.
#' @param control parameters for the nested sampling run, generated from
#' `set_run_control()`.
#' @param show_progress Logical. If `TRUE`, displays a progress bar during
#' sampling.
#'
#' @noRd
p_generate <- function(
  x,
  parent_info,
  parent_control,
  show_progress,
  call = caller_env()
) {
  check_installed("mirai", "for parallel nested sampling.", call = call)
  mirai::require_daemons(call = call)
  nworkers <- mirai::info()[["connections"]]
  split_id <- thread_nlive(x, nworkers, call)
  split_x <- split_run(x, split_id, parent_control, parent_info)

  # Load ernest namespace persistently on all worker daemons
  mirai::everywhere(library(ernest))
  load_check <- mirai::mirai("package:ernest" %in% search())
  if (!load_check[]) {
    cli::cli_abort(
      "{.pkg ernest} couldn't be loaded on some daemons.",
      "i" = "Is the latest version of {.pkg ernest} installed?",
      call = call
    )
  }

  m <- mirai::mirai_map(
    split_x,
    \(sx) {
      cur_env <- list2env(sx[c("unit", "log_lik", "birth_lik")])
      dead <- impl_(
        live_env = cur_env,
        lrps = lrps_,
        sampler_info = sx$info,
        control = sx$control,
        show_progress = FALSE
      )
      list("dead" = dead, "live" = as.list(cur_env))
    },
    impl_ = nested_sampling_impl,
    lrps_ = x$lrps
  )

  opts <- c(".stop", if (show_progress) ".progress" else NULL)
  m_out <- mirai::collect_mirai(m, options = opts)
  result <- reindex_threads(m_out, split_id)
  env_bind(
    x$live_env,
    !!!as.list(result$live)[c("unit", "log_lik", "birth_lik")]
  )
  new_ernest_run(x, result$dead)
}

#' Get the nlive for each worker.
#'
#' @param nlive The total nlive of the sampler.
#' @param workers An integer vector or NULL. The length of the vector is the
#' number of workers, the element is the nlive of each sub-sampler.
#' @param call Error info.
#'
#' @returns A validated integer vector, whose length is the number of
#' runs and each element is that run's nlive.
#' @noRd
thread_nlive <- function(x, nworkers, call = caller_env()) {
  nworkers <- min(x$nlive, nworkers)
  workers <- as.integer(pmax(1L, rep(x$nlive %/% nworkers, nworkers)))
  nlive_workers <- sum(workers)
  workers[[1]] <- workers[[1]] + (x$nlive - nlive_workers)
  preserve_seed(attr(x, "seed"))
  ids <- sample.int(x$nlive, size = x$nlive)
  vctrs::vec_chop(ids, sizes = workers)
}

#' Split the live set into a series of threads.
#'
#' @param x The sampler.
#' @param slices How the sampler is to be split.
#'
#' @returns Named list of IDs by worker and environments.
#' @noRd
split_run <- function(x, slices, parent_control, parent_info) {
  x_rcrd <- if (inherits_only(x, "ernest_sampler")) {
    NULL
  } else {
    x$rcrd
  }

  lapply(slices, \(slice) {
    list(
      "unit" = env_get(x$live_env, "unit")[slice, , drop = FALSE],
      "log_lik" = env_get(x$live_env, "log_lik")[slice],
      "birth_lik" = env_get(x$live_env, "birth_lik")[slice],
      "info" = split_info(length(slice), parent_info),
      "control" = new_generate_control(
        parent_control$max_iterations,
        parent_control$max_evaluations,
        parent_control$min_logz,
        prev_run = if (!is.null(x_rcrd)) {
          vctrs::vec_slice(x_rcrd, field(x_rcrd, "id") %in% slice)
        },
        call = call
      )
    )
  })
}

#' Scale sampler control parameters for parallel sub-samplers.
#'
#' @param split_nlive The number of live points for this worker's sub-sampler.
#' @param parent_info A list containing parent sampler information.
#'
#' @returns A list containing scaled sampler info for the worker. Both
#' `first_update` and `update_interval` are scaled by the ratio of the
#' worker's `split_nlive` to the parent's total `nlive`.
#'
#' @noRd
split_info <- function(split_nlive, parent_info) {
  frac_nlive <- split_nlive / parent_info$nlive
  list(
    seed = parent_info$seed,
    nlive = split_nlive,
    first_update = as.integer(parent_info$first_update * frac_nlive),
    update_interval = as.integer(parent_info$update_interval * frac_nlive)
  )
}

#' Reindex parallel sampling results and merge across workers.
#'
#' @param results A list of results from `mirai_map()`, each containing
#'   `$dead` (ernest records) and `$live` (list of live point data).
#' @param splits A list of index mappings, where each element is a vector
#'   of global indices corresponding to that worker's local indices.
#'
#' @returns A properly indexed and merged result ernest_rcrd combining all
#' workers' dead and live points, with global IDs correctly assigned.
#' @noRd
reindex_threads <- function(results, splits) {
  result <- mapply(
    \(res, split) {
      live <- extract_live_points(res$live, .id = split)
      vctrs::field(res$dead, "id") <- split[field(res$dead, "id")]
      vctrs::field(res$dead, "id") <- split[field(res$dead, "id")]
      vctrs::vec_c(res$dead, live)
    },
    res = results,
    split = splits
  )
  merge_results(vctrs::vec_c(!!!result))
}
