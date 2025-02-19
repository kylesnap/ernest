#' Compute evidence integrals
#'
#' @param sampler a ErnestLRPS object.
#'
#' @return The integral, contained in a wrapped `vctrs::rcrd` class.
#'
#' @importFrom utils head
#' @importFrom utils tail
#' @noRd
new_ernest_integral <- function(sampler) {
  dead_lik <- list_c(sampler@wrk$s_log_lik)
  dead_vol <- list_c(sampler@wrk$s_log_vol)
  live_lik <- sort(sampler@wrk$live_lik)
  live_vol <- log1p(
    (-1 - sampler@n_points)^(-1) * seq_len(sampler@n_points)
  ) + sampler@wrk$log_vol

  log_lik <- c(dead_lik, live_lik)
  log_vol <- c(dead_vol, live_vol)

  vctrs::new_rcrd(
    compute_integral(log_lik, log_vol),
    class = "ErnestIntegral"
  )
}

#' Make a new run_rcrd object.
#'
#' @param sampler The ErnestSampler object.
#'
#' @return An ErnestProg object, which contains the run history.
#' @noRd
new_ernest_prog <- function(sampler) {
  indx <- vctrs::vec_as_location(
    list_c(sampler@wrk$idx),
    n = sampler@n_points
  )
  parent <- vctrs::vec_as_location(
    list_c(sampler@wrk$parent),
    n = sampler@n_points
  )
  calls <- vctrs::vec_cast(
    list_c(sampler@wrk$calls),
    integer()
  )
  sampler_indx <- vctrs::vec_cast(
    list_c(sampler@wrk$sampler_iter),
    integer()
  )

  # Attributes carry helper variables
  vctrs::new_rcrd(
    list(
      "indx" = indx,
      "iter" = c(1L:sampler@wrk$tot_it),
      "parent" = parent,
      "calls" = calls,
      "sampler_indx" = sampler_indx
    ),
    time = sampler@wrk$time,
    class = "ErnestProg"
  )
}
