#' Extract the posterior sample weights from a nested sampling run
#'
#' Return the normalised posterior importance weights for the dead points in a
#' nested sampling run.
#'
#' @param object [[ernest_run]]\cr A nested sampling run.
#' @param log `[logical(1)]`\cr Whether to return the weights on the log
#' scale.
#' @inheritParams rlang::args_dots_empty
#'
#' @returns `[double()]` A numeric vector of normalised importance weights. When
#' `log = FALSE`, the values are exponentiated so they sum to one.
#'
#' @details
#' The log-weights in a nested sampling run are the individual contributions
#' of each sample to the log-evidence estimate. The unnormalised weight of the
#' \eqn{i}th sampled point is given as
#' \deqn{w_i = \frac{L_{i-1} + L_i}{2} * (V_{i-1} - V_i)}
#' where \eqn{L_i} is the likelihood value for the point and \eqn{V_i} is the
#' prior volume at which the point was sampled.
#'
#' The posterior importance weights are obtained by normalising the log-weights
#' with the final log-evidence estimate. They can be used to reweight posterior
#' samples from the run so they approximate the posterior distribution.
#'
#' @seealso [as_draws.ernest_run]
#'
#' @examples
#' data(example_run)
#' weights(example_run) |> head()
#' weights(example_run, log = TRUE) |> head()
#' @importFrom stats weights
#' @export
weights.ernest_run <- function(object, log = FALSE, ...) {
  check_dots_empty()
  weights.ernest_rcrd(object$rcrd, log = log)
}

#' @noRd
#' @export
weights.ernest_rcrd <- function(object, log = FALSE, ...) {
  check_dots_empty()
  check_bool(log)
  log_w <- try_fetch(
    compute_integral(object, truncate = TRUE),
    warning = function(cnd) abort("Can't estimate weights.", parent = cnd)
  )
  weights <- drop(log_w$log_weight - log_w$log_evidence)
  if (log) {
    weights
  } else {
    exp(weights)
  }
}

#' Effective sample size of a nested sampling run
#'
#' Uses Kish's formula to estimate the effective sample size of a nested
#' sampling run from its posterior importance weights.
#'
#' @param x A nested sampling rcrd or run.
#' @returns `[double(1)]` The effective sample size of the run.
#' @noRd
run_ess <- function(x) {
  w <- weights(x, log = FALSE)
  sum(w)^2 / sum(w^2)
}
