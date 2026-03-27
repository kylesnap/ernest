#' vctrs::new_rcrd() object representing samples from nested sampling.
#'
#' @param unit The parameter values in the unit cube.
#' @param log_lik The log-likelihood values for each sample.
#' @param id A unique identifier for each sample.
#' @param evals The number of likelihood evaluations for each sample.
#' @param birth_lik The log-likelihood value at which each sample was born.
#'
#' @returns A vctrs record class object containing the sample information.
#' @importFrom vctrs vec_cast vec_ptype_full vec_ptype2
#' @noRd
new_ernest_rcrd <- function(
  unit = matrix(double(0)),
  log_lik = double(0),
  id = integer(0),
  evals = integer(0),
  birth_lik = double(0),
  .call = caller_env(0)
) {
  n_dim <- ncol(unit)
  vctrs::new_rcrd(
    list(
      unit = vec_cast(unit, to = matrix(double(), ncol = n_dim), call = .call),
      log_lik = vec_cast(log_lik, to = double(), call = .call),
      id = vec_cast(id, to = integer(), call = .call),
      evals = vec_cast(evals, to = integer(), call = .call),
      birth_lik = vec_cast(birth_lik, to = double(), call = .call)
    ),
    variables = as.character(colnames(unit)),
    class = "ernest_rcrd"
  )
}

#' @export
#' @noRd
vec_ptype2.ernest_rcrd.ernest_rcrd <- function(x, y, ..., call = caller_env()) {
  if (!isTRUE(all.equal(attr(x, "variables"), attr(y, "variables")))) {
    vctrs::stop_incompatible_type(
      x,
      y,
      x_arg = caller_arg(x),
      y_arg = caller_arg(y),
      action = "combine",
      details = "`variables` attribute must match.",
      call = call
    )
  }
  new_ernest_rcrd(unit = vctrs::vec_ptype(field(x, "unit")))
}

#' @export
#' @noRd
vec_ptype2.ernest_rcrd.list <- function(x, y, ...) list()

#' @export
#' @noRd
vec_ptype2.list.ernest_rcrd <- function(x, y, ...) list()

#' @export
#' @noRd
vec_cast.ernest_rcrd.ernest_rcrd <- function(x, to, ...) x

#' @export
#' @noRd
vec_cast.list.ernest_rcrd <- function(x, to, ...) {
  vctrs::df_list(!!!vctrs::vec_data(x))
}

#' @export
#' @noRd
as.list.ernest_rcrd <- function(x, ...) {
  vec_cast(x, to = list(), ...)
}

#' Comparison operations
#' @importFrom vctrs field vec_proxy_compare vec_proxy_order
#' @export
#' @noRd
vec_proxy_compare.ernest_rcrd <- function(x, ...) {
  field(x, "log_lik")
}

#' @export
#' @noRd
vec_proxy_order.ernest_rcrd <- function(x, ...) {
  field(x, "log_lik")
}

#' Extract live points from the run environment.
#'
#' @param live_env The environment containing the live points.
#' @param .id An optional identifier for the live points. If NULL, a default
#' sequence will be used.
#'
#' @returns A vctrs record class object containing the live points.
#' @noRd
extract_live_points <- function(live_env, .id = NULL) {
  .id <- if (is.null(.id)) vctrs::vec_seq_along(live_env$unit) else .id
  order_lik <- order(live_env$log_lik)
  new_ernest_rcrd(
    unit = live_env$unit[order_lik, , drop = FALSE],
    log_lik = live_env$log_lik[order_lik],
    id = .id[order_lik],
    evals = rep(0L, vctrs::vec_size(live_env$unit)),
    birth_lik = live_env$birth_lik[order_lik]
  )
}

#' Extract iterations from an ernest_run object.
#'
#' @param x An ernest_run object.
#' @param keep_live Whether to include the live set in the output.
#'
#' @returns A list containing a subset of the elements from `x`.
#' @importFrom vctrs field
#' @noRd
as_ernest_rcrd <- function(x, keep_live = TRUE) {
  run <- new_ernest_rcrd(
    unit = x$samples$unit_cube,
    log_lik = x$weights$log_lik,
    id = x$weights$id,
    evals = x$weights$evaluations,
    birth_lik = x$weights$birth_lik
  )
  if (keep_live) {
    return(run)
  }
  dead <- vctrs::vec_as_location(field(run, "evals") != 0L, length(run))
  run[dead]
}
