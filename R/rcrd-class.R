#' vctrs::new_rcrd() object representing samples from nested sampling.
#'
#' @param unit The parameter values in the unit cube.
#' @param log_lik The log-likelihood values for each sample.
#' @param nlive The number of live points at the time each sample was taken.
#' @param id A unique identifier for each sample.
#' @param neval The number of likelihood evaluations for each sample.
#' @param birth_lik The log-likelihood value at which each sample was born.
#'
#' @returns A vctrs record class object containing the sample information.
#' @importFrom vctrs vec_cast vec_ptype_full vec_ptype2
#' @noRd
new_ernest_rcrd <- function(
  unit = matrix(double(0)),
  log_lik = double(0),
  id = integer(0),
  nlive = integer(0),
  neval = integer(0),
  birth_lik = double(0),
  .call = caller_env(0)
) {
  nvar <- ncol(unit)
  vctrs::new_rcrd(
    list(
      unit = vec_cast(
        unit,
        to = matrix(double(), ncol = nvar),
        call = .call
      ),
      log_lik = vec_cast(log_lik, to = double(), call = .call),
      id = vec_cast(id, to = integer(), call = .call),
      nlive = vec_cast(nlive, to = integer(), call = .call),
      neval = vec_cast(neval, to = integer(), call = .call),
      birth_lik = vec_cast(birth_lik, to = double(), call = .call)
    ),
    nvar = as.integer(nvar),
    class = "ernest_rcrd"
  )
}

#' @importFrom vctrs vec_ptype_full vec_ptype_abbr
#' @export
#' @noRd
vec_ptype_full.ernest_rcrd <- function(x, ...) "nested sampling record"

#' @export
#' @noRd
vec_ptype_abbr.ernest_rcrd <- function(x, ...) "ernest_rcrd"

#' @export
#' @noRd
format.ernest_rcrd <- function(x, ...) {
  log_lik <- field(x, "log_lik")
  birth_lik <- field(x, "birth_lik")

  out <- sprintf("%3g \U2192 %3g", birth_lik, log_lik)
  out[is.na(log_lik) | is.na(birth_lik)] <- NA_character_
  out
}

#' @export
#' @noRd
vec_ptype2.ernest_rcrd.ernest_rcrd <- function(x, y, ..., call = caller_env()) {
  if (!isTRUE(all.equal(attr(x, "nvar"), attr(y, "nvar")))) {
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
    nlive = rev(seq_along(live_env$log_lik)),
    neval = rep(0L, vctrs::vec_size(live_env$unit)),
    birth_lik = live_env$birth_lik[order_lik]
  )
}
