#' Storing nested sampling results
#'
#' `ernest_rcrd` is a [[vctrs::new_rcrd]] class that stores metadata for all
#' samples drawn from the nested sampling algorithm, tracking the history of
#' replacements in the live set and each point's contribution to evidence
#' estimation.
#'
#' @section Fields:
#' \describe{
#' \item{`unit`}{`[double(nvar)]` The sample's coordinates within the unit
#' hypercube (prior to transformation via the prior distribution).}
#' \item{`id`}{`[integer(1)]` A unique identifier for each sample in the
#' sequence of live points.}
#' \item{`nlive`}{`[integer(1)]` The number of effective live points remaining
#' when the sample was removed from the live set. Generally corresponds to the
#' `nlive` of the `ernest_run`, but can be lower if the sample found within a
#' plateau in the log-likelihood function (see References) or if the sample is
#' still in the live set at termination.}
#' \item{`neval`}{`[integer(1)]` The number of likelihood evaluations
#' required by the sampler to generate this replacement point, reflecting the
#' difficulty of sampling from the likelihood-restricted prior. Equal to `0`
#' for points remaining in the live set at termination.}
#' \item{`log_lik`}{`[double(1)]` The log-likelihood at the sample.}
#' \item{`birth_lik`}{`[double(1)]` The log-likelihood threshold that this
#' sample exceeded; the minimum likelihood constraint applied when generating
#' it. Equals `-Inf` for samples in the initial live set.}
#' }
#'
#' @references Fowlie, A., Handley, W., Su, L., Nested Sampling with Plateaus,
#' Monthly Notices of the Royal Astronomical Society, 503(1), 1199–1205,
#' \doi{10.1093/mnras/stab590}
#'
#' @details
#' This object is designed to be used internally within the `ernest_run` class
#' to track the history of samples generated during a nested sampling run.
#' Generally, users will not interact with `ernest_rcrd` objects directly, and
#' instead will call methods on `ernest_run` objects which, internally,
#' manipulate the `ernest_rcrd` object.
#'
#' @name ernest_rcrd
#' @keywords internal
NULL

#' Create a nested sampling record object
#'
#' Constructs an `ernest_rcrd` record from sample metadata generated during a
#' nested sampling run.
#'
#' @param unit A matrix of coordinates in the unit hypercube.
#' @param log_lik A numeric vector of log-likelihood values.
#' @param nlive An integer vector specifying the number of live points.
#' @param id An integer vector of unique identifiers for each sample.
#' @param neval An integer vector of likelihood evaluation counts.
#' @param birth_lik A numeric vector of the log-likelihood thresholds at which
#' each sample was generated
#'
#' @returns An `ernest_rcrd` object (a vctrs record class).
#' @importFrom vctrs vec_cast vec_ptype_full vec_ptype2
#' @noRd
new_ernest_rcrd <- function(
  unit = matrix(double(0)),
  log_lik = double(0),
  id = integer(0),
  nlive = integer(0),
  neval = integer(0),
  birth_lik = double(0)
) {
  nvar <- ncol(unit) %||% 0L
  unit <- vec_cast(unit, to = matrix(double(), ncol = nvar))
  log_lik <- vec_cast(log_lik, to = double())
  id <- vec_cast(id, to = integer())
  if (any(id < 1L)) {
    cli::cli_abort("`id` must be a positive integer.")
  }
  nlive <- vec_cast(nlive, to = integer())
  if (any(nlive < 0L)) {
    cli::cli_abort("`nlive` must be a non-negative integer.")
  }
  neval <- vec_cast(neval, to = integer())
  if (any(neval < 0L)) {
    cli::cli_abort("`neval` must be a non-negative integer.")
  }
  birth_lik <- vec_cast(birth_lik, to = double())
  vctrs::new_rcrd(
    vctrs::df_list(
      unit = unit,
      log_lik = log_lik,
      id = id,
      nlive = nlive,
      neval = neval,
      birth_lik = birth_lik
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
      details = "`nvar` attribute must match.",
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

#' Check that an ernest_rcrd object contains a valid run.
#'
#' Valid runs must contain IDs that are `nlive` contiguous integers starting
#' from `1`. Exactly `nlive` points within the run must have `neval == 0`
#' (the live points at termination). Finally, the run should be sorted in
#' ascending order of log-likelihood.
#'
#' @param x An `ernest_rcrd` object to validate.
#' @param nlive The expected number of live points in the run. If NULL, this
#' is inferred from the maximum `nlive` value.
#' @param arg The argument name to use in error messages.
#' @param call The calling environment to use in error messages.
#'
#' @returns TRUE if the run meets the described conditions, else FALSE.
#' @noRd
rcrd_is_run <- function(
  x,
  nlive = NULL,
  arg = caller_arg(x),
  call = caller_env()
) {
  vec_cast(x, to = new_ernest_rcrd())
  nlive <- nlive %||% max(field(x, "nlive"))
  check_number_whole(nlive, min = 1, arg = caller_arg(nlive), call = call)
  ids <- vctrs::vec_unique(field(x, "id"))
  if (length(ids) != nlive) {
    cli::cli_warn(
      "`{arg}` should contain {nlive} unique IDs, but has {length(ids)}.",
      call = call
    )
    return(FALSE)
  }
  diff <- vctrs::vec_set_symmetric_difference(ids, seq_len(nlive))
  if (length(diff) > 0) {
    cli::cli_warn(
      "`{arg}` should contain IDs from 1 to {nlive}.",
      call = call
    )
    return(FALSE)
  }
  last_id <- vapply(
    vctrs::vec_group_loc(field(x, "id"))$loc,
    \(x) x[[length(x)]],
    integer(1)
  )
  last_id_eval <- field(x, "neval")[last_id]
  if (any(last_id_eval != 0L)) {
    cli::cli_warn(
      "A single live point with `neval == 0` should be found for each ID.",
      call = call
    )
    return(FALSE)
  }
  zero_eval <- sum(field(x, "neval") == 0L)
  if (zero_eval != nlive) {
    cli::cli_warn(
      "Only {nlive} points should have `neval == 0`, but found {zero_eval}.",
      call = call
    )
    return(FALSE)
  }
  if (is.unsorted(x)) {
    cli::cli_warn(
      "`{arg}` should be sorted in ascending order of log-likelihood.",
      call = call
    )
    return(FALSE)
  }
  TRUE
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

#' One-row glance summary for a nested sampling record
#'
#' Returns the same core run diagnostics as [glance.ernest_run()], derived
#' from the stored sample record.
#'
#' @param x An ernest_rcrd object.
#' @param ... Must be empty.
#'
#' @return A one-row `tibble` with `nlive`, `nvar`, `niter`, `neval`,
#' `log_evidence`, `log_evidence_err`, and `information`.
#'
#' @noRd
#' @export
glance.ernest_rcrd <- function(x, ...) {
  check_dots_empty()
  rcrd_is_run(x)
  nlive <- max(field(x, "nlive"))
  nvar <- ncol(field(x, "unit"))
  niter <- sum(field(x, "neval") != 0L)
  neval <- sum(field(x, "neval"))
  integral <- compute_integral(x)
  new_tibble0(
    data_frame0(
      nlive = nlive,
      nvar = nvar,
      niter = niter,
      neval = neval,
      log_evidence = tail(integral$log_evidence, 1L),
      log_evidence_err = sqrt(tail(integral$log_evidence_var, 1L)),
      information = tail(integral$information, 1L)
    )
  )
}
