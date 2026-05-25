#' Storing nested sampling results
#'
#' `ernest_rcrd` is a [[vctrs::new_rcrd]] class that stores metadata for all
#' samples drawn from the nested sampling algorithm, tracking the history of
#' replacements in the live set and each point's contribution to evidence
#' estimation.
#'
#' @param unit `[matrix()]`\cr Coordinates in the unit hypercube, with one row
#' per sample and one column per variable.
#' @param log_lik `[double()]`\cr Log-likelihood values for each sample.
#' @param id `[character()]`\cr Unique identifier for each point within a
#' nested sampler.
#' @param nlive `[integer()]`\cr The number of live points in the run at the
#' time each sample was generated.
#' @param neval `[integer()]`\cr The number of likelihood evaluations performed
#' to generate each sample.
#' @param birth_lik `[double()]`\cr The log-likelihood threshold at which
#' each sample was generated, i.e. the log-likelihood of the worst live point
#' at the time of replacement.
#'
#' @returns An `ernest_rcrd` object, which is a vctrs record class designed to
#' store the history of samples generated during a nested sampling run.
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
ernest_rcrd <- function(
  unit = matrix(double(0)),
  log_lik = double(0),
  id = character(0),
  nlive = integer(0),
  neval = integer(0),
  birth_lik = double(0)
) {
  nvar <- ncol(unit) %||% 0L
  unit <- vec_cast(unit, to = matrix(double(), ncol = nvar))
  log_lik <- vec_cast(log_lik, to = double())
  id <- vec_cast(id, to = character())
  nlive <- vec_cast(nlive, to = integer())
  neval <- vec_cast(neval, to = integer())
  birth_lik <- vec_cast(birth_lik, to = double())
  params <- vctrs::vec_recycle_common(
    unit,
    log_lik,
    id,
    nlive,
    neval,
    birth_lik,
    .size = nrow(unit)
  )
  inject(new_ernest_rcrd(!!!params, .nvar = nvar))
}

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
#' @param .nvar The number of variables in the problem, used to validate the
#' `unit` matrix.
#'
#' @returns An `ernest_rcrd` object (a vctrs record class).
#' @importFrom vctrs vec_cast vec_ptype_full vec_ptype2
#' @noRd
new_ernest_rcrd <- function(
  unit = matrix(double(0)),
  log_lik = double(0),
  id = character(0),
  nlive = integer(0),
  neval = integer(0),
  birth_lik = double(0),
  .nvar = 0L
) {
  vctrs::vec_assert(unit, matrix(double(), ncol = .nvar))
  vctrs::vec_assert(log_lik, double())
  vctrs::vec_assert(id, character())
  vctrs::vec_assert(nlive, integer())
  vctrs::vec_assert(neval, integer())
  vctrs::vec_assert(birth_lik, double())
  if (vctrs::vec_any_missing(id) || any(id == "")) {
    cli::cli_abort("`id` cannot contain missing or empty values.")
  }
  if (any(nlive < 0L)) {
    cli::cli_abort("`nlive` must be a non-negative integer.")
  }
  if (any(neval < 0L)) {
    cli::cli_abort("`neval` must be a non-negative integer.")
  }

  vctrs::new_rcrd(
    vctrs::df_list(
      unit = unit,
      log_lik = log_lik,
      id = id,
      nlive = nlive,
      neval = neval,
      birth_lik = birth_lik
    ),
    nvar = as.integer(.nvar),
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
  ernest_rcrd(unit = matrix(double(0), ncol = attr(x, "nvar")))
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
  vec_cast(x, to = ernest_rcrd())
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
  if (is.unsorted(x)) {
    cli::cli_warn(
      "`{arg}` should be sorted in ascending order of log-likelihood.",
      call = call
    )
    return(FALSE)
  }
  TRUE
}

#' Extract the live or dead points as a list
#'
#' @param x An `ernest_rcrd` object containing the run history.
#' @param nlive The number of live points in the run. If NULL, this is inferred
#' from the maximum `nlive` value in the record.
#'
#' @returns A list with the elements `unit`, `log_lik`, `birth_lik`, `id`,
#' for the live points.
#' @noRd
get_live_set <- function(x, nlive = NULL) {
  nlive <- nlive %||% max(field(x, "nlive"))
  idx_loc <- vctrs::vec_group_loc(field(x, "id"))$loc
  if ((n <- vctrs::vec_size(idx_loc)) != nlive) {
    cli::cli_warn(
      "Expected {nlive} unique IDs in `{caller_arg(x)}`, but found {n}."
    )
  }
  live_idx <- vapply(idx_loc, \(loc) loc[[length(loc)]], integer(1))
  list(
    unit = field(x, "unit")[live_idx, , drop = FALSE],
    log_lik = field(x, "log_lik")[live_idx],
    birth_lik = field(x, "birth_lik")[live_idx],
    id = field(x, "id")[live_idx]
  )
}

#' Extract the indexes of the dead points from a previous run
#'
#' @param x An `ernest_rcrd` object containing the run history.
#' @param nlive The number of live points in the run. If NULL, this
#' is inferred from the maximum `nlive` value in the record.
#' @return An integer vector of indexes corresponding to the dead points in the
#' run.
#' @noRd
get_dead_idx <- function(x, nlive = NULL) {
  nlive <- nlive %||% max(field(x, "nlive"))
  idx_loc <- vctrs::vec_group_loc(field(x, "id"))$loc
  if ((n <- vctrs::vec_size(idx_loc)) != nlive) {
    cli::cli_warn(
      "Expected {nlive} unique IDs in `{caller_arg(x)}`, but found {n}."
    )
  }
  vctrs::vec_c(!!!lapply(idx_loc, \(loc) loc[-length(loc)]), integer()) |>
    sort()
}

#' Extract the live set from a nested sampling environment
#'
#' @param live_env The environment containing the live points.
#'
#' @returns A vctrs record class object containing the live points.
#' @noRd
env_to_rcrd <- function(live_env) {
  live_set <- env_get_list(
    live_env,
    nms = c("unit", "log_lik", "birth_lik", "id")
  )
  order_lik <- order(live_set$log_lik)
  ernest_rcrd(
    unit = live_set$unit[order_lik, , drop = FALSE],
    log_lik = live_set$log_lik[order_lik],
    id = live_set$id[order_lik],
    nlive = rev(seq_along(live_set$log_lik)),
    neval = rep(0L, vctrs::vec_size(live_set$unit)),
    birth_lik = live_set$birth_lik[order_lik]
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
  niter <- length(x) - nlive
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
