#' Test that a proposal from an LRPS object is valid
#'
#' @param lrps An LRPS object.
#' @param original Optional starting point.
#' @param criterion Likelihood threshold.
#' @param extra_args Extra expected names in result.
#' @param allow_failure Allow proposal to fail.
expect_proposal <- function(
  lrps,
  original = NULL,
  criterion = -Inf,
  extra_args = NULL,
  allow_failure = FALSE
) {
  stopifnot(inherits(lrps, "ernest_lrps"))
  orig_ncall <- env_get(lrps$cache, "neval")
  res <- propose(lrps, original = original, criterion = criterion)
  expected_names <- c("unit", "log_lik", "neval", extra_args)
  expected_calls <- orig_ncall + (if (!is.null(original)) res$neval else 0L)
  if (is.null(res$unit) && allow_failure) {
    if (!identical(res$neval, lrps$max_loop)) {
      fail("Proposal failed but neval does not match max_loop.")
    } else {
      pass()
    }
    return(invisible(res))
  }
  if (!setequal(names(res), expected_names)) {
    fail(c(
      "Result names do not match expected.",
      sprintf("Actual: %s", paste(sort(names(res)), collapse = ", ")),
      sprintf("Expected: %s", paste(sort(expected_names), collapse = ", "))
    ))
  } else if (!is.double(res$unit) || length(res$unit) != lrps$n_dim) {
    fail(
      "res$unit (%g) is not a double vector of expected size (%g).",
      length(res$unit),
      lrps$n_dim
    )
  } else if (criterion != Inf && res$log_lik < criterion) {
    fail(
      "log_lik (%.3f) is less than criterion (%.3f).",
      res$log_lik,
      criterion
    )
  } else if (!identical(lrps$cache$neval, expected_calls)) {
    fail(
      "cache$neval (%g) does not match expected_calls (%g).",
      lrps$cache$neval,
      expected_calls
    )
  } else {
    pass()
  }
  invisible(res)
}

#' Test that all proposal modes of an LRPS object are valid
#'
#' @param expr Expression to construct LRPS.
#' @param unit_log_fn Log-likelihood function.
#' @param n_dim Number of dimensions.
#' @param max_loop Max attempts.
#' @param ... Additional arguments.
#' @param extra_args Extra expected names in result.
#' @param allow_failure Allow proposal to fail when log_lik(x) < criterion.
expect_all_proposals <- function(
  expr,
  unit_log_fn,
  n_dim,
  max_loop = 1e3L,
  ...,
  extra_args = NULL,
  allow_failure = FALSE
) {
  original <- rep(0.5, n_dim)
  lrps <- exec(
    expr,
    unit_log_fn = unit_log_fn,
    n_dim = n_dim,
    max_loop = max_loop,
    ...
  )
  criterion <- unit_log_fn(original)

  expect_proposal(lrps, original = NULL, criterion = criterion)
  expect_proposal(
    lrps,
    original = original,
    criterion = criterion,
    extra_args = extra_args
  )
  expect_proposal(
    lrps,
    original = original,
    criterion = Inf,
    allow_failure = allow_failure,
    extra_args = extra_args
  )
  invisible(lrps)
}

#' Test that an object is a valid LRPS of a given subclass
#'
#' @param object The object to check.
#' @param subclass Expected subclass.
#' @param ... Additional cache names and prototypes.
expect_lrps <- function(object, subclass = NULL, ...) {
  act <- quasi_label(rlang::enquo(object))

  if (!inherits_only(act$val, c(subclass, "ernest_lrps"))) {
    fail(sprintf(
      "%s is not an `ernest_lrps` with subclass %s.\nActual class: %s",
      act$lab,
      paste(subclass %||% "NULL", collapse = ", "),
      paste(class(act$val), collapse = ", ")
    ))
  }
  if (!is_function(act$val$unit_log_fn)) {
    fail(sprintf("%s$unit_log_fn is not a function.", act$lab))
  }
  n_dim <- act$val$n_dim
  if (!is.null(n_dim) && !(is_scalar_integer(n_dim) && n_dim > 0)) {
    testthat::fail(sprintf(
      "(%s): `n_dim` must be a positive integer, not %d",
      act$lab,
      n_dim
    ))
  }
  if (!is_scalar_integer(act$val$max_loop) || act$val$max_loop <= 0) {
    fail(sprintf(
      "%s$max_loop must be a positive integer, not %s",
      act$lab,
      act$val$max_loop
    ))
  }
  if (!is_environment(act$val$cache)) {
    fail(sprintf("%s$cache must be an environment.", act$lab))
  }
  cache_list <- as.list(act$val$cache)
  ptypes <- list2(neval = 0L, ...)
  ptypes <- lapply(ptypes, vctrs::vec_ptype)
  expect_named(cache_list, names(ptypes), ignore.order = TRUE, label = "cache")
  for (nm in names(ptypes)) {
    if (!vctrs::vec_is(cache_list[[nm]], ptypes[[nm]])) {
      fail(sprintf("`cache` at %s has unexpected types.", nm))
    }
  }
  pass()
  invisible(act$val)
}

#' Test that update_lrps is idempotent for an LRPS object
#'
#' @param lrps An LRPS object.
#' @param subclass Expected subclass.
#' @param reset Cache keys to reset.
#' @param ... Additional cache names.
expect_idempotent_update <- function(
  lrps,
  subclass = NULL,
  reset = NULL,
  ptypes = NULL
) {
  cache_list <- as.list(lrps$cache)
  reset <- c("neval", reset)
  cache_list[names(cache_list) %in% reset] <- 0L
  new_lrps <- update_lrps(lrps)
  expect_lrps(new_lrps, subclass, !!!ptypes)
  expect_mapequal(as.list(new_lrps$cache), cache_list)
  invisible(new_lrps)
}

#' Run the sampler over a set of points
#'
#' @param lrps An LRPS object.
#' @param points Matrix of points.
#' @param restrict_prior Restrict to prior.
run_sampler <- function(lrps, points = NULL, restrict_prior = TRUE) {
  points <- points %||%
    matrix(stats::runif(300 * lrps$n_dim), ncol = lrps$n_dim)
  res <- apply(
    points,
    1,
    \(x) {
      propose(
        lrps,
        x,
        criterion = if (restrict_prior) lrps$unit_log_fn(x) else -Inf
      )
    }
  )
  keys <- names(res[[1]])
  res <- setNames(lapply(keys, function(k) lapply(res, `[[`, k)), keys)
  res$unit <- do.call(rbind, res$unit)
  for (k in setdiff(keys, "unit")) {
    res[[k]] <- unlist(res[[k]])
  }
  res
}

#' Helper to plot black and red points for visual tests
#'
#' @param black Matrix of black points.
#' @param red Matrix of red points.
test_plot <- function(black, red) {
  function() {
    plot(black)
    points(red, col = "red")
  }
}
