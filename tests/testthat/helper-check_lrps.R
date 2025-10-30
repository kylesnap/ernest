#' Checks that an object is a valid subclass of `ernest_lrps`,
#' with the expected structure, names, and types. Used to ensure subclasses
#' conform to the expected interface.
#'
#' @param object The object to check.
#' @param add_names Additional required names in the object.
#' @param cache_names Additional required names in the cache environment.
#' @param cache_types Named character vector of expected types for cache
#' entries.
#' @param allow_empty Logical; if TRUE, allow empty objects.
#' @noRd
check_valid_lrps <- function(
    object,
    add_names = NULL,
    cache_names = NULL,
    cache_types = NULL,
    allow_empty = FALSE) {
  expect_s3_class(object, "ernest_lrps")
  required_names <- c(add_names, "unit_log_fn", "n_dim", "max_loop", "cache")
  expect_named(object, required_names, ignore.order = TRUE)

  size <- if (allow_empty) 1 else NULL
  expect_type(object$unit_log_fn, "closure")
  expect_vector(object$n_dim, integer(), size = size)
  expect_vector(object$max_loop, integer(), size = size)
  expect_type(object$unit_log_fn, "closure")

  expect_type(object$cache, "environment")

  expected_names <- c("n_call", cache_names)
  expected_types <- c("integer", cache_types)
  names(expected_types) <- expected_names

  expect_setequal(env_names(object$cache), expected_names)
  cache_list <- env_get_list(object$cache, expected_names)
  types <- vapply(cache_list, typeof, character(1L))
  expect_equal(types, expected_types)
  invisible(NULL)
}

#' An internal helper for tests. Checks that the `propose` method for an
#' `ernest_lrps` subclass returns valid results and handles edge cases as
#' expected.
#'
#' @param lrps The `ernest_lrps` object.
#' @param unit_log_fn The log-likelihood function.
#' @param fail_on_no_accept Behaviour when criterion can't be reached.
check_propose <- function(lrps, unit_log_fn, fail_on_no_accept = TRUE) {
  withr::local_seed(42)
  check_res <- function(res) {
    expect_type(res, "list")
    expect_contains(names(res), c("unit", "log_lik", "n_call"))
    if (is.null(res$unit)) {
      expect_null(res$log_lik)
      expect_equal(res$n_call, lrps$max_loop)
      return(FALSE)
    }
    expect_vector(res$unit, double(), size = lrps$n_dim)
    expect_equal(res$log_lik, unit_log_fn(res$unit))
    TRUE
  }

  # Call propose with original = NULL
  initial_ncall <- env_cache(lrps$cache, "n_call", 0L)
  res1 <- propose(lrps, original = NULL)
  check_res(res1)
  expect_equal(env_get(lrps$cache, "n_call"), initial_ncall)

  # Call propose with original = some vector
  orig <- rep(0.5, lrps$n_dim)
  orig_log_lik <- lrps$unit_log_fn(orig)
  res2 <- propose(lrps, original = orig, criterion = orig_log_lik + 0.1)
  check_res(res2)
  expect_gte(res2$log_lik + 0.1, orig_log_lik)
  expect_gt(res2$n_call, 0L)
  expect_equal(env_get(lrps$cache, "n_call"), res2$n_call)

  # Error behaviour: criterion greater than max log lik.
  lrps$max_loop <- 1e3
  res3 <- propose(lrps, original = orig, criterion = Inf)
  behaviour <- check_res(res3)
  if (fail_on_no_accept) {
    expect_false(behaviour)
  } else {
    expect_true(behaviour)
    expect_equal(res3$log_lik, orig_log_lik)
    expect_equal(res3$unit, orig)
  }

  # Error behaviour: log_lik(original) < log_lik
  res4 <- propose(lrps, original = orig, criterion = orig_log_lik - 0.1)
  check_res(res4)
}

#' Checks that the `update_lrps` method for an `ernest_lrps` subclass
#' resets and repairs the cache, is idempotent, and works with live points.
#'
#' @param lrps The `ernest_lrps` object.
#' @param add_names Additional required names in the object.
#' @param cache_names Additional required names in the cache environment.
#' @param cache_types Named character vector of expected types for cache
#' entries.
#'
#' @returns A list with old and new live points generated after update.
check_update_lrps <- function(
    lrps,
    add_names = NULL,
    cache_names = NULL,
    cache_types = NULL) {
  withr::local_seed(42)
  # Generate 500 points with the LRPS.
  env_poke(lrps$cache, "n_call", 0L, create = FALSE)
  orig <- rep(0.5, lrps$n_dim)
  orig_log_lik <- lrps$unit_log_fn(orig)
  live <- replicate(
    500,
    propose(lrps, original = orig, criterion = orig_log_lik)
  )
  live_points <- do.call(rbind, live["unit", ])
  live_ncall <- sum(unlist(live["n_call", ]))

  # Check that update resets n_call to 0L.
  expect_equal(lrps$cache$n_call, live_ncall)
  new_lrps <- update_lrps(lrps)
  check_valid_lrps(new_lrps, add_names, cache_names, cache_types)
  expect_equal(new_lrps$cache$n_call, 0L)

  # Check that breaking the cache and updating repairs it.
  env_unbind(new_lrps$cache, c("n_call", cache_names))
  new_lrps <- update_lrps(new_lrps)
  check_valid_lrps(new_lrps, add_names, cache_names, cache_types)

  # Check that update is idempotent.
  newer_lrps <- update_lrps(lrps)
  expect_identical(new_lrps, newer_lrps)

  # Check that update with live doesn't invalidate the LRPS.
  updated_lrps <- update_lrps(lrps, unit = live_points)
  check_valid_lrps(updated_lrps, add_names, cache_names, cache_types)

  # Create 500 more points with the updated LRPS to ensure it still works.
  live <- replicate(
    500,
    propose(updated_lrps, original = orig, criterion = -Inf)
  )
  new_points <- do.call(rbind, live["unit", ])
  list(old = live_points, new = new_points)
}
