fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
describe("new_ernest_lrps", {
  it("fails informatively", {
    expect_error(
      new_ernest_lrps(unit_log_fn = 1),
      "`unit_log_fn` must be a function or `NULL`"
    )
    expect_error(
      new_ernest_lrps(fn, n_dim = 0),
      "`n_dim` must be a whole number larger than or equal to 1 or `NULL`"
    )
    expect_error(
      new_ernest_lrps(fn, n_dim = 2, cache = 1),
      "`cache` must be an environment"
    )

    local_options("ernest.max_loop" = 0L)
    expect_error(
      new_ernest_lrps(fn, n_dim = 2),
      "`getOption\\('ernest.max_loop'\\)` must be a whole number"
    )
    local_options("ernest.max_loop" = Inf)
    expect_error(
      new_ernest_lrps(fn, n_dim = 2),
      "`getOption\\('ernest.max_loop'\\)` must be a whole number"
    )
  })

  it("initializes correctly", {
    lrps <- new_ernest_lrps(fn, 2L)
    expect_lrps(lrps)

    local_options("ernest.max_loop" = 100L, "ernest.n_batch" = 100L)
    obj <- new_ernest_lrps(fn, 2L)
    expect_lrps(obj)
    expect_equal(obj$max_loop, 100L)
  })
})

test_that("propose.ernest_lrps can be called", {
  lrps <- new_ernest_lrps(fn, 2L)
  initial_ncall <- env_cache(lrps$cache, "neval", 0L)
  res1 <- propose.ernest_lrps(lrps, original = NULL)
  expect_contains(names(res1), c("unit", "log_lik", "neval"))
  expect_vector(res1$unit, double(), size = lrps$n_dim)
  expect_equal(res1$log_lik, fn(res1$unit))
  expect_equal(env_get(lrps$cache, "neval"), 0L)

  expect_error(
    propose.ernest_lrps(lrps, c(0.5, 0.5), -1),
    "`x` must not be the abstract class <ernest_lrps>."
  )
})

test_that("update_lrps.ernest_lrps is idempotent", {
  lrps <- new_ernest_lrps(fn, 2L)
  expect_idempotent_update(lrps)
})
