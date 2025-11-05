fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
describe("new_ernest_lrps", {
  it("fails informatively", {
    expect_snapshot(new_ernest_lrps(unit_log_fn = 1), error = TRUE)
    expect_snapshot(new_ernest_lrps(fn, n_dim = 0), error = TRUE)
    expect_snapshot(new_ernest_lrps(fn, n_dim = 2, cache = 1), error = TRUE)

    local_options("ernest.max_loop" = 0L)
    expect_snapshot(new_ernest_lrps(fn, n_dim = 2), error = TRUE)
    local_options("ernest.max_loop" = Inf)
    expect_snapshot(new_ernest_lrps(fn, n_dim = 2), error = TRUE)
  })

  it("initializes correctly", {
    lrps <- new_ernest_lrps(fn, 2L)
    check_valid_lrps(lrps)

    local_options("ernest.max_loop" = 100L)
    obj <- new_ernest_lrps(fn, 2L)
    check_valid_lrps(obj)
    expect_equal(obj$max_loop, 100L)
  })
})

test_that("propose.ernest_lrps can be called", {
  lrps <- new_ernest_lrps(fn, 2L)
  initial_ncall <- env_cache(lrps$cache, "n_call", 0L)
  res1 <- propose.ernest_lrps(lrps, original = NULL)
  expect_type(res1, "list")
  expect_contains(names(res1), c("unit", "log_lik", "n_call"))
  expect_vector(res1$unit, double(), size = lrps$n_dim)
  expect_equal(res1$log_lik, fn(res1$unit))
  expect_equal(env_get(lrps$cache, "n_call"), 0L)

  expect_snapshot(propose.ernest_lrps(lrps, c(0.5, 0.5), -1), error = TRUE)
})

test_that("update_lrps.ernest_lrps is idempotent", {
  lrps <- new_ernest_lrps(fn, 2L)
  lrps$cache$n_call <- 5L
  new_lrps <- update_lrps(lrps)
  expect_equal(new_lrps$cache$n_call, 0L)
  newer_lrps <- update_lrps(new_lrps)
  expect_identical(new_lrps, newer_lrps)
})
