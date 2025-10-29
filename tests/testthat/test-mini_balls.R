fn <- \(x) gaussian_blobs$prior$fn(x) |> gaussian_blobs$log_lik()
set.seed(42)

describe("mini_balls constructor", {
  it("returns correct class and structure", {
    obj <- mini_balls(1.5, p = 2)
    expect_s3_class(obj, c("mini_balls", "ernest_lrps"), exact = TRUE)
    expect_null(obj$unit_log_fn)
    expect_null(obj$n_dim)
    expect_equal(obj$max_loop, 1e6L)
    expect_equal(obj$enlarge, 1.5)
    expect_equal(obj$p, 2)
    expect_true("radius" %in% names(as.list(obj$cache)))
  })

  it("sets defaults and warns for enlarge = 1", {
    expect_snapshot(obj <- mini_balls(enlarge = 1))
    expect_equal(obj$enlarge, 1)
    expect_equal(obj$p, 2)
  })

  it("accepts method argument and sets p accordingly", {
    obj <- mini_balls(method = "maximum")
    expect_equal(obj$p, Inf)
    obj <- mini_balls(method = "manhattan")
    expect_equal(obj$p, 1)
    obj <- mini_balls(method = "euclidean")
    expect_equal(obj$p, 2)
  })
})

test_that("Test", {})

# Use a simple log-likelihood and prior for proposal tests
describe("propose.mini_balls", {
  it("proposes points in the unit cube", {
    sampler <- new_mini_balls(fn, 2, p = 2)
    env_poke(sampler$cache, "radius", 0.1)
    result <- propose(sampler, original = c(0.5, 0.5), criterion = -Inf)
    expect_length(result$unit, 2)
    expect_true(all(result$unit >= 0 & result$unit <= 1))
    expect_equal(result$log_lik, fn(result$unit))
    expect_gt(sampler$cache$n_call, 0L)
  })

  it("proposes points for p = 1 and p = Inf", {
    sampler1 <- new_mini_balls(fn, 2, p = 1)
    env_poke(sampler1$cache, "radius", 0.1)
    result1 <- propose(sampler1, original = c(0.5, 0.5), criterion = -Inf)
    expect_length(result1$unit, 2)
    expect_true(all(result1$unit >= 0 & result1$unit <= 1))

    sampler_inf <- new_mini_balls(fn, 2, p = Inf)
    env_poke(sampler_inf$cache, "radius", 0.1)
    result_inf <- propose(sampler_inf, original = c(0.5, 0.5), criterion = -Inf)
    expect_length(result_inf$unit, 2)
    expect_true(all(result_inf$unit >= 0 & result_inf$unit <= 1))
  })
})

describe("update_lrps.mini_balls", {
  it("is idempotent if unit is NULL", {
    sampler <- mini_balls()
    new_sampler2 <- update_lrps(sampler)
    expect_identical(new_sampler2, sampler)
  })

  it("updates radius to minimum nonzero distance", {
    sampler <- new_mini_balls(fn, 2, p = 2)
    live <- rbind(c(0.1, 0.1), c(0.2, 0.1), c(0.1, 0.2))
    updated <- update_lrps(sampler, live)
    expect_equal(updated$cache$radius, 0.1)
  })

  it("warns if all points are identical", {
    sampler <- new_mini_balls(fn, 2, p = 2)
    live <- matrix(rep(0.5, 6), ncol = 2)
    expect_snapshot(update_lrps(sampler, live))
  })
})
