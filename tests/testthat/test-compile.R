lrps <- new_rwmh_cube(
  log_lik = gaussian_2$log_lik,
  prior_transform = gaussian_2$prior_transform,
  n_dim = 2,
  update_interval = 100,
  num_steps = 20,
  target_acceptance = 0.25,
  init_epsilon <- 1
)

test_that("Compile works from scratch", {
  sampler <- ernest_sampler$new(lrps, 3)
  sampler$compile()
  live <- sampler$live_points
  expect_named(
    live,
    c("units", "points", "log_lik")
  )
  expect_equal(
    t(apply(live$units, 1, gaussian_2$prior_transform)),
    live$points
  )
  expect_equal(
    apply(live$points, 1, gaussian_2$log_lik),
    live$log_lik
  )
})

test_that("Compile works when refresh = FALSE", {
  sampler <- ernest_sampler$new(lrps, 3)
  sampler$generate(max_iterations = 1000)
  live_pre <- sampler$live_points

  sampler$compile(refresh = FALSE)
  live_post <- sampler$live_points
  expect_equal(live_pre, live_post)
})

test_that("Compile works when refresh = TRUE", {
  sampler <- ernest_sampler$new(lrps, 3)
  sampler$generate(max_iterations = 1000)
  expect_named(
    sampler$dead_points,
    c("units", "points", "log_lik")
  )
  live_pre <- sampler$live_points

  sampler$compile(refresh = TRUE)
  live_post <- sampler$live_points
  expect_true(
    !identical(live_pre, live_post)
  )
  expect_true(is_empty(sampler$dead_points))
})

test_that("Passing a poorly formed LRPS raises errors", {
  bad_lrps <- lrps
  bad_lrps$prior_transform <- function(x) {
    c(-Inf, stats::qunif(x[2], -10, 10))
  }
  expect_snapshot_error(
    ernest_sampler$new(bad_lrps, 3)$compile()
  )

  bad_lrps <- lrps
  bad_lrps$log_lik <- function(x) {
    if (any(x < 0)) {
      NaN
    } else {
      gaussian_2$log_lik(x)
    }
  }
  expect_snapshot_error(
    ernest_sampler$new(bad_lrps, 3)$compile()
  )

  bad_lrps <- lrps
  bad_lrps$log_lik <- function(x) {
    if (any(x < 0)) {
      -Inf
    } else {
      gaussian_2$log_lik(x)
    }
  }
  expect_snapshot_warning(
    ernest_sampler$new(bad_lrps, 3)$compile()
  )
})

test_that("create_live generates valid live points", {
  live_points <- create_live(lrps, 100)

  expect_equal(dim(live_points$units), c(100, 2))
  expect_equal(dim(live_points$points), c(100, 2))
  expect_length(live_points$log_lik, 100)
  expect_true(all(live_points$units >= 0 & live_points$units <= 1))
  expect_true(all(is.finite(live_points$points)))
  expect_true(all(is.finite(live_points$log_lik) | live_points$log_lik == -Inf))
})

test_that("create_live throws errors as expected", {
  lrps$prior_transform <- function(x) {
    x[1] <- Inf
    x
  }
  expect_snapshot_error(create_live(lrps, 100))

  lrps$prior_transform <- gaussian_2$prior_transform
  lrps$log_lik <- function(x) {
    c(1, 2)
  }
  expect_snapshot_error({
    live <- create_live(lrps, 100)
    print(live$log_lik)
  })

  lrps$log_lik <- function(x) {
    if (any(x < 0.5)) {
      NaN
    } else {
      gaussian_2$log_lik(x)
    }
  }
  expect_snapshot_error(create_live(lrps, 100))

  lrps$log_lik <- function(x) {
    if (any(x < 0.5)) {
      NA
    } else {
      gaussian_2$log_lik(x)
    }
  }
  expect_snapshot_error(create_live(lrps, 100))
})

test_that("check_live validates live points correctly", {
  lrps <- new_rwmh_cube(
    log_lik = gaussian_2$log_lik,
    prior_transform = gaussian_2$prior_transform,
    n_dim = 2,
    update_interval = 100,
    num_steps = 20,
    target_acceptance = 0.25,
    init_epsilon <- 1
  )
  live_points <- create_live(lrps, 100)

  expect_null(check_live(live_points, lrps, 100))

  expect_error(
    check_live("Blorp", lrps, 100),
    "`live` must be a list."
  )

  bad_list <- list(units = live_points$points, points = live_points$units)
  expect_error(
    check_live(bad_list, lrps, 100),
    "`live` must contain elements `units`, `points`, and `log_lik`."
  )

  bad_list <- live_points
  bad_list$units <- runif(100)
  expect_error(
    check_live(bad_list, lrps, 100),
    "must be a matrix."
  )

  bad_list <- live_points
  bad_list$points <- runif(100)
  expect_error(
    check_live(bad_list, lrps, 100),
    "must be a matrix."
  )

  bad_list <- live_points
  bad_list$log_lik <- runif(99)
  expect_error(
    check_live(bad_list, lrps, 100),
    "must be a vector of doubles with length 100."
  )

  bad_list$log_lik <- as.character(runif(100))
  expect_error(
    check_live(bad_list, lrps, 100),
    "must be a vector of doubles with length 100."
  )

  bad_list <- live_points
  bad_list$units <- live_points$units[c(1:99),]
  expect_error(
    check_live(bad_list, lrps, 100),
    "100, "
  )

  bad_lrps <- lrps
  bad_lrps$n_dim <- 3
  expect_error(
    check_live(live_points, bad_lrps, 100),
    "100, 3"
  )

  bad_list <- live_points
  bad_list$units[2,2] <- -0.5
  expect_error(
    check_live(bad_list, lrps, 100),
    "contains values outside of \\[0, 1\\]."
  )

  bad_list$units <- live_points$units
  bad_list$points[2,2] <- NA
  expect_error(
    check_live(bad_list, lrps, 100),
    "contains non-finite values."
  )

  bad_list$points <- live_points$points
  bad_list$log_lik[1] <- NaN
  expect_error(
    check_live(bad_list, lrps, 100),
    "contains non-finite values."
  )

  bad_list$log_lik[1] <- Inf
  expect_error(
    check_live(bad_list, lrps, 100),
    "contains `\\+Inf` values."
  )

  bad_list$log_lik[1] <- -Inf
  expect_null(check_live(bad_list, lrps, 100))
})
