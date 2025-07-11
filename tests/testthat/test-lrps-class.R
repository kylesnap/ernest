gaussian_2 <- make_gaussian(2)

test_that("ernest_lrps class initializes correctly", {
  lrps <- ernest_lrps$new(gaussian_2$log_lik, gaussian_2$prior$fn, 2L)

  expect_equal(lrps$private$max_loop, 1e6L)
  expect_equal(lrps$private$n_iter, 0L)
  expect_equal(lrps$private$n_call, 0L)
  expect_equal(lrps$private$n_dim, 2L)
  expect_snapshot(lrps$as_string())
  expect_equal(
    lrps$history,
    list("n_iter" = integer(), "n_call" = integer())
  )
})

test_that("ernest_lrps class initializes find_point", {
  lrps <- ernest_lrps$new(gaussian_2$log_lik, gaussian_2$prior$fn, 2L)
  units <- matrix(seq(0, 1, length.out = 200), nrow = 100, ncol = 2)
  transformed_units <- apply(units, 1, lrps$find_point)

  expected_points <- apply(units, 1, gaussian_2$prior$fn)
  expect_equal(
    do.call(rbind, lapply(transformed_units, `[[`, "point")),
    t(expected_points)
  )

  expected_log_lik <- apply(expected_points, 2, gaussian_2$log_lik)
  expect_equal(
    vctrs::vec_cast(
      do.call(c, lapply(transformed_units, `[[`, "log_lik")),
      numeric()
    ),
    expected_log_lik
  )
})

test_that("Uniform subclass initalizes", {
  uniform <- uniform_lrps$new(gaussian_2$log_lik, gaussian_2$prior$fn, 2L)

  expect_snapshot(uniform$as_string())
  expect_equal(
    uniform$history,
    list("n_iter" = integer(), "n_call" = integer())
  )
})

uniform <- uniform_lrps$new(gaussian_2$log_lik, gaussian_2$prior$fn, 2L)
test_that("Uniform subclass calls propose_uniform correctly", {
  result <- uniform$propose_uniform(-Inf)
  expect_equal(dim(result$unit), c(1, 2))
  expect_equal(
    gaussian_2$log_lik(gaussian_2$prior$fn(result$unit)),
    result$log_lik
  )
  expect_gte(result$log_lik, -Inf)

  test_val <- c(-37, -16, -9, -5, -3)
  result <- uniform$propose_uniform(test_val)
  expect_equal(dim(result$unit), c(5, 2))
  expect_true(all(result$log_lik >= test_val))
  expect_equal(
    apply(result$unit, 1, \(x) gaussian_2$log_lik(gaussian_2$prior$fn(x))),
    result$log_lik
  )
  expect_equal(
    uniform$history,
    list("n_iter" = integer(), "n_call" = integer())
  )
})

test_that("Uniform subclass calls propose_live correctly", {
  original <- matrix(runif(5 * 2), ncol = 2)
  test_val <- c(-37, -16, -9, -5, -3)
  result <- uniform$propose_live(original, test_val)
  expect_equal(dim(result$unit), dim(original))
  expect_true(all(result$unit != original))
  expect_true(all(result$log_lik >= test_val))
  n_call <- result$n_call
  expect_mapequal(
    uniform$history,
    list("n_iter" = c(5L), "n_call" = n_call)
  )

  test_val <- c(-37, -16, -9, -5, -3)
  result2 <- uniform$propose_live(original, test_val)
  expect_mapequal(
    uniform$history,
    list(
      "n_iter" = c(5L, 10L),
      "n_call" = c(n_call, n_call + result2$n_call)
    )
  )
})

test_that("Uniform can be updated without protest, and cleared", {
  expect_no_message(
    uniform$update()
  )
  expect_equal(uniform$since_update, 0L)

  uniform$clear()
  expect_equal(uniform$since_update, 0L)
  expect_equal(
    uniform$history,
    list("n_iter" = integer(), "n_call" = integer())
  )
})

test_that("RWMH can be initialized and catches bad parameters", {
  expect_snapshot_error({
    rwcube_lrps$new(
      log_lik_fn = gaussian_2$log_lik,
      prior_fn = gaussian_2$prior$fn,
      n_dim = 2L,
      target_acceptance = 0.02
    )
  })
  expect_snapshot_error({
    rwcube_lrps$new(
      log_lik_fn = gaussian_2$log_lik,
      prior_fn = gaussian_2$prior$fn,
      n_dim = 2L,
      target_acceptance = 1.1
    )
  })
  expect_snapshot_error({
    rwcube_lrps$new(
      log_lik_fn = gaussian_2$log_lik,
      prior_fn = gaussian_2$prior$fn,
      n_dim = 2L,
      steps = 1
    )
  })

  new <- rwcube_lrps$new(
    log_lik_fn = gaussian_2$log_lik,
    prior_fn = gaussian_2$prior$fn,
    n_dim = 2L
  )
  expect_snapshot(new$as_string())
  expect_equal(new$epsilon, 1)
})

rwcube <- rwcube_lrps$new(
  log_lik_fn = gaussian_2$log_lik,
  prior_fn = gaussian_2$prior$fn,
  n_dim = 2L
)
test_that("rwcube can call propose_live on a single point", {
  result <- rwcube$propose_live(c(0.5, 0.5), -Inf)
  expect_equal(dim(result$unit), c(1, 2))
  expect_equal(
    gaussian_2$log_lik(gaussian_2$prior$fn(result$unit)),
    result$log_lik
  )
  expect_gte(result$log_lik, -Inf)
})

test_that("rwcube can call propose_live on many points", {
  prev <- rwcube$history
  unit <- matrix(runif(10 * 2), nrow = 10)
  original <- t(apply(unit, 1, gaussian_2$prior$fn))
  logl_expected <- apply(original, 1, gaussian_2$log_lik)

  result <- rwcube$propose_live(unit, logl_expected)
  expect_equal(dim(result$unit), dim(original))
  expect_equal(
    result$log_lik,
    apply(result$unit, 1, \(x) gaussian_2$log_lik(gaussian_2$prior$fn(x)))
  )
  expect_true(all(result$log_lik >= logl_expected))

  expect_mapequal(
    rwcube$history,
    list(
      "n_iter" = c(1L, 11L),
      "n_call" = c(25L, 275L),
      "n_accept" = c(prev$n_accept, prev$n_accept + result$n_accept),
      epsilon = c(1, 1)
    )
  )
})

test_that("rwcube can update", {
  n_accept <- rwcube$history$n_accept[2]
  rwcube$update()
  epsilon <- rwcube$epsilon
  expect_lt(epsilon, 1)

  unit <- matrix(punif(seq(-5, 5, length.out = 10), -5, 5), nrow = 5)
  logl_expected <- c(-10.311600, -6.41296, -5.113419, -6.412965, -10.311600)
  result <- rwcube$propose_live(unit, logl_expected)
  expect_equal(
    rwcube$history$n_call,
    c(25L, 275L, 125L)
  )
  expect_lt(rwcube$epsilon, 1)
})

test_that("rwcube can clear", {
  rwcube$clear()
  expect_equal(rwcube$since_update, 0L)
  expect_equal(
    rwcube$history,
    list(
      "n_iter" = integer(),
      "n_call" = integer(),
      "n_accept" = integer(),
      "epsilon" = numeric()
    )
  )
  expect_equal(rwcube$epsilon, 1)
})

test_that("rwcube can handle plateau", {
  log_l <- function(x) {
    -1
  }
  prior <- create_uniform_prior(3, lower = -10, upper = 10)

  rwcube <- rwcube_lrps$new(log_l, prior$fn, 3L)
  att_one <- rwcube$propose_live(c(0, 0, 0), 0)
  expect_mapequal(
    att_one,
    list(
      "unit" = matrix(c(0, 0, 0), ncol = 3),
      "log_lik" = -1,
      "n_accept" = 0L,
      "n_call" = 25L
    )
  )
})
