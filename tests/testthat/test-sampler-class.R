wrapped_lik <- create_likelihood(gaussian_blobs$log_lik)

test_that("ernest_sampler initializes correctly", {
  sampler <- new_ernest_sampler(
    log_lik_fn = wrapped_lik,
    prior = gaussian_blobs$prior,
    lrps = rwmh_cube(),
    nlive = 500,
    first_update = 200,
    update_interval = 50
  )
  expect_identical(env_depth(sampler$live_env), 1L)
  expect_snapshot(sampler)
})

sampler_call <- call2(
  new_ernest_sampler,
  log_lik_fn = wrapped_lik,
  prior = gaussian_blobs$prior,
  lrps = rwmh_cube(),
  nlive = 500,
  first_update = 200L,
  update_interval = 50L
)

#' @srrstats {G5.2, G5.2a, G5.2b} Constructors are all tested for informative
#' error messages
describe("new_ernest_sampler", {
  expect_no_error(bad_sampler <- eval(sampler_call))

  it("catches invalid nlive", {
    points_call <- call_modify(sampler_call, nlive = 0L)
    expect_error(
      eval(points_call),
      "`nlive` must be a whole number larger than or equal to 1"
    )
    bad_sampler$nlive <- Inf
    expect_error(
      refresh_ernest_sampler(bad_sampler),
      "`nlive` must be a whole number, not `Inf`"
    )
  })

  it("catches invalid first_update/update_interval", {
    first_update_call <- call_modify(sampler_call, first_update = -1L)
    expect_error(
      eval(first_update_call),
      "`first_update` must be a whole number larger than or equal to 0"
    )
    bad_sampler$first_update <- Inf
    expect_error(
      refresh_ernest_sampler(bad_sampler),
      "`first_update` must be a whole number, not `Inf`"
    )

    update_interval_call <- call_modify(sampler_call, update_interval = -1L)
    expect_error(
      eval(update_interval_call),
      "`update_interval` must be a whole number larger than or equal to 0"
    )
    bad_sampler$first_update <- 25L
    bad_sampler$update_interval <- Inf
    expect_error(
      refresh_ernest_sampler(bad_sampler),
      "`update_interval` must be a whole number, not `Inf`"
    )
  })

  it("catches log_lik_fn/prior", {
    loglik_call <- call_modify(sampler_call, log_lik_fn = list())
    expect_error(
      eval(loglik_call),
      "`log_lik_fn` must be an object with class ernest_likelihood"
    )
    bad_sampler$log_lik_fn <- sum
    expect_error(
      refresh_ernest_sampler(bad_sampler),
      "`log_lik_fn` .+ not a primitive function."
    )
    bad_sampler$log_lik_fn <- wrapped_lik

    prior_call <- call_modify(sampler_call, prior = list())
    expect_error(
      eval(prior_call),
      "`prior` must be an object with class ernest_prior"
    )
    bad_sampler$prior <- sum
    expect_error(
      refresh_ernest_sampler(bad_sampler),
      "`prior` .+ not a primitive function"
    )
  })

  it("Catches invalid LRPS", {
    lrps_call <- call_modify(sampler_call, lrps = list())
    expect_error(
      eval(lrps_call),
      "`lrps` must be an object with class ernest_lrps"
    )
    lrps_call <- call_modify(sampler_call, lrps = stats::qunif)
    expect_error(eval(lrps_call), "`lrps` .+ not a function")
  })
})

test_that("refresh works as expected", {
  sampler <- eval(sampler_call)
  sampler2 <- refresh_ernest_sampler(sampler)
  expect_identical(sampler, sampler2)
})

#' Zero-Length Data
#'
#' @srrstats {G5.8a} Tests for when likelihood and prior presented by user
#' return zero-lengths.
test_that("Zero-length likelihood fails", {
  ll <- \(theta) double(0)
  prior <- create_uniform_prior(names = LETTERS[1:2])

  expect_snapshot(ernest_sampler(ll, prior, seed = 42), error = TRUE)
})

#' Wrong types
#'
#' @srrstats {G5.8b} Ernest fails on non-numeric likelihood/prior outputs.
NULL

test_that("Fails on character types", {
  prior_fn <- \(theta) c("A", "B")
  expect_snapshot(create_prior(prior_fn, names = LETTERS[1:2]), error = TRUE)

  ll <- \(theta) if (theta[1] < 0) "L" else "U"
  expect_snapshot(
    ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42),
    transform = \(x) gsub("\\d+\\.\\d+", "#\\.#", x),
    error = TRUE
  )
})

test_that("Fails on complex types", {
  prior_fn <- \(theta) 0.15i * theta
  expect_snapshot(create_prior(prior_fn, names = LETTERS[1:2]), error = TRUE)

  ll <- \(theta) sum(0.15i * length(theta))
  expect_snapshot(
    ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42),
    transform = \(x) gsub("\\d+\\.\\d+", "#\\.#", x),
    error = TRUE
  )
})

#' Special tests: Perfectly flat and nearly flat likelihoods
#'
#' @srrstats {G5.8d} Covers situations in which the likelihood starts as flat
#' (caught by compile) or becomes flat after many iterations.
NULL

test_that("Ernest fails when ll is flat to begin with", {
  ll <- \(theta) 0
  expect_snapshot(
    ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42),
    error = TRUE
  )
})
