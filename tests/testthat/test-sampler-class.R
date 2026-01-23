wrapped_lik <- create_likelihood(gaussian_blobs$log_lik)

test_that("ernest_sampler initializes correctly", {
  sampler <- new_ernest_sampler(
    log_lik_fn = wrapped_lik,
    prior = gaussian_blobs$prior,
    lrps = rwmh_cube(),
    nlive = 500,
    first_update = 200L,
    update_interval = 50L
  )

  expect_identical(sampler$log_lik, wrapped_lik)
  expect_identical(sampler$prior, gaussian_blobs$prior)
  expect_identical(sampler$nlive, 500L)
  expect_identical(sampler$first_update, 200L)
  expect_identical(sampler$update_interval, 50L)
  expect_identical(env_depth(sampler$run_env), 1L)
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
