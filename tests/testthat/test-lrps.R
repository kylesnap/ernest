test_that("unif_cube creates correct structure", {
  sampler <- unif_cube()
  expect_s3_class(sampler, c("lrps_call", "call"))
  expect_mapequal(
    call_args(sampler),
    list(
      log_lik_fn = missing_arg(),
      prior_fn = missing_arg(),
      n_dim = missing_arg()
    )
  )

  sampler <- call_modify(
    sampler,
    log_lik_fn = gaussian_2$log_lik,
    prior_fn = \(x) qunif(x, -5, 5),
    n_dim = 2
  )
  sampler <- eval_bare(sampler)
  expect_s3_class(sampler, c("uniform_lrps", "ernest_lrps"))
})

test_that("rwmh_cube creates correct structure", {
  sampler <- rwmh_cube()
  expect_s3_class(sampler, c("lrps_call", "call"))
  expect_mapequal(
    call_args(sampler),
    list(
      log_lik_fn = missing_arg(),
      prior_fn = missing_arg(),
      n_dim = missing_arg(),
      steps = 25L,
      target_acceptance = 0.5
    )
  )

  sampler <- call_modify(
    sampler,
    log_lik_fn = gaussian_2$log_lik,
    prior_fn = \(x) qunif(x, -5, 5),
    n_dim = 2
  )
  sampler <- eval_bare(sampler)
  expect_s3_class(sampler, c("rwcube_lrps", "ernest_lrps"))
})
