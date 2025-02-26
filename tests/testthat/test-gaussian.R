# test_that("2D Gaussian Likelihood (Uniform Cube)", {
#   gauss <- make_gaussian(2L)
#   sampler <- nested_sampling(
#     gauss$log_lik,
#     prior_transform = gauss$prior_transform,
#     n_dim = 2L,
#     sampler = unif_cube()
#   )
#   run <- generate(sampler, dlogz = 0.5)
# })

test_that("2D Gaussian Likelihood (Random Walk)", {
  skip_on_cran()
  gauss <- make_gaussian(2L)
  sampler <- nested_sampling(
    gauss$log_lik,
    prior_transform = gauss$prior_transform,
    n_dim = 2L,
    sampler = rwmh_cube(),
    verbose = TRUE
  )
  run <- generate(sampler, dlogz = 0.5)
})
