test_that("ernest run object validates", {
  sampler <- unit_cube(2)
  log_lik <- function(x) sum(dnorm(x, log = TRUE))
  prior_list <- merge_transformations(list(
    prior_transform(\(u) qnorm(u)),
    prior_transform(\(u) qnorm(u))
  ))

  sampler <- unit_cube(2)
  sampler$log_lik <- log_lik
  sampler$prior <- prior_list$composite_fn
  sampler <- refresh(sampler)

  result <- nested_sampling_impl(
    sampler = sampler,
    live_size = 100,
    dlogz = 0.01,
    max_iter = 500,
    max_call = 1000,
    first_update = 20L,
    update_iter = 10,
    verbose = FALSE
  )

  new_ernest_run(
    sampler,
    prior_list,
    result
  ) |> print()
})
