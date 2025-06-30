ll_fn <- function(x) {
  -0.5 * sum(x^2)
}

prior <- create_uniform_prior(n_dim = 2, lower = -10, upper = 10)

extract_dead <- function(run, dead_it) {
  dead <- tibble(
    "log_lik" = run$log_lik[dead_it],
    "log_volume" = run$log_volume[dead_it],
    "log_weight" = run$log_weight[dead_it],
    "log_evidence" = run$log_evidence[dead_it],
    "id" = run$id[dead_it],
    "points" = run$points[dead_it],
    "calls" = run$calls[dead_it],
    "birth" = run$birth[dead_it]
  )
}
set.seed(42L)

test_that("Runs can be completed and resume", {
  sampler <- nested_sampling(ll_fn, prior, n_point = 100)

  run1 <- generate(sampler, max_iterations = 100)
  dead_it1 <- seq(run1$n_iter)
  dead1 <- extract_dead(run1, dead_it1)

  run2 <- generate(sampler, max_iterations = 300)
  dead2_1 <- extract_dead(run2, dead_it1)
  expect_mapequal(dead1, dead2_1)
  dead_it2 <- seq(run2$n_iter)
  dead2 <- extract_dead(run2, dead_it2)

  run3 <- generate(sampler)
  dead3_2 <- extract_dead(run3, dead_it2)
  expect_mapequal(dead2, dead3_2)
})

test_that("Throws errors when stop criteria are already passed", {
  sampler <- nested_sampling(ll_fn, prior, n_point = 100)
  generate(sampler, max_iterations = 100)
  expect_snapshot_error(generate(sampler, max_iterations = 50))

  sampler <- nested_sampling(ll_fn, prior, n_point = 100)
  generate(sampler, max_calls = 1000)
  expect_snapshot_error(generate(sampler, max_calls = 500))

  sampler <- nested_sampling(ll_fn, prior, n_point = 100)
  generate(sampler)
  expect_snapshot_error(generate(sampler, min_logz = 1))
})

test_that("Early exits are repaired", {
  test_env <- new_environment()
  env_bind(test_env, "count" = -100, "max_count" = 100)
  counting_ll <- function(x) {
    test_env$count <- test_env$count + 1L
    if (test_env$count > test_env$max_count) {
      stop("Early exit")
    }
    ll_fn(x)
  }

  sampler <- nested_sampling(counting_ll, prior, n_point = 100)
  expect_error(generate(sampler, max_iterations = 200), "Early exit")

  test_env$max_count <- .Machine$integer.max
  expect_snapshot_warning(run <- generate(sampler, max_iterations = 100))
  expect_equal(run$n_iter, 100)
  dead_it <- seq(run$n_iter)
  dead <- extract_dead(run, dead_it)

  test_env$max_count <- run$n_iter + 100L
  expect_error(run <- generate(sampler), "Early exit")

  test_env$max_count <- .Machine$integer.max
  expect_snapshot_warning(run2 <- generate(sampler))
  dead_2 <- extract_dead(run2, dead_it)
  expect_mapequal(dead, dead_2)
})
