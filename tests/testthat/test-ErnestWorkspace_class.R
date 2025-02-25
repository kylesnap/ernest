test_that("ErnestWorkspace initializes correctly", {
  log_lik <- function(x) sum(x^2)
  prior_transform <- function(x) x * 2 - 1
  n_dim <- 3
  n_points <- 5

  workspace <- ErnestWorkspace$new(log_lik, prior_transform, n_dim, n_points)

  expect_equal(nrow(workspace$live_units), n_points)
  expect_equal(ncol(workspace$live_units), n_dim)
  expect_equal(nrow(workspace$live_points), n_points)
  expect_equal(ncol(workspace$live_points), n_dim)
  expect_equal(length(workspace$live_lik), n_points)
})

test_that("pop_point logs points correctly", {
  log_lik <- function(x) sum(x^2)
  prior_transform <- function(x) x * 2 - 1
  n_dim <- 3
  n_points <- 5

  workspace <- ErnestWorkspace$new(log_lik, prior_transform, n_dim, n_points)

  expect_error(
    workspace$pop_point(1, -1, -2),
    "`pop_point` called before `worst_idx`"
  )

  worst_idx <- workspace$worst_idx
  unit <- workspace$live_units[worst_idx, ]
  point <- workspace$live_points[worst_idx, ]
  lik <- workspace$live_lik[worst_idx]

  copy <- workspace$pop_point(1, -1, -2)

  expect_equal(workspace$private$.dead_units[[1]], unit)
  expect_equal(workspace$private$.dead_points[[1]], point)
  expect_equal(workspace$private$.dead_lik[[1]], lik)
  expect_equal(workspace$private$.log_z, -1)
  expect_equal(workspace$private$.dead_vol[[1]], -2)
  expect_true(copy > 0 && copy <= n_points && copy != worst_idx)
})

test_that("push_point adds new points correctly", {
  log_lik <- function(x) sum(x^2)
  prior_transform <- function(x) x * 2 - 1
  n_dim <- 3
  n_points <- 5

  workspace <- ErnestWorkspace$new(log_lik, prior_transform, n_dim, n_points)
  iter <- 1
  new_point <- list(unit = runif(n_dim), parameter = runif(n_dim), log_lik = log_lik(runif(n_dim)), num_calls = 1)
  copy <- 1
  cur_update <- 1

  worst_idx <- workspace$worst_idx
  copy <- workspace$pop_point(iter, -1, -2)
  calls <- workspace$push_point(iter, new_point, copy, cur_update)

  expect_equal(workspace$live_units[workspace$private$.worst_idx, ], new_point$unit)
  expect_equal(workspace$live_points[workspace$private$.worst_idx, ], new_point$parameter)
  expect_equal(workspace$live_lik[workspace$private$.worst_idx], new_point$log_lik)
  expect_equal(workspace$private$.calls[[iter]], new_point$num_calls)
  expect_equal(workspace$private$.parent[[iter]], copy)
  expect_equal(workspace$private$.updates[[iter]], cur_update)
  expect_equal(calls, new_point$num_calls)
})

test_that("get_dead returns dead points correctly", {
  log_lik <- function(x) sum(x^2)
  prior_transform <- function(x) x * 2 - 1
  n_dim <- 3
  n_points <- 5

  workspace <- ErnestWorkspace$new(log_lik, prior_transform, n_dim, n_points)
  iter <- 1
  log_z <- -1.e300
  log_vol <- -1.e300

  worst_idx <- workspace$worst_idx
  workspace$pop_point(iter, log_z, log_vol)
  dead_points <- workspace$get_dead(FALSE)

  expect_equal(nrow(dead_points), iter)
  expect_equal(ncol(dead_points), n_dim)
})

test_that("active bindings work correctly", {
  log_lik <- function(x) sum(x^2)
  prior_transform <- function(x) x * 2 - 1
  n_dim <- 3
  n_points <- 5

  workspace <- ErnestWorkspace$new(log_lik, prior_transform, n_dim, n_points)

  expect_equal(workspace$n_iter, 0)
  expect_equal(workspace$n_call, 0)
  expect_equal(workspace$log_vol, 0.0)
  expect_equal(workspace$log_z, -1.e300)
  expect_equal(workspace$worst_lik, -1.e300)
  expect_equal(workspace$last_update, 0L)
})
