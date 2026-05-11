test_that("summary.ernest_estimate works when ndraws = 0", {
  data(example_run)
  obj <- calculate(example_run, ndraws = 0)
  base <- c("x", ".value")
  out <- summary(
    obj,
    which = c("evidence", "weight", "likelihood"),
    n = 512,
    width = c(0.5, 0.8, 0.95)
  )

  expect_snapshot_value(out$evidence, tolerance = 1e-3, style = "json2")
  expect_snapshot_value(out$weight, tolerance = 1e-3, style = "json2")
  expect_snapshot_value(out$likelihood, tolerance = 1e-3, style = "json2")
})

test_that("summary.ernest_estimate works when ndraws = 100", {
  data(example_run)
  withr::local_seed(42)
  obj <- calculate(example_run, ndraws = 100)
  base <- c("x", ".value")
  curve <- c(
    c(".lower", ".upper", ".actual_width"),
    c(".width", ".point", ".interval")
  )
  out <- summary(
    obj,
    which = c("evidence", "weight", "likelihood"),
    n = 512,
    width = c(0.5, 0.8, 0.95)
  )

  expect_named(out, c("evidence", "weight", "likelihood"))
  expect_named(out$evidence, c(base, curve))
  expect_named(out$weight, c(base, curve))
  expect_named(out$likelihood, base)
})

test_that("plot throws errors", {
  expect_error(plot(example_run, n = 1), "larger than or equal to 2")
  expect_error(
    plot(example_run, which = "bad"),
    'must be one of "evidence", "weight", or "likelihood"'
  )
})

test_that("plot an ernest_estimate object", {
  skip_plot_snapshot()
  set.seed(42)
  calc <- calculate(example_run, 500)

  vdiffr::expect_doppelganger(
    "estimate (500 draws)",
    plot(calc)
  )

  vdiffr::expect_doppelganger(
    "estimate (500 draws, no evidence)",
    plot(calc, which = "evidence")
  )

  calc <- calculate(example_run, ndraws = 1)
  vdiffr::expect_doppelganger(
    "estimate (1 draw)",
    plot(calc)
  )
})

test_that("plot an ernest_run object", {
  skip_plot_snapshot()
  vdiffr::expect_doppelganger(
    "example_run",
    plot(example_run)
  )

  vdiffr::expect_doppelganger(
    "example_run (no evidence)",
    plot(example_run, which = c("weight", "likelihood"))
  )
})
