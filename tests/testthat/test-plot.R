test_that("ernest_run can be plotted", {
  run <- readRDS(test_path("./example_run.rds"))
  vdiffr::expect_doppelganger("ernest_run plot", plot(run))
})

test_that("ernest_run can be calculated, then plotted", {
  run <- readRDS(test_path("./example_run.rds"))
  set.seed(42)
  expect_warning(vdiffr::expect_doppelganger(
    "ernest_run, one ndraw",
    plot(run, ndraws = 1)
  ))

  expect_warning(vdiffr::expect_doppelganger(
    "ernest_run, two ndraws",
    plot(run, ndraws = 2)
  ))

  vdiffr::expect_doppelganger(
    "ernest_run, many draws",
    plot(run, ndraws = 100)
  )
})

test_that("ernest_estimate can be plotted", {
  run <- readRDS(test_path("./example_run.rds"))
  set.seed(42)
  calculate <- calculate(run, ndraws = 100)
  vdiffr::expect_doppelganger("ndraws = 100", plot(calculate))
})
