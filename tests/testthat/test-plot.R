test_that("ernest_run can be plotted", {
  run <- readRDS(test_path("./example_run.rds"))
  vdiffr::expect_doppelganger("ernest_run plot", plot(run))
})

test_that("ernest_run can be calculated, then plotted", {
  run <- readRDS(test_path("./example_run.rds"))
  set.seed(42)
  vdiffr::expect_doppelganger(
    "ernest_run plot after calculate",
    plot(run, ndraws = 100)
  )
})

test_that("ernest_estimates can be plotted", {
  run <- readRDS(test_path("./example_run.rds"))
  set.seed(42)
  calculate <- calculate(run, ndraws = 100)
  vdiffr::expect_doppelganger("ndraws = 100", plot(calculate))
})
