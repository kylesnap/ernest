test_that("Density tables", {
  set.seed(42)
  run <- readRDS(test_path("./example_run.rds"))
  table <- visualize(run, type = "density", plot = FALSE)

  expect_s3_class(table, "tbl_df")
  expect_named(table, c(".var", ".dist"))
  expect_equal(table$.var, colnames(run$samples))

  table <- visualize(run, units = "unit_cube", type = "density", plot = FALSE)
  expect_gt(min(min(table$.dist)), 0)
  expect_lt(max(max(table$.dist)), 1)
})

test_that("Trace tables", {
  set.seed(42)
  run <- readRDS(test_path("./example_run.rds"))
  table <- visualize(run, type = "trace", plot = FALSE)

  expect_s3_class(table, "tbl_df")
  expect_named(table, c(".var", ".point", ".log_volume", ".weight"))
  expect_equal(unique(table$.var), colnames(run$samples))
  expect_equal(order(table$.point), order(as.vector(run$samples)))
})

test_that("Density plot", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()

  set.seed(42)
  run <- readRDS(test_path("./example_run.rds"))
  vdiffr::expect_doppelganger(
    "visualize-basic",
    visualize(run)
  )
  vdiffr::expect_doppelganger(
    "visualize-radial",
    visualize(run, radial = TRUE, vars = c(".radial"))
  )
})

test_that("Trace plot", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()

  set.seed(42)
  run <- readRDS(test_path("./example_run.rds"))
  vdiffr::expect_doppelganger(
    "visualize-trace",
    visualize(run, type = "trace")
  )
})
