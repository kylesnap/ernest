test_that("Density tables", {
  set.seed(42)
  data(example_run)
  table <- visualize(example_run, type = "density", plot = FALSE)

  expect_s3_class(table, "tbl_df")
  expect_named(table, c(".var", ".dist"))
  expect_equal(table$.var, colnames(example_run$samples))

  table <- visualize(
    example_run,
    units = "unit_cube",
    type = "density",
    plot = FALSE
  )
  expect_gt(min(min(table$.dist)), 0)
  expect_lt(max(max(table$.dist)), 1)
})

test_that("Trace tables", {
  set.seed(42)
  data(example_run)
  table <- visualize(example_run, type = "trace", plot = FALSE)

  expect_s3_class(table, "tbl_df")
  expect_named(table, c(".var", ".point", ".log_volume", ".weight"))
  expect_equal(unique(table$.var), colnames(example_run$samples))
  expect_equal(order(table$.point), order(as.vector(example_run$samples)))
})

test_that("Density plot", {
  skip_extended_test()
  set.seed(42)
  data(example_run)
  vdiffr::expect_doppelganger(
    "visualize-basic",
    visualize(example_run)
  )
  vdiffr::expect_doppelganger(
    "visualize-radial",
    visualize(example_run, radial = TRUE, vars = c(".radial"))
  )
})

test_that("Trace plot", {
  skip_extended_test()
  set.seed(42)
  data(example_run)
  vdiffr::expect_doppelganger(
    "visualize-trace",
    visualize(example_run, type = "trace")
  )
})
