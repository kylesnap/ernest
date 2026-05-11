withr::local_seed(42)

test_that("visualize validates parameters", {
  data(example_run)
  expect_error(
    visualize(example_run, q),
    "Column `q` doesn't exist."
  )

  expect_error(
    visualize(example_run, .which = "plot"),
    '"density" or "trace"'
  )

  expect_error(
    visualize(example_run, .units = "x"),
    '"original" or "unit_cube"'
  )
})

skip_plot_snapshot()
test_that("Density plot", {
  data(example_run)
  vdiffr::expect_doppelganger(
    "visualize-basic",
    visualize(example_run)
  )

  vdiffr::expect_doppelganger(
    "visualize-x",
    visualize(example_run, x)
  )

  vdiffr::expect_doppelganger(
    "visualize-yz",
    visualize(example_run, y, z, .units = "unit_cube")
  )
})

test_that("Trace plot", {
  data(example_run)
  vdiffr::expect_doppelganger(
    "trace-basic",
    visualize(example_run, .which = "trace")
  )

  vdiffr::expect_doppelganger(
    "trace-x",
    visualize(example_run, x, .which = "trace")
  )

  vdiffr::expect_doppelganger(
    "trace-yz",
    visualize(example_run, y, z, .which = "trace", .units = "unit_cube")
  )
})
