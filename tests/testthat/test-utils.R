test_that("check_class works as expected", {
  expect_invisible(check_class(structure(list(), class = "foo"), "foo"))
  expect_invisible(
    check_class(structure(list(), class = c("foo", "bar")), "foo")
  )
  expect_invisible(
    check_class(structure(list(), class = c("foo", "bar")), c("foo", "bar"))
  )
  expect_invisible(check_class(NULL, "foo", allow_null = TRUE))
  expect_snapshot(check_class(1, "foo"), error = TRUE)
  expect_error(check_class(NULL, "foo", allow_null = FALSE), "not `NULL`")
})

test_that("check_unique_names works as expected", {
  expect_invisible(check_unique_names(list(a = 1, b = 2)))
  expect_snapshot(check_unique_names(list(a = 1, a = 2)), error = TRUE)
  expect_error(check_unique_names(list(1, 2)), "unique names")
  expect_error(check_unique_names(list(a = 1, 2)), "unique names")
})
