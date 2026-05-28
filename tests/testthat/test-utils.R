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
