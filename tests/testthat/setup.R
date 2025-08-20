#' @srrstats {BS2.13} By default, all testing is done with disabled messages, but
#' warnings are not silenced.
withr::local_options(
  list(rlib_message_verbosity = "quiet", ernest.extended_tests = TRUE),
  .local_envir = teardown_env()
)
