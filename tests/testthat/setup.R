#' @srrstats {BS2.13} By default, all testing is done with disabled messages, but
#' warnings are not silenced.
local_options(
  rlib_message_verbosity = "quiet",
  ernest.extended_tests = TRUE,
  .frame = teardown_env()
)
