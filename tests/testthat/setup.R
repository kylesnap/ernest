#' @srrstats {BS2.13} By default, all testing is done with disabled messages,
#' but warnings are not silenced.
withr::local_options(
  list(rlib_message_verbosity = "quiet", rgl.useNULL = TRUE),
  .local_envir = teardown_env()
)

#' Skip extended tests using the "ERNEST_EXTENDED_TESTS" variable.
skip_extended <- function() {
  skip_if(
    isFALSE(as.logical(Sys.getenv("ERNEST_EXTENDED_TESTS", "true"))),
    "Extended test"
  )
}
