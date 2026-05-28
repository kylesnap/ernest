#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom generics generate
#' @importFrom lifecycle deprecated
#' @importFrom utils head
#' @importFrom utils tail
#' @useDynLib ernest, .registration = TRUE
## usethis namespace: end
NULL

# Globals for parallelization.R
utils::globalVariables(c("nested_sampling_impl_", "lrps_"))
