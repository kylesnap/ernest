#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import vctrs
#' @importFrom generics generate
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom utils head
#' @importFrom utils tail
#' @useDynLib ernest, .registration = TRUE
## usethis namespace: end
NULL

# CRAN flags on parallelized variables
utils::globalVariables(c("impl_", "lrps_"))
