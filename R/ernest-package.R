#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom Rcpp sourceCpp
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom rlang %@%
#' @importFrom rlang %@%<-
#' @importFrom rlang %|%
#' @importFrom rlang %||%
#' @importFrom tibble tibble
#' @useDynLib ernest, .registration = TRUE
## usethis namespace: end
NULL

.onLoad <- function(...) {
  S7::methods_register()
}
