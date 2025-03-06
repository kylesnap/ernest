#' @importFrom generics generate
#' @export
generics::generate

#' @importFrom generics compile
#' @export
generics::compile

#' @importFrom generics calculate
#' @export
generics::calculate

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom posterior as_draws
#' @export
posterior::as_draws

#' @importFrom posterior as_draws_matrix
#' @export
posterior::as_draws_matrix

#' @importFrom posterior as_draws_df
#' @export
posterior::as_draws_df

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom generics generate
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang %@%
#' @importFrom rlang %@%<-
#' @importFrom rlang %|%
#' @importFrom rlang %||%
#' @importFrom tibble tibble
#' @importFrom utils head
#' @importFrom utils tail
#' @useDynLib ernest, .registration = TRUE
## usethis namespace: end
NULL
