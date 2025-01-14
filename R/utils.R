#' Process a list of probability transformations for ernest
#'
#' @param prior_transforms A list of objects of class "prior_transforms"
#'
#' @returns A list containing the merged probability transformation, a list
#' of parameter names, and a list of the supports.
#' @noRd
merge_transformations <- function(prior_transforms) {
  if (!every(prior_transforms, \(x) inherits(x, "prior_transform"))) {
    rlang::abort("All elements of prior_transforms must be of class 'prior_transform'")
  }

  composite_fn <- \(x) {
    map2_dbl(prior_transforms, x, \(pt, val) pt$fn(val))
  }

  names <- map_chr(prior_transforms, \(x) ifelse(is.null(x$name), "V", x$name)) |>
    make.names(unique = TRUE)
  supports <- map(prior_transforms, \(x) x$support)

  list(composite_fn = composite_fn, names = names, supports = supports)
}
