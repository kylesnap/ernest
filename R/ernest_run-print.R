#' Format an ernest run object for printing
#'
#' @param x An ernest run object.
#' @param ... Ignored.
#'
#' @export
S7::method(format, ErnestRun) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cli::cli_format_method({
      cli::cli_h1("{.pkg Ernest} Nested Sampling Run")
      cli::cli_ul(c(
        "Live Points: {x@n_points}",
        "Iterations: {x@n_iter}",
        "Calls: {x@n_call}"
      ))
    })
  }

#' Print an ernest run object
#'
#' @param x An ernest run object.
#' @param digits Number of significant digits to print.
#' @param ... Ignored.
#'
#' @returns `x`, invisibly.
#' @export
S7::method(print, ErnestRun) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cat(format(x, digits = digits), sep = "\n")
    invisible(x)
  }
