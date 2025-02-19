#' Encode an ernest object
#'
#' Format an object from ernest in pretty printing
#'
#' @param x An ernest run object.
#' @param digits The number of significant digits to be used while printing.
#'
#' @returns An object containing a character representation of x.
#' @name format
NULL

#' @export
S7::method(print, ErnestLRPS) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cat(format(x, digits = digits), sep = "\n")
    invisible(x)
  }

#' Print an ernest object
#'
#' @param x An ernest run object.
#' @param digits The number of significant digits to be used while printing.
#' @name print
NULL
