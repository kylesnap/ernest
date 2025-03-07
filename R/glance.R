#' Glance at an `ErnestSampler` object
#'
#' Construct a single row summary of a given ernest run object
#'
#' @param x An `ErnestSampler` object
#' @param ... Must be left empty.
#'
#' @returns A single-row [tibble::tibble()] summarizing the run.
#' @export
glance.ErnestSampler <- function(x, ...) {
  check_dots_empty(...)
  integral <- x$wrk$integral_rcrd
  integral_summary <- if (!is_null(integral) && !attr(integral, "partial")) {
    tibble::tibble_row(
      "log_z" = tail(vctrs::field(integral, "log_z"), 1),
      "log_z_err" = sqrt(tail(vctrs::field(integral, "log_z_var"), 1)),
      "information" = tail(vctrs::field(integral, "h"), 1),
    )
  } else {
    NULL
  }
  vctrs::vec_cbind(
    tibble::tibble_row(
      "n_dim" = x$n_dim,
      "n_points" = x$n_points,
      "n_iter" = x$wrk$n_iter %||% 0,
      "n_call" = x$wrk$n_call %||% 0,
      "eff" = if (.data$n_call == 0) NULL else .data$n_iter/.data$n_call,
    ),
    integral_summary
  )
}
