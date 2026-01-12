#' Configure logging for ernest runs
#'
#' Set up log for ernest. #' "INFO"-level logging saves the results of each call
#' to [update_lrps]; "DEBUG"-level logging additionally saves the results of
#' each call to [propose].
#'
#' @param dir Character or `FALSE.` Directory in which to create the log file.
#' This directory must already exist. If `FALSE`, logging is disabled.
#' @param threshold Character. Minimum message level to record, one of
#' "INFO", "DEBUG", "WARN", "ERROR", or "FATAL".
#' @param layout Character. Log output format, either "json" or "logfmt".
#'
#' @return
#' A list with S3 class "ernest_logging" if `dir` is a valid directory, or
#' NULL if `dir` is FALSE. The list contains arguments used to create an
#' instance of [log4r::logger] when [generate] is called. The return value is
#' also stored in the R option 'ernest_logging'.
#'
#' @srrstats {G4.0} Users can choose the location of logging files.
#'
#' @keywords internal
#' @export
ernest_logging <- function(
  dir = tempdir(),
  threshold = "INFO",
  layout = c("json", "logfmt")
) {
  # Capture the unevaluated expression for comparison
  dir_expr <- enquo(dir)

  if (isFALSE(dir)) {
    options("ernest_logging" = NULL)
    return(NULL)
  }
  check_installed("log4r", reason = "writing log files")

  dir <- if (identical(dir_expr, quote(tempdir()))) {
    tempdir()
  } else if (!dir.exists(dir)) {
    cli::cli_warn("Can't find the filepath `dir`. Using {.fn tempdir} instead.")
    tempdir()
  } else {
    normalizePath(dir)
  }
  threshold <- arg_match(
    threshold,
    values = c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  )
  layout <- arg_match(layout)
  file_ext <- switch(layout, "json" = ".json", "logfmt" = ".file")
  layout <- switch(
    layout,
    "json" = log4r::json_log_layout(),
    "logfmt" = log4r::logfmt_log_layout()
  )

  config <- structure(
    list(threshold = threshold, dir = dir, fileext = file_ext, layout = layout),
    class = "ernest_logging"
  )
  options("ernest_logging" = config)
  config
}

#' Simple format for ernest_logging
#' @noRd
#' @export
format.ernest_logging <- function(x, ...) {
  cli::cli_format_method({
    cli::cli_text("logfile configuration {.cls ernest_logging}")
    cli::cli_text("Directory: {.path {x$dir}}")
    cli::cli_text("Threshold: {x$threshold}")
  })
}

#' Simple print for ernest_logging
#' @noRd
#' @export
print.ernest_logging <- function(x, ...) {
  print(format(x, ...), sep = "\n")
}

#' Configure logging when `generate` is called
#' @noRd
start_logging <- function() {
  if (is.null(config <- getOption("ernest_logging"))) {
    return(NULL)
  }
  check_class(config, "ernest_logging")
  file <- file.path(
    config$dir,
    paste0(
      "ernest_generate_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      config$fileext
    )
  )
  log4r::logger(
    threshold = config$threshold,
    appenders = log4r::file_appender(file, layout = config$layout)
  )
}

#' Check the class of an object
#'
#' Validates that an object inherits from at least one of the specified classes.
#'
#' @param x An object to check.
#' @param class A character vector of allowed classes.
#' @param ... Additional arguments passed to error handlers.
#' @param allow_null Logical. If TRUE, allows NULL values.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if `x` inherits from one of the specified
#' classes, otherwise throws an informative error.
#' @noRd
check_class <- function(
  x,
  class,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if ((allow_null && is.null(x)) || inherits_any(x, class)) {
    return(invisible(NULL))
  }

  cls_format <- cli::pluralize(
    "an object with {?class/at least one class from} {class}"
  )
  stop_input_type(
    x,
    cls_format,
    ...,
    allow_na = FALSE,
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

#' Check that an object is a double matrix
#'
#' Validates that an object is a double matrix with specified dimensions
#' and within given bounds.
#'
#' @param x An object to check.
#' @param nrow Expected number of rows.
#' @param ncol Expected number of columns.
#' @param lower Numeric. Exclusive lower bounds, recycled to length `ncol`.
#' @param upper Numeric. Exclusive upper bounds, recycled to length `ncol`.
#' @param ... Additional arguments passed to error handlers.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if `x` is a valid double matrix,
#' otherwise throws an informative error.
#' @noRd
check_matrix <- function(
  x,
  nrow,
  ncol,
  lower = -Inf,
  upper = Inf,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_number_whole(nrow, min = 1, call = call)
  check_number_whole(ncol, min = 1, call = call)
  bounds <- vctrs::vec_recycle_common(
    "lower" = lower,
    "upper" = upper,
    .size = ncol
  )

  if (!is.double(x) || !is.matrix(x)) {
    stop_input_type(
      x,
      "a double matrix",
      ...,
      allow_na = FALSE,
      allow_null = FALSE,
      arg = arg,
      call = call
    )
  }
  if (!isTRUE(all.equal(dim(x), c(nrow, ncol)))) {
    cli::cli_abort(
      c(
        "`{arg}` must have dimensions {nrow} x {ncol}.",
        "x" = "Actual dimensions: {nrow(x)} x {ncol(x)}."
      ),
      arg = arg,
      call = call
    )
  }

  for (i in seq(nrow(x))) {
    if (any(is.na(x[i, ]) | is.nan(x[i, ]))) {
      cli::cli_abort(
        "`{arg}` must not contain missing or `NaN` values.",
        call = call
      )
    }
    if (any(x[i, ] <= bounds$lower)) {
      cli::cli_abort(
        "`{arg}` must respect the lower boundary ({lower}).",
        call = call
      )
    }
    if (any(x[i, ] >= bounds$upper)) {
      cli::cli_abort(
        "`{arg}` must respect the upper boundary ({upper}).",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' Check that a list has unique, non-empty names
#'
#' Validates that all elements of a list are named and that names are unique.
#'
#' @param x A list to check.
#' @param ... Additional arguments passed to error handlers.
#' @param arg Argument name for error messages.
#' @param call Call environment for error messages.
#'
#' @return Returns NULL invisibly if all names are unique and non-empty,
#' otherwise throws an informative error.
#' @noRd
check_unique_names <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  nms <- vctrs::vec_names(x)
  if (is.null(nms) != any(nms == "")) {
    cli::cli_abort(
      "All elements of `{arg}` must have unique names.",
      call = call
    )
  }

  if (vctrs::vec_duplicate_any(nms)) {
    idx <- vctrs::vec_duplicate_id(nms) |> unique()
    cli::cli_abort(
      c(
        "All elements of `{arg}` must have unique names.",
        "x" = "Repeated names: {nms[idx]}"
      ),
      call = call
    )
  }

  invisible(NULL)
}

#' Inject a list into a vector
#'
#' Concatenates the elements of a list into a single vector.
#'
#' @param x A list to concatenate.
#'
#' @return A vector containing all elements of the list.
#' @noRd
list_c <- function(x) {
  inject(c(!!!x))
}

# Helpers for computing and reporting results -----

#' Compute the nested sampling integral and statistics
#'
#' Calculates the nested sampling integral and related statistics from
#' log-likelihoods and log-volumes.
#'
#' @param log_lik Numeric vector of log-likelihoods in descending order.
#' @param log_volume Numeric vector of log-volumes in ascending order.
#'
#' @return A list containing log-likelihoods, log-volumes, log-weights,
#' log-evidence, log-evidence variance, and information.
#' @noRd
compute_integral <- function(log_lik, log_volume) {
  if (vctrs::vec_size_common(log_lik, log_volume) == 0L) {
    return(list(
      log_lik = double(0),
      log_volume = double(0),
      log_weight = double(0),
      log_evidence = double(0),
      log_evidence_var = double(0),
      information = double(0)
    ))
  }
  log_weight <- drop(get_logweight(log_lik, log_volume))
  log_evidence <- drop(get_logevid(log_weight))
  information <- get_information(log_lik, log_volume, log_evidence)

  # Estimate the error around logz
  dh <- c(information[1], diff(information))
  log_evidence_var <- abs(cumsum(dh * -diff(c(0, log_volume))))

  vctrs::vec_cast_common(
    log_lik = log_lik,
    log_volume = log_volume,
    log_weight = log_weight,
    log_evidence = log_evidence,
    log_evidence_var = log_evidence_var,
    information = information,
    .to = double()
  )
}
