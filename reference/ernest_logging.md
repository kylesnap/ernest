# Configure logging for ernest runs

Set up log for ernest. \#' "INFO"-level logging saves the results of
each call to
[update_lrps](https://kylesnap.github.io/ernest/reference/update_lrps.md);
"DEBUG"-level logging additionally saves the results of each call to
[propose](https://kylesnap.github.io/ernest/reference/propose.md).

## Usage

``` r
ernest_logging(
  dir = tempdir(),
  threshold = "INFO",
  layout = c("json", "logfmt")
)
```

## Arguments

- dir:

  Character or `FALSE.` Directory in which to create the log file. This
  directory must already exist. If `FALSE`, logging is disabled.

- threshold:

  Character. Minimum message level to record, one of "INFO", "DEBUG",
  "WARN", "ERROR", or "FATAL".

- layout:

  Character. Log output format, either "json" or "logfmt".

## Value

A list with S3 class "ernest_logging" if `dir` is a valid directory, or
NULL if `dir` is FALSE. The list contains arguments used to create an
instance of [log4r::logger](https://rdrr.io/pkg/log4r/man/logger.html)
when [generate](https://generics.r-lib.org/reference/generate.html) is
called. The return value is also stored in the R option
'ernest_logging'.
