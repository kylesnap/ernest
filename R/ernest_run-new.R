#' An S7 Class, Containing a Nested Sampling Run
#'
#' @slot sampler An object of class `ErnestLRPS`.
#' @slot dead_points A list containing two matrices: "units" contains
#' the dead points expressed in the sampled unit hypercube, and "points"
#' contains the points expressed in original parameter space.
#'
#' @slot n_iter The number of iterations in the run.
#' @slot n_call The number of likelihood calls in the run.
#' @slot time The time taken to run the sampler.
#'
#' @include ernest_lrps-new.R
ErnestRun <- S7::new_class(
  "ErnestRun",
  properties = list(
    sampler = ErnestLRPS,
    dead_points = S7::class_list,
    integral = S7::new_S3_class("ErnestIntegral"),
    progress = S7::new_S3_class("ErnestProg"),
    n_iter = S7::new_property(
      getter = function(self) {
        tail(vctrs::field(self@progress, "iter"), 1L)
      }
    ),
    n_call = S7::new_property(
      getter = function(self) {
        sum(vctrs::field(self@progress, "calls"))
      }
    ),
    log_z = S7::new_property(
      getter = function(self) {
        tail(vctrs::field(self@integral, "log_z"), 1L)
      }
    ),
    log_z_var = S7::new_property(
      getter = function(self) {
        tail(vctrs::field(self@integral, "log_z_var"), 1L)
      }
    ),
    information = S7::new_property(
      getter = function(self) {
        tail(vctrs::field(self@integral, "information"), 1L)
      }
    )
  ),
  constructor = function(sampler) {
    dead <- list(
      "units" = do.call(rbind, sampler@wrk$units),
      "points" = do.call(rbind, sampler@wrk$points)
    )
    S7::new_object(
      S7::S7_object(),
      sampler = sampler,
      dead_points = dead,
      integral = new_ernest_integral(sampler),
      progress = new_ernest_prog(sampler)
    )
  }
)

#' @name format
#' @importFrom prettyunits pretty_dt
#' @importFrom prettyunits pretty_signif
#' @export
S7::method(format, ErnestRun) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cli::cli_format_method({
      cli::cli_h1("Nested Sampling Run from {.pkg ernest}")
      cli::cli_bullets_raw(format(x@sampler, digits = digits))
      cli::cli_h3("Results")
      cli::cli_dl(c(
        "Iterations" = "{x@n_iter}",
        "Calls" = "{x@n_call}",
        "Time" = "{pretty_dt(attr(x@progress, 'time'))}",
        "Log. Z" = "{pretty_signif(x@log_z)} \U00B1 {pretty_signif(sqrt(x@log_z_var))}"
      ))
    })
  }

#' @name print
#' @export
S7::method(print, ErnestRun) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cat(format(x, digits), sep = "\n")
    invisible(x)
  }
