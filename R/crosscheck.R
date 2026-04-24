#' Visually diagnose sampling issues in an `ernest_run`
#'
#' Issues in sampling caused by poor LRPS behaviour or challenging features
#' in the model's likelihood may confound nested sampling results. To identify
#' these issues, this plot tracks the insertion indicies of new points as they
#' are inserted into the live set, calculates a rolling rank sum statistic,
#' then creates a CUSUM plot highlighting potentially problematic sampling
#' iterations.
#'
#' @param run [[ernest_run]]\cr Results from a nested sampling run.
#' @param width `[integer(1)]`\cr Width of the rolling window used to compute
#' the rank-sum process. Defaults to `run$nlive` when left `NULL`.
#' @param omega `[double(1)]`\cr Reference value used to create the cusum chart.
#' Larger values make the chart less sensitive to small shifts in the rank sum
#' of the insertions.
#' @param h `[double(1)]`\cr Decision interval for out-of-bounds detection.
#' Iterations are flagged when the upper CUSUM exceeds `h` or the lower CUSUM
#' drops below `-h`.
#' @param plot `[logical(1)]`\cr If `TRUE`, returns a CUSUM `ggplot` object. If
#' `FALSE`, returns the tabular CUSUM values used to make the plot.
#'
#' @returns
#' If `plot = TRUE`, a `ggplot2::ggplot()` object.
#'
#' If `plot = FALSE`, a tibble with columns:
#' * `.iter`: `[integer()]` CUSUM iteration index.
#' * `x`: `[double()]` Rolling rank-sum statistic.
#' * `z`: `[double()]` Standardized rank-sum statistic.
#' * `.lower`: `[double()]` Lower tabular CUSUM sequence.
#' * `.upper`: `[double()]` Upper tabular CUSUM sequence.
#' * `OOB`: `[logical()]` Indicator for whether either CUSUM is outside the
#' decision interval [-h, h].
#'
#' @details
#' The diagnostic follows these steps:
#' * Compute insertion indices from the run's record of live-point
#' replacements.
#' * Build a rolling rank sum of insertion ranks using `width`.
#' * Standardize this process using the expected mean and standard deviation
#' under ideal insertion behavior.
#' * Apply a one-sided tabular CUSUM to detect sustained positive or negative
#' departures from expectation.
#'
#' Out-of-bounds (`OOB`) points indicate iterations where insertion behavior may
#' be inconsistent with well-mixed sampling in the likelihood-restricted prior.
#'
#' @seealso
#' * [plot()] for evidence, weight, and likelihood diagnostics.
#' * [visualize()] for posterior density and trace diagnostics.
#'
#' @examples
#' data(example_run)
#'
#' # CUSUM diagnostic chart
#' calculate_cusum(example_run)
#'
#' # Retrieve CUSUM values for custom plotting or thresholds
#' head(calculate_cusum(example_run, plot = FALSE))
#' @export
calculate_cusum <- function(
  run,
  width = NULL,
  omega = 1.5,
  h = 4,
  plot = TRUE
) {
  check_class(run, "ernest_run")
  width <- width %||% run$nlive
  check_number_whole(width, min = 1)
  check_number_decimal(omega, min = 0)
  check_number_decimal(h, min = 0)

  insertions <- get_insertion_indices(run$rcrd)
  subinsert <- vctrs::vec_sort(insertions[insertions$iter != 0, ])
  if (width > nrow(subinsert)) {
    cli::cli_abort(
      "`width` must be less than or equal to the number of inserted points."
    )
  }
  rs <- rank_sum(subinsert$insertion, width = width)
  # Mean and SD of an nlive-sided die!
  mean <- (run$nlive + 1) / 2
  sd <- sqrt((run$nlive^2 - 1) / 12 / width)
  df <- cusum(rs, mean, sd, omega = omega, h = h)
  if (!plot) {
    return(new_tibble0(df))
  }
  df |>
    ggplot(aes(.data$.iter)) +
    geom_line(aes(y = .data$z, colour = .data$OOB, group = 1)) +
    scale_x_continuous("Iteration") +
    scale_y_continuous(expression(z(RS))) +
    ggplot2::scale_colour_manual(
      guide = NULL,
      breaks = c(FALSE, TRUE),
      values = c("black", "red")
    )
}

#' Calculate a CUSUM plot using the tabular method
#'
#' @param x A numeric vector of values to calculate the CUSUM on.
#' @param mean,sd The expected mean and standard deviation of the values in
#' `x` under good sampling.
#' @param omega Reference value used in the tabular CUSUM recursion. Larger
#' values make the chart less sensitive to small shifts.
#' @param h Decision interval for out-of-bounds detection. Iterations are
#' flagged when the upper CUSUM exceeds `h` or the lower CUSUM drops below `-h`.
#'
#' @returns A data frame.
#' @noRd
cusum <- function(x, mean, sd, omega = 0.5, h = 5) {
  z <- (x - mean) / sd
  upper <- Reduce(
    \(prev, cur) max(0, prev + cur - omega),
    x = z,
    init = 0,
    accumulate = TRUE
  )[-1]
  lower <- Reduce(
    \(prev, cur) min(0, prev + cur + omega),
    x = z,
    init = 0,
    accumulate = TRUE
  )[-1]
  data_frame0(
    ".iter" = seq_along(x),
    "x" = x,
    "z" = z,
    ".lower" = lower,
    ".upper" = upper,
    "OOB" = upper > h | lower < -h
  )
}

#' Calculate a rolling rank sum along samples
#'
#' @param samples A numeric vector or matrix of samples. If a matrix, the
#' rank sum is calculated along rows.
#' @param width The width of the rolling window to calculate the rank sum over.
#'
#' @return A numeric vector or matrix with `width` fewer columns than `samples`,
#' containing the rolling rank sums of the most recent `width` samples at each
#' position.
#' @noRd
rank_sum <- function(samples, width) {
  dims <- dim(samples) %||% c(1, length(samples))
  cum_sums <- matrix(0, nrow = dims[[1]], ncol = dims[[2]] + 1)
  cum_sums[, -1] <- matrixStats::rowCumsums(samples, dim. = dims)
  lead <- vctrs::num_as_location(seq(width + 1, dims[2] + 1), n = dims[2] + 1)
  lag <- vctrs::num_as_location(seq(1, dims[2] - width + 1), n = dims[2] + 1)
  1 / width * (cum_sums[, lead] - cum_sums[, lag])
}

#' Get the insertion index of each point into the live set
#'
#' @param rcrd An `ernest_rcrd` object.
#'
#' @returns A data frame with these columns:
#' * "iter": The iteration of when this point was inserted into the live set.
#' * "id": The id of the point.
#' * "insertion": The rank of the point in the live set at the time of
#' insertion, based on likelihood values among alive points.
#' @noRd
get_insertion_indices <- function(rcrd) {
  check_class(rcrd, "ernest_rcrd")

  # Iteration: Comes from the iteration of its birth (which is the same as the
  # death of that ID's previous incarnation).
  group_locs <- vctrs::vec_group_loc(field(rcrd, "id"))
  group_idx <- lapply(group_locs$loc, \(x) c(0, x[-length(x)]))
  iter <- vctrs::list_combine(
    group_idx,
    indices = group_locs$loc,
    size = length(rcrd)
  )
  rm(group_locs, group_idx)

  # Find points who were born before needle, and died after needle
  matches <- vctrs::vec_locate_matches(
    needles = data_frame0("a" = iter, "b" = iter),
    haystack = data_frame0("a" = iter, "b" = seq_along(rcrd)),
    condition = c(">=", "<")
  )
  log_lik <- field(rcrd, "log_lik")
  insertions <- vapply(
    vctrs::vec_split(matches, matches$needles)$val,
    \(x) {
      r <- rank(log_lik[x$haystack], ties.method = "min")
      r[match(x$needles[[1]], x$haystack)]
    },
    integer(1)
  )
  rm(matches, log_lik)
  gc()

  data_frame0(
    "iter" = iter,
    "id" = field(rcrd, "id"),
    "insertion" = insertions
  )
}
