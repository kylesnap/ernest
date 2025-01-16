#' Process a list of probability transformations for ernest
#'
#' @param prior_transforms A list of objects of class "prior_transforms"
#'
#' @returns A list containing the merged probability transformation, a list
#' of parameter names, and a list of the supports.
#' @noRd
merge_transformations <- function(prior_list) {
  if (!every(prior_list, \(x) inherits(x, "prior_transform"))) {
    rlang::abort("All elements of prior_transforms must be of class 'prior_transform'")
  }
  composite_fn <- \(x) {
    map2_dbl(prior_list, x, \(pt, val) pt$fn(val))
  }
  names <- map_chr(prior_list, \(x) ifelse(is.null(x$name), "V", x$name)) |>
    make.names(unique = TRUE)
  supports <- map(prior_list, \(x) x$support)
  list(composite_fn = composite_fn, names = names, supports = supports)
}

#' =====

#' Compute an integral
#'
#'  @param log_lik Vector of log likelihoods
#'  @param log_vol Vector of log volumes
#'
#'  @return The integral
compute_integral <- function(log_lik, log_vol) {
  pad_log_lik <- c(-1e300, log_lik)
  d_log_vol <- diff(c(0, log_vol))
  log_d_vol <- log_vol - d_log_vol + log1p(-exp(d_log_vol))
  log_d_vol2 <- log_d_vol + log(0.5)

  d_log_vol <- -diff(c(0, log_vol))

  log_wt <- map2_dbl(
    tail(pad_log_lik, -1),
    head(pad_log_lik, -1),
    \(x, y) log_add_exp(x, y)
  ) + log_d_vol2

  log_z <- accumulate(log_wt, \(x, y) log_add_exp(x, y))
  log_z_max <- tail(log_z, 1)

  h_part1 <- cumsum(
    exp(tail(pad_log_lik, -1) - log_z_max + log_d_vol2)
    * tail(pad_log_lik, -1) +
      exp(head(pad_log_lik, -1) - log_z_max + log_d_vol2) *
      head(pad_log_lik, -1)
  )
  h <- h_part1 - log_z_max * exp(log_z - log_z_max)
  dh <- diff(c(0, h))
  log_z_var <- abs(cumsum(dh * d_log_vol))

  tibble::tibble(
    "log_wt" = log_wt,
    "log_z" = log_z,
    "log_z_var" = log_z_var,
    "information" = h
  )
}





