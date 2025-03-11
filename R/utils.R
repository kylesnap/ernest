# Helper Functions -----
validate_integer_parameter <- function(x, multiplicand, min = NULL) {
  x <- if (is.integer(x)) {
    x
  } else {
    check_number_decimal(x, allow_infinite = FALSE)
    as.integer(x * multiplicand)
  }
  check_number_whole(x, min = min, allow_infinite = FALSE)
  as.integer(x)
}

#' Check ptype dispatches over character vectors, integers, and data.frame
#'
#' @noRd
make_ptype <- function(x, ...) {
  UseMethod("make_ptype")
}

#' If x is a character vector, return an empty tibble with colnames set to x
#' @noRd
#' @export
make_ptype.character <- function(x, ...) {
  x <- vctrs::vec_as_names(x, repair = "universal_quiet")
  tibble::as_tibble(
    matrix(numeric(), nrow = 0, ncol = length(x), dimnames = list(NULL, x))
  )
}

#' If x is a scalar double, return a x-column tibble with cols index after X
#' @noRd
#' @export
make_ptype.numeric <- function(x, ...) {
  check_number_whole(x, min = 1, allow_infinite = FALSE)
  names <- vctrs::vec_as_names(rep("X", x), repair = "universal_quiet")
  tibble::as_tibble(
    matrix(numeric(), nrow = 0, ncol = x, dimnames = list(NULL, names))
  )
}

#' If x is a data.frame, validate its size (based on a hardhat function)
#' @noRd
#' @export
make_ptype.data.frame <- function(x, ...) {
  if (tibble::is_tibble(x) && nrow(x) == 0L) {
    return(x)
  }

  if (!tibble::is_tibble(x)) {
    stop_input_type(
      x = x,
      what = "a tibble",
      arg = "ptype"
    )
  }

  size <- nrow(x)
  cli::cli_abort("ptype must be size 0, not size {size}.")
}

#' Compute the nested sampling integral
#'
#' @param log_lik A vector of log-likelihoods in descending order.
#' @param log_vol A vector of log-volumes in ascending order.
#' @param n_iter The number of iterations used to compute the integral.
#'
#' @return A list with the following components:
#' log_z: Log. evidence
#' log_z_var: Variance of the log. evidence
#' h: Information
#' dh: Differential information
#'
#' @noRd
compute_integral <- function(log_lik, log_vol, n_iter) {
  if (length(log_lik) != length(log_vol)) {
    cli::cli_abort("`log_lik` and `log_vol` must have the same length.")
  }
  partial_msg <- if (is.unsorted(log_lik)) {
    "`log_lik` should be a vector in ascending order."
  } else if (is.unsorted(rev(log_vol), strictly = TRUE)) {
    "`log_vol` should be a vector in strictly ascending order."
  } else { NULL }

  if (!is.null(partial_msg)) {
    cli::cli_warn("{partial_msg}\nReturning run information without evidence estimates.")
    return(vctrs::new_rcrd(
      list("log_lik" = log_lik, "log_vol" = log_vol),
      partial = TRUE,
      n_iter = n_iter
    ))
  }

  pad_log_lik <- c(-1e300, log_lik)
  d_log_vol <- diff(c(0, log_vol))
  log_d_vol <- log_vol - d_log_vol + log(-expm1(d_log_vol))
  log_d_vol2 <- log_d_vol - log(2)
  d_log_vol <- -diff(c(0, log_vol))

  log_wt <- mapply(
    \(lik1, lik2, ldv) logaddexp(lik1, lik2) + ldv,
    tail(pad_log_lik, -1),
    head(pad_log_lik, -1),
    log_d_vol2
  )

  log_z <- accumulate(log_wt, \(cur, nxt) logaddexp(cur, nxt))
  log_z_max <- tail(log_z, 1)
  h_term <- cumsum(
    exp(tail(pad_log_lik, -1) - log_z_max + log_d_vol2) * tail(pad_log_lik, -1) +
      exp(head(pad_log_lik, -1) - log_z_max + log_d_vol2) * head(pad_log_lik, -1)
  )
  h <- h_term - log_z_max * exp(log_z - log_z_max)
  dh <- diff(c(0, h))
  log_z_var <- abs(cumsum(dh * d_log_vol))

  vctrs::new_rcrd(
    list(
      "log_lik" = log_lik,
      "log_vol" = log_vol,
      "log_weight" = log_wt,
      "log_z" = log_z,
      "log_z_var" = log_z_var,
      "h" = h
    ),
    n_iter = n_iter,
    partial = FALSE
  )
}
