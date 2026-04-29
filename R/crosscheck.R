#' Visually examine the sampling behaviour of a nested sampling run
#'
#' @param x [[ernest_run]]\cr An 'ernest_run' object.
#' @param size `[[integer(1)]]`\cr The number of points to include in each
#' split. If `NULL`, this defaults to `x$nlive`. If larger than `x$niter`, the
#' run is not split.
#' @param conf.level `[[double(1)]]`\cr Confidence level for the plotted
#' confidence intervals.
#' @param data_only `[[logical(1)]]`\cr Whether to automatically plot the
#' results. If `FALSE`, the plotted data is instead returned to the user.
#'
#' @rdname crosscheck_ernest
#' @importFrom ggplot2 geom_step
#' @export
crosscheck_plot <- function(
  x,
  size = NULL,
  conf.level = 0.95, # nolint
  data_only = FALSE,
  ...
) {
  check_class(x, "ernest_run")
  check_number_decimal(conf.level)
  if (conf.level <= 0.5 || conf.level >= 1) {
    stop_input_type(conf.level, "a single number between 0.5 and 1")
  }
  check_bool(data_only)

  splits <- crosscheck_prep(x, size)
  null <- calculate_ecdf_ci(length(splits[[1]]), x$nlive, 1 - conf.level)
  cumsums <- lapply(
    splits,
    \(x, nlive) {
      data_frame0(
        !!!null,
        ".y" = c(0, cumsum(tabulate(x, nbins = nlive)))
      )
    },
    nlive = x$nlive
  )
  names(cumsums) <- names(splits)
  cdf <- vctrs::vec_rbind(!!!cumsums, .names_to = "idx")
  if (data_only) {
    return(cdf)
  }
  ggplot(cdf, aes(x = .data$.x)) +
    geom_step(
      aes(
        y = .data$.y / .data$.n - .data$.p,
        group = factor(.data$idx),
        colour = factor(.data$idx)
      )
    ) +
    geom_step(
      aes(y = .data$.lower / .data$.n - .data$.p),
      colour = "gray60"
    ) +
    geom_step(
      aes(y = .data$.upper / .data$.n - .data$.p),
      colour = "gray60"
    ) +
    ggplot2::annotate("segment", x = 0, y = 0, xend = x$nlive) +
    ggplot2::labs(
      x = "Insertion Index",
      y = bquote(hat(F[n](k)) - F[n](k)),
      colour = "Iterations"
    )
}

#' @param type `[[character(1)]]`\cr The type of test statistic to use for the
#' uniformity test. One of "W2" (Cramer-von Mises), "A2" (Anderson-Darling),
#' "ks" (Kolmogorov-Smirnov). Tests are not performed if `type` is `NA`.
#' @importFrom stats p.adjust.methods
#'
#' @rdname crosscheck_ernest
#' @export
crosscheck_tests <- function(
  x,
  size = NULL,
  type = c("W2", "A2", "ks"),
  p.adjust = p.adjust.methods,
  ...
) {
  check_class(x, "ernest_run")
  check_installed("dgof", "to perform uniformity tests")
  y_ecdf <- stats::ecdf(1:x$nlive)
  test_fn <- switch(
    arg_match(type),
    "W2" = \(x) dgof::cvm.test(x = x, y = y_ecdf, type = "W2", ...),
    "A2" = \(x) dgof::cvm.test(x = x, y = y_ecdf, type = "A2", ...),
    "ks" = \(x) dgof::ks.test(x = x, y = y_ecdf, ...),
  )

  splits <- crosscheck_prep(x, size)
  stats <- lapply(
    splits[-length(splits)],
    \(x) {
      tst <- test_fn(x)
      c("statistic" = tst[["statistic"]], "p.value" = tst[["p.value"]])
    }
  )
  stats <- vctrs::vec_rbind(!!!stats, .names_to = "split")

  if (nrow(stats) > 1) {
    stats$adj.p.value <- stats::p.adjust(stats$p.value, method = p.adjust)
  }
  new_tibble0(stats)
}

#' Get the insertion indicies and slice by `size`
crosscheck_prep <- function(x, size, call = caller_env()) {
  check_class(x, "ernest_run", call = call)
  indices <- get_insertion_indices(x$rcrd)
  observed <- indices$insertion_idx[sort(indices$iter)]

  size <- size %||% x$nlive
  check_number_whole(size, min = 1, call = call)
  if (size > vctrs::vec_size(observed)) {
    size <- vctrs::vec_size(observed)
  }
  times <- vctrs::vec_size(observed) %/% size
  rem <- vctrs::vec_size(observed) %% size
  splits <- vctrs::vec_chop(
    observed,
    sizes = c(rep(size, times), rem)
  )
  names(splits)[-length(splits)] <- sprintf(
    "[%d, %d]",
    seq_len(times) * size - size + 1,
    seq_len(times) * size
  )
  splits[-length(splits)]
}

#' Get the insertion index of each point into the live set
#'
#' @param rcrd An `ernest_rcrd` object.
#'
#' @return An integer vector, the same length as `run$rcrd`. Each element is
#' between 1 and run$nlive, inclusive, and indicates the index of the live set
#' into which the corresponding point in `run$rcrd` was inserted.
#' @noRd
get_insertion_indices <- function(rcrd) {
  needles <- data_frame0(
    b = field(rcrd, "birth_lik"),
    d = field(rcrd, "birth_lik")
  )
  haystack <- data_frame0(
    b = field(rcrd, "birth_lik"),
    d = field(rcrd, "log_lik")
  )

  # Born no later than `needle`, and not yet dead at `needle's` birth
  alive <- vctrs::vec_locate_matches(
    needles,
    haystack,
    condition = c(">=", "<")
  )
  live_history <- vctrs::vec_split(alive$haystack, alive$needles)

  insertions <- mapply(
    \(i, idx) {
      r <- rank(haystack$d[idx], ties.method = "first")
      m <- match(i, idx)
      if (is.na(m)) 1L else as.integer(r[m])
    },
    i = live_history$key,
    idx = live_history$val,
    USE.NAMES = FALSE,
    SIMPLIFY = TRUE
  )

  # Get indexes of each birth
  matches <- vctrs::vec_locate_matches(field(rcrd, "id"), field(rcrd, "id"))
  idx <- vapply(
    vctrs::vec_split(matches, by = matches$needles)$val,
    \(x) {
      if (any(x$haystack < x$needles)) {
        max(x$haystack[x$haystack < x$needles])
      } else {
        NA_integer_
      }
    },
    integer(1)
  )

  df <- data_frame0(
    iter = idx,
    id = field(rcrd, "id"),
    birth_lik = field(rcrd, "birth_lik"),
    insertion_idx = insertions
  )
  df <- df[!is.na(df$iter), ]
  row.names(df) <- vctrs::vec_seq_along(df)
  df
}

#' Get the CI for an ECDF assuming a normal distribution
#'
#' @param nsamp Number of samples in the ECDF
#' @param nlive Number of live points in the run
#' @param alpha Significance level for the confidence interval.
#'
#' @return A data frame with columns `.x`, `.p`, `.lower`, `.upper`, and `.n`.
#' @noRd
calculate_ecdf_ci <- function(nsamp, nlive, alpha) {
  check_number_whole(nsamp, min = 1)
  check_number_whole(nlive, min = 1)
  check_number_decimal(alpha, min = 0, max = 0.5)
  ndraws <- getOption("posterior.rvar_ndraws", 4000L)
  check_number_whole(ndraws, min = 1)
  # (a) Simulate M draws of N realizations of DisUnif(1, K) and its ECDF
  ecdf_mat <- vapply(
    seq_len(ndraws),
    \(i) {
      tabulate(sample.int(nlive, size = nsamp, replace = TRUE), nbins = nlive)
    },
    numeric(nlive)
  )
  ecdf_mat <- matrixStats::colCumsums(ecdf_mat)
  # (b) For all i in K, Calculate l = Bin(NF(p_i); N, p_i)
  # u = Bin(NF(p_i) - 1; N, p_i)
  probs <- seq(nlive) / nlive
  l <- stats::pbinom(ecdf_mat, size = nsamp, prob = probs)
  u <- stats::pbinom(
    ecdf_mat - 1,
    size = nsamp,
    prob = probs,
    lower.tail = FALSE
  )
  # (c) Calculate gamma_m = 2 min_k(min(l, 1 - u)) and report gamma as the
  # (1 - alpha) quantile of gamma_m
  mins <- pmin(l, u)
  mins <- 2 * matrixStats::colMins(mins)
  gamma <- quantile(mins, alpha)
  data_frame0(
    .x = seq(0, nlive),
    .p = seq(0, nlive) / nlive,
    .lower = c(0, qbinom(gamma / 2, nsamp, seq(nlive) / nlive)),
    .upper = c(0, qbinom(1 - gamma / 2, nsamp, seq(nlive) / nlive)),
    .n = nsamp
  )
}
