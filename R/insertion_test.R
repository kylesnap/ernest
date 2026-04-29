#' Get the insertion index of each point into the live set
#'
#' @param run An 'ernest_run' object.
#'
#' @return An integer vector, the same length as `run$rcrd`. Each element is
#' between 1 and run$nlive, inclusive, and indicates the index of the live set
#' into which the corresponding point in `run$rcrd` was inserted.
#' @noRd
get_insertion_indices <- function(run) {
  needles <- data_frame0(
    b = field(run$rcrd, "birth_lik"),
    d = field(run$rcrd, "birth_lik")
  )

  haystack <- data_frame0(
    b = field(run$rcrd, "birth_lik"),
    d = field(run$rcrd, "log_lik")
  )

  # Born no later than `needle`, and not yet dead at `needle` birth
  alive <- vctrs::vec_locate_matches(
    needles,
    haystack,
    condition = c(">=", "<")
  )
  live_history <- vctrs::vec_split(alive$haystack, alive$needles)

  insertions <- mapply(
    \(i, idx) {
      r <- rank(haystack$d[idx], ties.method = "first")
      as.integer(r[match(i, idx)])
    },
    i = live_history$key,
    idx = live_history$val,
    USE.NAMES = FALSE,
    SIMPLIFY = TRUE
  )
  data_frame0(
    log_lik = field(run$rcrd, "log_lik"),
    birth_lik = field(run$rcrd, "birth_lik"),
    id = field(run$rcrd, "id"),
    rank = insertions
  )
}

#' Get ECDF of Uniformity Plot
get_insertion_ecdf <- function(indices, nlive, binwidth = 0, conf = 0.95) {
  indices <- indices[order(indices$birth_lik), , drop = FALSE]
  indices <- indices[indices$birth_lik != -Inf, , drop = FALSE]
  indices$iteration <- seq_len(nrow(indices))

  if (nrow(indices) == 0) {
    cli::cli_abort("No insertion indices selected by `slices`.")
  }
  indices$p <- indices$rank / nlive
  probs <- seq(0, 1, length.out = nlive + 1)
  binwidth <- nlive * binwidth
  if (binwidth < 1) {
    binwidth <- vctrs::vec_size(indices)
  }
  slice_n <- vctrs::vec_size(indices) %/% binwidth
  slice_r <- vctrs::vec_size(indices) %% binwidth
  sizes <- c(rep(binwidth, slice_n), slice_r)
  sliced_indices <- vctrs::vec_chop(indices, sizes = sizes)
  sliced_indices <- sliced_indices[-length(sliced_indices)]

  gamma <- adjust_alpha_simulate(1 - conf, binwidth, K = nlive)
  print(gamma)
  lower <- qbinom(gamma / 2, binwidth, (0:nlive) / nlive)
  upper <- qbinom(1 - (gamma / 2), binwidth, (0:nlive) / nlive)

  band_data <- lapply(sliced_indices, \(x) {
    data_frame0(
      x = probs,
      .ecdf = stats::ecdf(x$p)(probs),
      .lower = lower,
      .upper = upper,
      .n = binwidth
    )
  })
  vctrs::vec_rbind(
    !!!band_data,
    .names_to = "Interval"
  )
}

#' Plot ECDF difference against uniform reference
#'
#' @param indicies_ecdf Output from `get_insertion_ecdf()`.
#'
#' @return A ggplot object showing ECDF differences and confidence limits.
#' @noRd
plot_insertion_ecdf_diff <- function(indicies_ecdf, which = c("ecdf", "diff")) {
  which <- arg_match(which)
  if (which == "diff") {
    indicies_ecdf$.ecdf <- indicies_ecdf$.ecdf - indicies_ecdf$x
    indicies_ecdf$.lower <- indicies_ecdf$.lower /
      indicies_ecdf$.n -
      indicies_ecdf$x
    indicies_ecdf$.upper <- indicies_ecdf$.upper /
      indicies_ecdf$.n -
      indicies_ecdf$x
  } else {
    indicies_ecdf$.lower <- indicies_ecdf$.lower / indicies_ecdf$.n
    indicies_ecdf$.upper <- indicies_ecdf$.upper / indicies_ecdf$.n
  }
  ggplot(indicies_ecdf, aes(x = .data$x)) +
    geom_line(aes(y = .data$.lower), alpha = 0.35) +
    geom_line(aes(y = .data$.upper), alpha = 0.35) +
    ggplot2::geom_line(
      aes(y = .data$.ecdf, colour = factor(.data$Interval)),
      linewidth = 0.75
    ) +
    ggplot2::labs(
      x = "Insertion rank / nlive",
      y = "ECDF - Uniform CDF"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    ggplot2::theme_minimal()
}
