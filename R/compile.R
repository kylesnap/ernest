#' @importFrom generics compile
#' @export
generics::compile

#' Finalize a nested sampler before sampling.
#'
#' @param x An `ErnestSampler` object.
#' @param overwrite Whether to fully overwrite the current sampler. If false,
#' and if the sampler has already been finalised, an an error is issued.
#'
#' @return An `ErnestSampler` object with the finalised sampler.
#'
#' @rdname compile
#' @export
S7::method(compile, ErnestLRPS) <- function(x) {
  if (x@compiled) {
    cli::cli_warn("The sampler has already been compiled.")
    return(NULL)
  }
  x@wrk <- new_environment(list(
    "live_units" = matrix(
      stats::runif(x@n_dim * x@n_points),
      nrow = x@n_points,
      ncol = x@n_dim
    ),
    "live_points" = matrix(nrow = x@n_points, ncol = x@n_dim),
    "live_lik" = numeric(x@n_points)
  ))

  for (i in 1:x@n_points) {
    x@wrk$live_points[i, ] <- x@prior_transform(x@wrk$live_units[i, ])
    x@wrk$live_lik[i] <- x@log_lik(x@wrk$live_points[i, ])
  }
  x
}

#' @name compile
#' @export
S7::method(compile, ErnestRun) <- function(x) {
  # Removes everything but the live samples.
  env_unbind(
    x@wrk,
    setdiff(
      env_names(x@wrk),
      c("live_points", "live_units", "live_lik")
    )
  )
  env_binding_lock(x@wrk, c("live_points", "live_units", "live_lik"))
  invisible(x)
}
