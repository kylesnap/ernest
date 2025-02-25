#' The Nested Sampling Workspace
#'
#' @description
#' Nested sampling is conducted within an environment (implemented as a
#' stripped-down `R6` object). This allows for the state of the sampling to be
#' continued later. Users aren't expected to access this item directly, rather
#' they may use the generics provided to interact with the objects in tidy and
#' readable formats.
#'
ErnestWorkspace <- R6::R6Class(
  "ErnestWorkspace",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field live_units A matrix of live points, stored in the unit hypercube
    live_units = NULL,
    #' @field live_points A matrix of live points, stored in the orig. param space
    live_points = NULL,
    #' @field live_lik A vector of log-likelihoods for the live points
    live_lik = NULL,

    #' @description
    #' Initialize the workspace with the live points and their log-likelihoods.
    #' @param log_lik The log-likelihood function.
    #' @param prior_transform The prior transform function.
    #' @param n_dim The number of dimensions in the parameter space.
    #' @param n_points The number of live points to use.
    #' @returns A new ErnestWorkspace object, which is an R environment.
    initialize = function(log_lik, prior_transform, n_dim, n_points) {
      self$live_units <- matrix(
        stats::runif(n_dim * n_points),
        nrow = n_points,
        ncol = n_dim
      )
      self$live_points <- matrix(nrow = n_points, ncol = n_dim)
      self$live_lik <- numeric(n_points)
      for (i in 1:n_points) {
        self$live_points[i, ] <- prior_transform(live_units[i, ])
        self$live_lik[i] <- log_lik(live_points[i, ])
      }
      private$.n_points <- n_points
    },

    #' @description
    #' Log a point and its log-likelihood to the dead set.
    #' @param iter The current iteration number.
    #' @param log_z The current evidence estimate.
    #' @param log_vol The current prior volume.
    pop_point = function(iter, log_z, log_vol) {
      # Save the dead point
      private$.dead_units[[iter]] <- live_units[private$.worst_idx, ]
      private$.dead_points[[iter]] <- live_points[private$.worst_idx, ]
      private$.dead_lik[[iter]] <- live_lik[private$.worst_idx]

      # Save additional information
      private$.dead_vol[[iter]] <- log_vol
      private$.dead_idx[[iter]] <- .worst_idx
      private$.log_z <- log_z
      private$.worst_lik <- .dead_lik[[iter]]

      # Return an index of a non-worst point for proposal.
      copy <- sample.int(private$.n_points, size = 1)
      if (is.null(private$.worst_idx)) {
        cli::cli_abort(
          "`pop_point` called before `worst_idx`."
        )
      }
      while (copy == private$.worst_idx) {
        copy <- sample.int(private$.n_points, size = 1)
      }
      copy
    },

    #' @description
    #' Add a new point to the live set, replacing the point existing at
    #' index `worst_idx`.
    #' @param iter The current iteration number.
    #' @param new The new point to add.
    #' @param copy The index of the point to copy from the live set.
    #' @param cur_update The number of updates made to the sampler.
    push_point = function(iter, new, copy, cur_update) {
      # Replace live point at worst with 'new'
      self$live_units[.worst_idx, ] <- new$unit
      self$live_points[.worst_idx, ] <- new$parameter
      self$live_lik[.worst_idx] <- new$log_lik

      private$.calls[[iter]] <- new$num_calls
      private$.parent[[iter]] <- copy
      private$.updates[[iter]] <- cur_update
      private$.calls[[iter]]
    },

    #' @description
    #' Get the dead points returned as a matrix
    #' @param unit_scale Whether to return the points in the unit hypercube.
    get_dead = function(unit_scale) {
      check_bool(unit_scale)
      if (unit_scale) {
        do.call(rbind, private$.dead_units)
      } else {
        do.call(rbind, private$.dead_points)
      }
    }
  ),
  private = list(
    .n_points = NULL,
    .dead_units = list(),
    .dead_points = list(),
    .dead_lik = list(),
    .dead_vol = list(),
    .dead_idx = list(),

    .calls = list(),
    .parent = list(),
    .updates = list(),

    .log_z = -1.e300,
    .worst_idx = NULL,
    .worst_lik = -1.e300
  ),
  active = list(
    #' @field n_iter The total number of sampling iterations.
    n_iter = function() {
      length(private$.calls)
    },
    #' @field n_call The total calls made to the likelihood function,
    #' or `0L` if no calls have been made yet.
    n_call = function() {
      sum(list_c(private$.calls))
    },

    #' @field log_vol The current prior volume occupied by the dead
    #' points, or `0.0` if no points have been killed yet.
    log_vol = function() {
      if (is_empty(private$.dead_vol)) {
        0.0
      } else {
        tail(private$.dead_vol, 1L)
      }
    },

    #' @field log_z The current evidence estimate from the dead
    #' points, or `-1.e300` if no points have been killed yet.
    log_z = function() {
      private$.log_z
    },

    #' @field worst_lik The worst log-likelihood value most recently added
    #' to the dead set, or -1.e300 if no points have been killed yet.
    worst_lik = function() {
      private$.worst_lik
    },

    #' @field worst_idx Get the index of the current worst point in the live set.
    worst_idx = function() {
      private$.worst_idx <- which.min(live_lik)
      private$.worst_idx
    },

    #' @field last_update The number of times the sampler has been updated, or
    #' `0L` if no updates have been made yet.
    last_update = function() {
      if (is_empty(private$.updates)) {
        0L
      } else {
        tail(private$.updates, 1L)
      }
    },

    #' @field integral_rcrd Returns the log_vol and log_lik vectors as a `vctrs::rcrd`
    #' object.
    integral_rcrd = function() {
      dead_lik <- list_c(private$.dead_lik)
      dead_vol <- list_c(private$.dead_vol)
      last_vol <- tail(dead_vol, 1)

      live_vol <- log1p(
        (-1 - private$.n_points)^(-1) * seq_len(private$.n_points)
      ) + last_vol

      vctrs::new_rcrd(
        list(
          "log_vol" = vctrs::vec_c(dead_vol, live_vol),
          "log_lik" = vctrs::vec_c(dead_lik, sort(self$live_lik))
        )
      )
    }
  )
)
