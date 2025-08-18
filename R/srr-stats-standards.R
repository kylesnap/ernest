#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.2} Life cycle statement found in CONTRIBUTING.md.
#' @srrstats {G1.4} Software uses `roxygen2`.
#' @srrstats {G1.4a} Internal functions are also documented with roxygen2, using the @noRd or keywords internal.
#' @srrstats {G2.0a, G2.1a} Documentation consistently mentions the expected type and length of each variable.
#' @srrstats {G5.2, G5.2a, G5.2b} Warning and error messages are all tested with expect_error/warning or expect_snapshot.
#' @srrstats {G5.5} Tests are run with a fixed random seed.
#' @srrstatsTODO {BS1.3b} Documentation on the different sampling
#' algorithms (but warns users away from the inefficient `unif_cube`) sampler.
#' @srrstatsTODO {BS4.0} Cites original software implementation.
#' blueprints inherit from the one created by `new_blueprint()`, and the default
#' method specific blueprints inherit from the other three here.
#' @srrstatsTODO {BS2.12, BS2.13} Documentation of the `rlang` verbosity parameter.
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G1.6} ernest does not make performance claims against other R packages, mostly due to a lack of true R-based implementations.
#' @srrstatsNA {G2.4d, G2.4e, G2.5} Ernest does not allow for factor inputs; users provide model information through likelihood functions.
#' @srrstatsNA {G5.4a} Ernest conducts its algorithmic and p. recovery tests against
#' analytical values from dynesty and certified values from the NIST.
#' @srrstatsNA {BS2.9, BS2.10} Software does not use parallel chains.
#' @srrstatsNA {G2.7, G2.8, G2.9, G2.10, G2.11, G2.12} Ernest does not allow for tabular input, working instead from user-inputted likelihood functions.
#' @srrstatsNA {G3.1, G3.1a} Ernest doesn't presently call a covariance algorithm.
#' @srrstatsNA {G4.0} Ernest does not write to the user's storage.
#' @srrstatsNA {G5.10, G5.11, G5.11a, G5.12} Ernest does not use extended
#' testing tags. I have used "skip_if_*" tags to prevent the
#' CI/CRAN checks from running over certain plotting tests (where vdiffr might
#' fail) and from running all of the algorithmic tests.
#' @srrstatsNA {BS7.0, BS7.1} Ernest always requires a log-likelihood function
#' and a prior distribution to perform integration. Please advise if prior
#' recovery tests are required and how they should be performed.
#' @srrstatsNA {BS1.0} Ernest does not use the term "hyperparameter".
#' @srrstatsNA {BS1.4, BS1.5, BS5.4, BS5.5} Ernest does not use convergence checkers on run
#' results. The main stopping criterion for the run is given by the remaining
#' log evidence in the unexplored prior (min_logz).
#' @srrstatsNA {BS2.7, BS2.11} Nested sampling doesn't rely on user-provided
#' starting values for the samplers.
#' @srrstatsNA {BS3.1, BS3.2} Similar to other nested samplers, ernest doesn't
#' examine the model for colinearity. Ernest does check for likelihood plateau,
#' and can gracefully stop a run when a perfect plateau is reached.
#' @srrstatsNA {BS6.5} Currently, visualize cannot produce distributional
#' plots and trace plots on the same graphic.
#'
#' @noRd
NULL
