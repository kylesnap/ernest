# Run nested sampling to estimate Bayesian evidence

Executes the nested sampling algorithm, iteratively replacing the worst
live point with a new sample from a likelihood-restricted prior until a
stopping criterion is met.

## Usage

``` r
# S3 method for class 'ernest_sampler'
generate(
  x,
  max_iterations = NULL,
  max_evaluations = NULL,
  min_logz = 0.05,
  show_progress = NULL,
  ...
)

# S3 method for class 'ernest_run'
generate(
  x,
  max_iterations = NULL,
  max_evaluations = NULL,
  min_logz = 0.05,
  show_progress = NULL,
  ...
)
```

## Arguments

- x:

  \[[ernest_sampler](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)\]
  or \[ernest_run\]  
  A nested sampling specification.

- max_iterations:

  `[integer(1)]`  
  The maximum number of iterations to perform. Optional; if `NULL` this
  criterion is ignored.

- max_evaluations:

  `[integer(1)]`  
  The maximum number of times the run can evaluate the likelihood
  function. Optional; if `NULL` this criterion is ignored.

- min_logz:

  `[double(1)]`  
  The minimum log-ratio between the current estimated evidence and the
  remaining evidence. Must be non-negative; if set to zero, this
  criterion is ignored.

- show_progress:

  `[logical(1)]`  
  If `TRUE`, displays a progress spinner and iteration counter during
  sampling. Optional; if `NULL` the global option
  `rlib_message_verbosity` is used to determine whether to show
  progress.

- ...:

  Arguments passed on to
  [`compile.ernest_run`](https://kylesnap.github.io/ernest/reference/compile-ernest.md)

  `clear`

  :   `[logical(1)]`  
      If `TRUE`, clears results from previous runs before compiling. If
      `FALSE`, retains previous results and validates the live set.

## Value

An `[ernest_run]` object with the nested sampling results.

This inherits from
[ernest_sampler](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
and adds:

- `niter`: `[integer(1)]` Number of iterations performed.

- `neval`: `[integer(1)]` Number of times the likelihood function was
  evaluated.

- `log_evidence`: `[double(1)]` The log-evidence estimate.

- `log_evidence_err`: `[double(1)]` The standard error of the
  log-evidence estimate, derived using `information`.

- `information`: `[double(1)]` The estimated Kullback-Leibler
  divergence.

- `log_weight`: `[double(nsample)]` Each sample's contribution to the
  log-evidence estimate, computed from its log-likelihood and prior
  volume.

- `rcrd`:
  \[[ernest_rcrd](https://kylesnap.github.io/ernest/reference/ernest_rcrd.md)\]
  An object storing an internal record of each point generated during
  the run.

## Details

At least one of `max_iterations`, `max_evaluations`, or `min_logz` must
specify a valid stopping criterion. Setting `min_logz` to zero while
leaving `max_iterations` and `max_evaluations` at their defaults will
result in an error.

If `x` is an `ernest_run` object, the stopping criteria are checked
against the current state of the run. An error is thrown if the stopping
criteria have already been satisfied by `x`.

The `min_logz` parameter controls the relative tolerance for the
remaining evidence in the unexplored parameter space. Sampling stops
when the estimated remaining evidence is sufficiently small compared to
the accumulated evidence.

`rcrd` will have size `niter + nlive`: The first `niter` entries
correspond to the points removed during the run, the last `nlive`
entries correspond to the points within the live set at the end of the
run.

## References

Skilling, J. (2006). Nested Sampling for General Bayesian Computation.
Bayesian Analysis, 1(4), 833–859.
[doi:10.1214/06-BA127](https://doi.org/10.1214/06-BA127)

## See also

[`calculate.ernest_run()`](https://kylesnap.github.io/ernest/reference/calculate.ernest_run.md)
[`summary.ernest_run()`](https://kylesnap.github.io/ernest/reference/summary.ernest_run.md)
[`weights.ernest_run()`](https://kylesnap.github.io/ernest/reference/weights.ernest_run.md)

## Examples

``` r
prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
ll_fn <- function(x) -sum(x^2)
sampler <- ernest_sampler(ll_fn, prior, nlive = 100)
sampler
#> Nested sampling run specification:
#> * No. points: 100
#> * Sampling method: 25-step random walk sampling (acceptance target = 50%)
#> * Prior: uniform prior distribution with 2 dimensions (Uniform_1 and Uniform_2)

# Stop sampling after a set number of iterations or likelihood evaluations.
generate(sampler, max_iterations = 100)
#> Nested sampling run:
#> * No. points: 100
#> * Sampling method: 25-step random walk sampling (acceptance target = 50%)
#> * Prior: uniform prior distribution with 2 dimensions (Uniform_1 and Uniform_2)
#> ── Results ─────────────────────────────────────────────────────────────────────
#> * Iterations: 100
#> * Likelihood evals.: 155
#> * Log-evidence: -0.6118 (± 0.1252)
#> * Information: 0.08528

# Use the default stopping criteria
if (FALSE)  generate(sampler)  # \dontrun{}
```
