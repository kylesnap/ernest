# Run nested sampling to estimate Bayesian evidence

Executes the nested sampling algorithm, iteratively replacing the worst
live point with a new sample from a likelihood-restricted prior until a
stopping criterion is met.

## Usage

``` r
# S3 method for class 'ernest_sampler'
generate(
  x,
  ...,
  max_iterations = NULL,
  max_calls = NULL,
  min_logz = 0.05,
  show_progress = NULL
)
```

## Arguments

- x:

  An object of class `ernest_sampler` or `ernest_run`.

- ...:

  Arguments passed on to
  [`compile.ernest_run`](https://kylesnap.github.io/ernest/reference/compile.md)

  `seed`

  :   **\[deprecated\]** `seed` is no longer supported; set RNG with the
      `seed` argument of
      [`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md).

  `clear`

  :   Logical. If `TRUE`, clears results from previous runs before
      compiling. If `FALSE`, retains previous results and validates live
      points.

- max_iterations:

  Optional positive integer. The maximum number of iterations to
  perform. If `NULL`, this criterion is ignored.

- max_calls:

  Optional positive integer. The maximum number of calls to the
  likelihood function. If `Inf`, this criterion is ignored.

- min_logz:

  Non-negative double. The minimum log-ratio between the current
  estimated evidence and the remaining evidence. If zero, this criterion
  is ignored.

- show_progress:

  Logical. If `TRUE`, displays a progress spinner and iteration counter
  during sampling. If `NULL`, then the parameter is inferred based on
  the value of the `rlib_message_verbosity` option.

## Value

An object of class `ernest_run`, inheriting from `ernest_sampler`, with
additional components:

- `n_iter`: Integer. Number of iterations.

- `n_calls`: Integer. Total number of likelihood function calls.

- `log_lik`: `double(n_iter + n_points)`. Log-likelihoods for each
  sample.

- `log_volume`: `double(n_iter + n_points)`. Estimated log-prior volumes
  at each iteration.

- `log_weight`: `double(n_iter + n_points)`. Unnormalised log-weights
  for each sample.

- `log_evidence`: `double(n_iter + n_points)`. Cumulative log-evidence
  estimates at each iteration.

- `log_evidence_var`: `double(n_iter + n_points)`. Variance of the
  log-evidence estimate at each iteration.

- `information`: `double(n_iter + n_points)`. KL divergence between the
  prior and posterior, estimated at each iteration.

- `samples`: `matrix(nrow = n_iter + n_points, ncol = n_dim)`. Parameter
  values for each sample.

- `samples_unit`: `matrix(nrow = n_iter + n_points, ncol = n_dim)`.
  Parameter values for each sample in unit hypercube representation.

- `id`: `integer(n_iter + n_points)`. Unique integer identifiers for
  each sample from the live set (ranging from 1 to `n_points`).

- `points`: `integer(n_iter + n_points)`. Number of live points present
  at each iteration.

- `calls`: `integer(n_iter + n_points)`. Number of calls used to
  generate a new live point at each iteration.

- `birth`: `integer(n_iter + n_points)`. Iteration at which each sample
  was first created (ranging from 0 to `n_iter`).

## Details

At least one of `max_iterations`, `max_calls`, or `min_logz` must
specify a valid stopping criterion. Setting `min_logz` to zero while
leaving `max_iterations` and `max_calls` at their defaults will result
in an error.

If `x` is an `ernest_run` object, the stopping criteria are checked
against the current state of the run. An error is thrown if the stopping
criteria have already been satisfied by `x`.

The `min_logz` parameter controls the relative tolerance for the
remaining evidence in the unexplored parameter space. Sampling stops
when the estimated remaining evidence is sufficiently small compared to
the accumulated evidence.

## References

Skilling, J. (2006). Nested Sampling for General Bayesian Computation.
Bayesian Analysis, 1(4), 833–859.
[doi:10.1214/06-BA127](https://doi.org/10.1214/06-BA127)

## Examples

``` r
prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#> New names:
#> • `Uniform` -> `Uniform...1`
#> • `Uniform` -> `Uniform...2`
ll_fn <- function(x) -sum(x^2)
sampler <- ernest_sampler(ll_fn, prior, n_point = 100)
sampler
#> nested sampling specification <ernest_sampler>
#> • No. Points: 100
#> • LRPS Method: rwmh_cube
#> 
#> ernest LRPS method <rwmh_cube/ernest_lrps>
#> • Dimensions: 2
#> • No. Log-Lik Calls: 0
#> • No. Accepted Proposals: 0
#> • No. Steps: 25
#> • Target Acceptance: 0.5
#> • Step Size: 1.000

# Stop sampling after a set number of iterations or likelihood calls.
generate(sampler, max_iterations = 100)
#> ℹ Created 100 live points.
#> ✔ `max_iterations` reached (100).
#> nested sampling results <ernest_run/ernest_sampler>
#> • No. Points: 100
#> • LRPS Method: rwmh_cube
#> ────────────────────────────────────────────────────────────────────────────────
#> • No. Iterations: 100
#> • No. Calls: 176
#> • Log. Evidence: -0.6444 (± 0.1312)

# The final number of calls may exceed `max_calls`, as `generate`
# only checks the number of calls when removing a live point.
generate(sampler, max_calls = 2600)
#> ℹ Created 100 live points.
#> ✔ `max_calls` surpassed (2603 > 2600).
#> nested sampling results <ernest_run/ernest_sampler>
#> • No. Points: 100
#> • LRPS Method: rwmh_cube
#> ────────────────────────────────────────────────────────────────────────────────
#> • No. Iterations: 227
#> • No. Calls: 2603
#> • Log. Evidence: -0.6349 (± 0.07714)

# Use the default stopping criteria
if (FALSE)  generate(sampler)  # \dontrun{}
```
