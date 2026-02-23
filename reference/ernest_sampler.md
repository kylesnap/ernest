# Prepare a new nested sampling run

Initializes an `ernest_sampler` object containing the components
required to perform nested sampling. This object can then be used to
build sequences of nested samples with
[`generate()`](https://generics.r-lib.org/reference/generate.html).

## Usage

``` r
ernest_sampler(
  log_lik,
  prior,
  sampler = rwmh_cube(),
  nlive = 500,
  first_update = NULL,
  update_interval = NULL,
  seed = NA
)
```

## Arguments

- log_lik:

  `[function]` or
  \[[ernest_likelihood](https://kylesnap.github.io/ernest/reference/create_likelihood.md)\]  
  A function which computes the log-likelihood of a given model. If a
  function, it is wrapped with
  [`create_likelihood()`](https://kylesnap.github.io/ernest/reference/create_likelihood.md).

- prior:

  \[[ernest_prior](https://kylesnap.github.io/ernest/reference/create_prior.md)\]  
  Describes the prior space within which to generate samples.

- sampler:

  \[[ernest_lrps](https://kylesnap.github.io/ernest/reference/new_ernest_lrps.md)\]  
  Specifies the likelihood-restricted prior sampling method used to
  replace points within the live set.

- nlive:

  `[integer(1)]`  
  The number of points to generate within the live set. Strictly
  positive.

- first_update:

  `[integer(1)]`  
  The number of likelihood calls to make with the default [uniform
  LRPS](https://kylesnap.github.io/ernest/reference/unif_cube.md) method
  before swapping to the technique described by `sampler`. Optional; if
  left `NULL` this is set to `nlive * 2.5`.

- update_interval:

  `[integer(1)]`  
  The number of likelihood calls between updates to the `sampler`
  object. Optional; if left `NULL` this is set to `nlive * 1.5`.

- seed:

  `[integer(1)]`  
  Sets the random seed controlling the random number generator for
  nested sampling runs. Optional; if left `NA` the
  [.Random.seed](https://rdrr.io/r/base/Random.html) set within R is
  preserved and restored after a run.

## Value

`[ernest_sampler]`  
A named list, containing a specification for a nested sampling run.
Contains the arguments passed to this function as well as an environment
`run_env`, which is used to store the live set during sampling.

## Details

The `ernest_sampler` object is tested with
[`compile()`](https://generics.r-lib.org/reference/compile.html) before
it is returned. This helps to catch errors with the likelihood and prior
specifications. If this compilation step fails, review your `log_lik_fn`
and `prior` objects for their compliance.

## Verbosity

Messages from ernest can be silenced with the global options
`rlib_message_verbosity` and `rlib_warning_verbosity`. These options
take the values:

- "default": Verbose unless the `.frequency` argument is supplied.

- "verbose": Always verbose.

- "quiet": Always quiet.

When set to quiet, messages are not displayed and the condition is not
signaled. See
[`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
more information.

## Examples

``` r
prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
ll_fn <- function(x) -sum(x^2)
sampler <- ernest_sampler(ll_fn, prior, nlive = 100)
sampler
#> Nested sampling run specification:
#> * No. points: 100
#> * Sampling method: 25-step random walk sampling (acceptance target = 50.0%)
#> * Prior: uniform prior distribution with 2 dimensions (Uniform_1 and Uniform_2)

# Use a unit-cube LRPS (not recommended in practice)
unit_sampler <- ernest_sampler(
  ll_fn,
  prior,
  nlive = 100,
  sampler = unif_cube()
)
unit_sampler
#> Nested sampling run specification:
#> * No. points: 100
#> * Sampling method: Uniform unit cube sampling
#> * Prior: uniform prior distribution with 2 dimensions (Uniform_1 and Uniform_2)
```
