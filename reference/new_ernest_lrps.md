# Create a new likelihood-restricted prior sampler (LRPS)

Nested sampling relies on generating a series of points in the prior
space with increasing log-likelihood values. This is accomplished using
a likelihood-restricted prior sampler (LRPS), which generates
independent and identically distributed points from the prior, subject
to a hard likelihood constraint.

To create your own LRPS, subclass `new_ernest_lrps` and provide S3
methods for
[`propose()`](https://kylesnap.github.io/ernest/reference/propose.md)
and
[`update_lrps()`](https://kylesnap.github.io/ernest/reference/update_lrps.md)
for your subclass.

## Usage

``` r
new_ernest_lrps(
  unit_log_fn = NULL,
  n_dim = NULL,
  max_loop = getOption("ernest.max_loop", 1000000L),
  cache = NULL,
  ...,
  .class = NULL,
  .call = caller_env()
)
```

## Arguments

- unit_log_fn, n_dim:

  Provided when
  [`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
  is called with a given `ernest_lrps`:

  - `unit_log_fn` (function, optional): Takes a matrix of points in the
    unit cube and returns a numeric vector of log-likelihood values.

  - `n_dim` (integer, optional): Number of dimensions of the prior
    space.

- max_loop:

  Positive integer. Maximum number of attempts to generate points via
  region-based sampling. Usually hidden from users, but can be set via
  the `ernest.max_loop` option.

- cache:

  (environment, optional) Environment for caching values. If `NULL`, a
  new environment is created.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Name-value pairs for additional elements for subclasses of this LRPS.

- .class:

  (character vector, optional) Subclasses of this LRPS.

- .call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An LRPS specification: a list containing the input arguments, with a
class specific to the LRPS type.
