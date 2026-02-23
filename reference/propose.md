# Generate a new point using LRPS

Use an LRPS method to replace points within the live set during a nested
sampling run. This method must be implemented on all classes that
inherit from `ernest_lrps`.

## Usage

``` r
propose(x, original = NULL, criterion = -Inf)

# S3 method for class 'multi_ellipsoid'
propose(x, original = NULL, criterion = -Inf)

# S3 method for class 'rwmh_cube'
propose(x, original = NULL, criterion = -Inf)

# S3 method for class 'slice_rectangle'
propose(x, original = NULL, criterion = -Inf)

# S3 method for class 'unif_cube'
propose(x, original = NULL, criterion = -Inf)

# S3 method for class 'unif_ellipsoid'
propose(x, original = NULL, criterion = -Inf)
```

## Arguments

- x:

  \[[ernest_lrps](https://kylesnap.github.io/ernest/reference/new_ernest_lrps.md)\]  
  A likelihood-restricted prior sampler.

- original:

  `[double(x$n_dim)]`  
  A parameter vector which can be used to start the proposal process.
  Optional; if `NULL` proposals are generated from sampling the
  unconstrained [unit
  cube](https://kylesnap.github.io/ernest/reference/unif_cube.md).

- criterion:

  `[double(1)]`  
  A log-likelihood value that restricts the region of prior space to
  sample from.

## Value

`[list]`, containing at least these named elements:

- `unit`: `[double(x$n_dim)]` The replacement point, expressed in the
  unit-cube.

- `log_lik`: `[double(1)]` The log-likelihood value at `unit`.

- `neval`: `[integer(1)]` The number of evaluations of `unit_log_fn`
  needed to generate `unit`. Do not confuse this with the number of
  samples evaluated during the call to `propose`, as `unit_log_fn` may
  be capable of evaluating multiple parameters with a single call.
