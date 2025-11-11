# Prepare a likelihood function for nested sampling

Creates a modified version of a log-likelihood function that always
returns either a finite value or `-Inf` for each vector of parameters
provided.

## Usage

``` r
create_likelihood(fn, on_nonfinite = c("warn", "quiet", "abort"))
```

## Arguments

- fn:

  A function that takes a vector of parameters and returns a scalar
  log-likelihood value (either a finite double or `-Inf`).

- on_nonfinite:

  A case-sensitive string. Action to perform when `fn` returns a value
  that is non-finite and not `-Inf` (i.e., `NaN`, `NA`, `Inf`):

  - `"warn"`: Issue a warning and return `-Inf`.

  - `"quiet"`: Silently return `-Inf`.

  - `"abort"`: Stop execution and signal an error.

## Value

A function with class `ernest_likelihood`. When provided a parameter
vector this function will always return either a scalar finite double,
the value `-Inf`, or an error message.

## Details

Model likelihoods should be provided as a log-density function. The
first argument of `fn` should be a vector of free parameters.

If the model likelihood is conditional on some data, then use this step
to incorporate this data into your nested sampling run. We recommended
using an (anonymous
function)[`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)
to do this (see Examples).

It is expected that the log-likelihood function returns a scalar finite
double or `-Inf` for each parameter vector. Non-finite values other than
`-Inf`, such as `NaN`, `Inf`, or `NA` (i.e. missing values) are handled
with the behavior of `on_nonfinite`.

## Examples

``` r
# A 3D Gaussian likelihood function
n_dim <- 3
sigma <- diag(0.95, nrow = 3)
det_sigma <- determinant(sigma, logarithm = TRUE)$modulus
attributes(det_sigma) <- NULL
prec <- solve(sigma)
log_norm <- -0.5 * (log(2 * pi) * n_dim + det_sigma)

fn <- function(theta) {
  drop(-0.5 * crossprod(theta, crossprod(prec, theta)) + log_norm)
}
log_lik <- create_likelihood(fn)
log_lik(c(0, 0, 0))
#> [1] -2.679876

# Bind data to the likelihood function an anonymous function.
y <- 100000000 * runif(11, min = 0.1, max = 0.3)
log_lik <- function(theta, y) {
  if (theta[2] <= 0) {
    return(-Inf)
  }
  sum(dnorm(y, mean = theta[1], sd = theta[2], log = TRUE))
}
create_likelihood(\(theta) log_lik(theta, y))
#> likelihood function <ernest_likelihood>
#> 
#> function (theta) 
#> log_lik(theta, y)
```
