# Prepare a likelihood function for nested sampling

Creates a modified version of a log-likelihood function that always
returns either a finite value or `-Inf` for each vector of parameters
provided.

## Usage

``` r
create_likelihood(
  scalar_fn,
  vectorized_fn,
  on_nonfinite = c("warn", "quiet", "abort")
)
```

## Arguments

- scalar_fn, vectorized_fn:

  `[function]`  
  The log-likelihood function. Provide either `scalar_fn` or
  `vectorized_fn`:

  - `scalar_fn`: Should accept a parameter as a numeric vector and
    return a single numeric value representing the log-likelihood, or
    `-Inf`.

  - `vectorized_fn`: Should accept a matrix of parameter vectors (rows
    as samples, columns as elements of the parameter vector) and return
    a vector of log-likelihoods or `-Inf` values for each row.

- on_nonfinite:

  `[character]`  
  How the sampler should handle values returned by `fn` or `matrix_fn`
  that are not finite and not equal to `-Inf`. Must be one of:

  - `"warn"`: Issue a warning and return `-Inf`.

  - `"quiet"`: Silently return `-Inf`.

  - `"abort"`: Stop execution and signal an error.

## Value

`[ernest_likelihood]`, which inherits from `function`.

## Details

Provide model likelihoods as a log-density function, which take a vector
of free parameter values and return the corresponding log-likelihood
value.

Likelihoods are typically the most computationally expensive function to
evaluate in a nested sampling run. ernest allows you to implement your
likelihood as a function over a single parameter vector (`scalar_fn`) or
over a matrix of parameters (`vectorized_fn`).

ernest expects the log-likelihood function to return a finite double or
`-Inf` for each parameter vector. The behaviour when encountering
non-finite values other than `-Inf` (such as `NaN`, `Inf`, or `NA`) is
controlled by `on_nonfinite`.

If your log-likelihood depends on additional data (e.g., an observation
matrix or data frame), provide these using an (anonymous
function)[`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)
(see Examples).

## See also

See the
[cubature](https://bnaras.github.io/cubature/articles/cubature.html)
package for more examples of scalar and vectorized functions.

## Examples

``` r
library(mvtnorm)

# Multivariate Normal Distribution
m <- 3
mean <- rep(0, m)
sigma <- diag(m)
sigma[2, 1] <- sigma[1, 2] <- 3 / 5
sigma[3, 1] <- sigma[1, 3] <- 1 / 3
sigma[3, 2] <- sigma[2, 3] <- 11 / 15
prec <- solve(sigma)
log_det <- -sum(log(diag(chol(sigma))))

# Provide a Scalar Log-Likelihood Function:
log_lik <- function(x) {
  log_det - 0.5 * m * log(2 * pi) - 0.5 * (t(x) %*% prec %*% x)
}
log_lik(c(0, 0, 0))
#>           [,1]
#> [1,] -2.128207

# `create_likelihood` allows scalar fns. to accept matrix inputs:
try(log_lik(matrix(rep(0, m * 2), nrow = 2)))
#> Error in t(x) %*% prec : non-conformable arguments
scalar_ll <- create_likelihood(scalar_fn = log_lik)
scalar_ll(matrix(rep(0, m * 2), nrow = 2))
#> [1] -2.128207 -2.128207

# Provide a Vectorized Log-Likelihood Function:
v_log_lik <- function(x) {
  dmvnorm(x, mean = mean, sigma = sigma, log = TRUE)
}
v_log_lik(c(0, 0, 0))
#> [1] -2.128207
v_log_lik(matrix(rep(0, m * 2), nrow = 2))
#> [1] -2.128207 -2.128207

vector_ll <- create_likelihood(vectorized_fn = v_log_lik)
vector_ll
#> Vectorized Log-likelihood Function
#> function (x) 
#> {
#>     dmvnorm(x, mean = mean, sigma = sigma, log = TRUE)
#> }

# Control Behaviour when Nonfinite Likelihood Values are Encountered
# Default: Warn and replace with `-Inf`
vector_ll(c(0, 0, NA))
#> Warning: Replacing `NA` with `-Inf`.
#> [1] -Inf

# Signal an error
abort_ll <- create_likelihood(log_lik, on_nonfinite = "abort")
try(abort_ll(c(0, 0, NA)))
#> Error : Couldn't calculate the log-lik of 0, 0, and NA.
#> Caused by error:
#> ! log-lik. values must be either finite or `-Inf`.
#> ✖ Detected non-viable value: `NA`.

# Silently replace all non-finite values
quiet_ll <- create_likelihood(vectorized_fn = v_log_lik, on_nonfinite = "quiet")
quiet_ll(c(0, 0, NA))
#> [1] -Inf
```
