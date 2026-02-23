# Example Nested Sampling Run with Ernest

Load a precomputed example nested sampling run generated using the
ernest package. It demonstrates a typical output from a nested sampling
run on a simple 3-dimensional Gaussian likelihood, with a uniform prior
over each dimension. This dataset is intended for use in documentation,
tutorials, and gaining experience with `ernest_run`'s S3 methods.

## Usage

``` r
example_run
```

## Format

An object of class `ernest_run` containing the results of a nested
sampling run.

## Source

This example problem comes from the crash course for the
[dynesty](https://dynesty.readthedocs.io/en/v2.1.5/crashcourse.html)
Python-based nested sampling software.

## Details

The likelihood used to generate the points is \\MVN(0, \Sigma)\\, with
each variance in \\\Sigma\\ set to 1 and each covariance set to 0.95.
The prior for each parameter is uniform on the interval `[-10, 10\]`.

This run uses the following non-default settings:

- `log_lik`: A 3D multivariate Gaussian with mean zero and covariance
  matrix `diag(0.95, 3)`.

- `prior`: Uniform over each dimension (x, y, z) in the range \[-10,
  10\].

- `seed`: 42

View the `$spec` element of `example_run` to see the full R
specification of the likelihood and prior.

\[-10, 10\]: R:-10,%2010%5C \[-10, 10\]: R:-10,%2010
