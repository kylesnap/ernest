# Generate samples with a random walk

Create new live points by evolving a current live point through a
Metropolis-Hastings random walk, rejecting steps that fail to meet the
likelihood criterion.

## Usage

``` r
rwmh_cube(steps = 25, target_acceptance = 0.5)
```

## Arguments

- steps:

  Positive integer. Number of steps to take when generating a proposal
  point.

- target_acceptance:

  Number between `1 / steps` and 1.0. Target acceptance rate for
  proposed points.

## Value

An object of class `c("rwmh_cube", "ernest_lrps")` that can be used with
[`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
to specify the sampling behaviour.

## Details

The random walk LRPS generates proposals by performing a fixed number of
Metropolis-Hastings steps within the unit hypercube. Each step proposes
a new location by adding a random perturbation to the current position,
accepting or rejecting the step based on whether it satisfies the
likelihood criterion. This process continues for the specified number of
steps, with the final accepted position returned as the proposal.

**Step-size Adaptation**: The step size \\\epsilon\\ is adapted between
sampling rounds using
[`update_lrps()`](https://kylesnap.github.io/ernest/reference/update_lrps.md).
The adaptation uses a Newton-like method to target the desired
acceptance rate \\\alpha\_\*\\. Given the current acceptance rate
\\\alpha_i\\ and number of dimensions \\d\\, the step size is updated
with:

\$\$\epsilon_i \* \exp(\frac{\alpha_i - \alpha^\*}{d \cdot
\alpha\_\*})\$\$

Given the previously-accepted sample \\X\_{i-1}\\ and the number of
dimensions \\d\\, proposed points are generated from:

\$\$X\_{i-1} + S_d(0, \epsilon)\$\$

where \\S(0, \epsilon)\\ is a point drawn uniformly from the
\$d\$-dimensional ball centered on the origin with radius \\\epsilon\\.

## Control Parameters

- `steps`: Start with 25. Increase to generate samples that more closely
  follow the posterior distribution; decrease for computational
  efficiency.

- `target_acceptance`: Start with 0.4-0.6. Lower values encourage more
  global exploration of the posterior, higher values encourage
  explorations close to the starting point.

## References

- Skilling, J. (2006). Nested Sampling for General Bayesian Computation.
  Bayesian Analysis, 1(4), 833–859.
  [doi:10.1214/06-BA127](https://doi.org/10.1214/06-BA127)

- Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package for
  Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
  Royal Astronomical Society, 493, 3132–3158.
  [doi:10.1093/mnras/staa278](https://doi.org/10.1093/mnras/staa278)

## See also

Other ernest_lrps:
[`mini_balls()`](https://kylesnap.github.io/ernest/reference/mini_balls.md),
[`multi_ellipsoid()`](https://kylesnap.github.io/ernest/reference/multi_ellipsoid.md),
[`no_underrun()`](https://kylesnap.github.io/ernest/reference/no_underrun.md),
[`slice_rectangle()`](https://kylesnap.github.io/ernest/reference/slice_rectangle.md),
[`unif_cube()`](https://kylesnap.github.io/ernest/reference/unif_cube.md),
[`unif_ellipsoid()`](https://kylesnap.github.io/ernest/reference/unif_ellipsoid.md)

## Examples

``` r
# Basic usage with default parameters
lrps <- rwmh_cube()

# A faster sampler for simple-to-traverse posterior surfaces
fast_lrps <- rwmh_cube(
  steps = 20,
  target_acceptance = 0.7
)
```
