# Generate new points with a random walk

Create new samples for the live set by evolving a current point in the
set through a Metropolis-Hastings random walk, rejecting steps that fail
to meet the likelihood criterion.

## Usage

``` r
rwmh_cube(steps = 25, target_acceptance = 0.5)
```

## Arguments

- steps:

  `[integer(1)]`  
  Number of steps to take when generating a proposal point. Must be
  greater or equal to 2.

- target_acceptance:

  `[double(1)]`  
  Target acceptance rate for proposed points. Must be a number between
  `1 / steps` and 1.

## Value

`[rwmh_cube]`, a named list that inherits from
\[[ernest_lrps](https://kylesnap.github.io/ernest/reference/new_ernest_lrps.md)\].

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

- `steps`: Start with 25. Increase to generate points that more closely
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
[`multi_ellipsoid()`](https://kylesnap.github.io/ernest/reference/multi_ellipsoid.md),
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
