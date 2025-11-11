# Generate samples from the spanning ellipsoid

**\[experimental\]** Uses the bounding ellipsoid of the live points to
define the region of prior space that contains new points. Effective for
unimodal and roughly-Gaussian posteriors.

## Usage

``` r
unif_ellipsoid(enlarge = 1.25)
```

## Arguments

- enlarge:

  Double, greater than or equal to 1. Factor by which to inflate the
  bounding ellipsoid's volume before sampling (see Details).

## Value

A list with class `c("unif_ellipsoid", "ernest_lrps")`. Use with
[`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
to specify nested sampling behaviour.

## Details

Nested likelihood contours rarely form perfect ellipses, so sampling
from the spanning ellipsoid without enlargement may exclude valid
regions. This can bias proposals towards the ellipsoid centre and
overestimate evidence. Setting `enlarge = 1` will produce a warning.

The covariance matrix of the points is used to estimate the ellipsoid's
shape. In exceptional cases (e.g., perfect collinearity), this matrix
may be singular. Should this occur, the covariance matrix is
reconditioned by adjusting its eigenvalues. Should this also fail, the
algorithm falls back to sampling from the circumscribed sphere bounding
the unit hypercube.

## Ellipsoids

Ellipsoids are stored in the `cache` environment of the LRPS object.
Ellipsoids are defined by their centre \\c\\ and shape matrix \\A\\. The
set of points \\x\\ contained within the ellipsoid is given by \$\$ x
\in {\bf{R}}^n \| (x-c) A (x-c)' \leq 1 \$\$

The volume of the ellipsoid is \\V = \mathrm{Vol}(S_n)
\sqrt{\det(A^{-1})}\\, where \\\mathrm{Vol}(S_n)\\ is the volume of the
unit hypersphere.

For sampling, we store the matrix \\A^{-1/2}\\, the inverse of the
positive-semidefinite square root of \\A\\. The ellipsoid can
equivalently be defined as the set of points \$\$x = A^{-1/2} y + c,\$\$
where \\y\\ are points from the unit hypersphere.

For more on ellipsoids and their operations, see [Algorithms for
Ellipsoids](http://tcg.mae.cornell.edu/pubs/Pope_FDA_08.pdf) by S.B.
Pope, Cornell University Report FDA 08-01 (2008).

## Status

This LRPS is experimental and has not been extensively validated across
different nested sampling problems. You are encouraged to use it, but
please exercise caution interpretting results and report any issues or
unexpected behaviour.

## References

Feroz, F., Hobson, M. P., Bridges, M. (2009) MULTINEST: An Efficient and
Robust Bayesian Inference Tool for Cosmology and Particle Physics.
Monthly Notices of the Royal Astronomical Society. 398(4), 1601–1614.
[doi:10.1111/j.1365-2966.2009.14548.x](https://doi.org/10.1111/j.1365-2966.2009.14548.x)

Mukherjee, P., Parkinson, D., & Liddle, A. R. (2006). A Nested Sampling
Algorithm for Cosmological Model Selection. The Astrophysical Journal,
638(2), L51. [doi:10.1086/501068](https://doi.org/10.1086/501068)

## See also

Other ernest_lrps:
[`mini_balls()`](https://kylesnap.github.io/ernest/reference/mini_balls.md),
[`multi_ellipsoid()`](https://kylesnap.github.io/ernest/reference/multi_ellipsoid.md),
[`no_underrun()`](https://kylesnap.github.io/ernest/reference/no_underrun.md),
[`rwmh_cube()`](https://kylesnap.github.io/ernest/reference/rwmh_cube.md),
[`slice_rectangle()`](https://kylesnap.github.io/ernest/reference/slice_rectangle.md),
[`unif_cube()`](https://kylesnap.github.io/ernest/reference/unif_cube.md)

## Examples

``` r
data(example_run)
lrps <- unif_ellipsoid(enlarge = 1.25)

ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#> nested sampling specification <ernest_sampler>
#> • No. Points: 500
#> • LRPS Method: unif_ellipsoid
#> 
#> ernest LRPS method <unif_ellipsoid/ernest_lrps>
#> • Dimensions: 3
#> • No. Log-Lik Calls: 0
#> • Center: 0.5000, 0.5000, 0.5000
#> • Log Volume: 1.001
#> • Enlargement: 1.25
```
