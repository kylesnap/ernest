# Visualize posterior distributions or traces from a nested sampling run

Produces visualizations of the posterior distributions or the evolution
of variables along the log-prior volume from a nested sampling run.

## Usage

``` r
# S3 method for class 'ernest_run'
visualize(
  x,
  ...,
  .which = c("density", "trace"),
  .units = c("original", "unit_cube"),
  .radial = FALSE
)
```

## Arguments

- x:

  \[[ernest_run](https://kylesnap.github.io/ernest/reference/generate.ernest_sampler.md)\]
  The results of a nested sampling run.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more variables to plot from the run. If omitted, all variables
  are plotted.

- .which:

  `[charcacter(1)]` Character string specifying the type of plot to
  produce. Options are `"density"` for the posterior density of each
  parameter or `"trace"` for the trace of variables along log-volume.

- .units:

  `[character(1)]`  
  The scale of the sampled points:

  - `"original"`: Points are on the scale of the prior space.

  - `"unit_cube"`: Points are on the (0, 1) unit hypercube scale.

- .radial:

  `[logical(1)]`  
  If `TRUE`, returns an additional column `.radial` containing the
  radial coordinate (i.e., the Euclidean norm) for each sampled point.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

The [`visualize()`](https://generics.r-lib.org/reference/visualize.html)
function is designed to quickly explore the results of a nested sampling
run through two types of plots:

- **Density plots** show the marginal posterior for each selected
  variable, using
  [`ggdist::stat_halfeye()`](https://mjskay.github.io/ggdist/reference/stat_halfeye.html)
  to visualize uncertainty and distribution shape.

- **Trace plots** display the evolution of variables as a function of
  log-volume, with points coloured by posterior weight. This can help
  diagnose sampling behavior and identify regions of interest in the
  prior volume.

Posterior weights are derived from the individual contributions of each
sampled point in the prior space to a run's log-evidence estimate. A
point's weight is a function of (a) the point's likelihood and (b) the
estimated amount of volume within that point's likelihood contour. See
ernest's vignettes for more information.

## Note

This package requires the tidyselect package to be installed. If
`which = "trace"` is selected, the ggdist package is also required.

## See also

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  diagnostic plots of nested sampling runs.

- [`as_draws_rvars()`](https://mc-stan.org/posterior/reference/draws_rvars.html)
  for extracting posterior samples.

## Examples

``` r
# Load example run
library(ggdist)
data(example_run)

# Plot posterior densities for all parameters
visualize(example_run, .which = "density")


# Plot trace of the radial coordinate in unit-cube scale
visualize(
  example_run,
  .which = "trace",
  .units = "unit_cube",
  .radial = TRUE
)
```
