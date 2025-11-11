# Plot the posterior distribution of an `ernest_run`

Create a plot of the posterior distributions from a nested sampling run,
or trace the evolution of discarded live points along the log prior
volume.

## Usage

``` r
# S3 method for class 'ernest_run'
visualize(x, ..., type = c("density", "trace"), vars = NULL, plot = TRUE)
```

## Arguments

- x:

  An
  [ernest_run](https://kylesnap.github.io/ernest/reference/generate.ernest_sampler.md)
  object.

- ...:

  Arguments passed on to
  [`as_draws.ernest_run`](https://kylesnap.github.io/ernest/reference/as_draws.md)

  `units`

  :   Case-sensitive string. The scale for the sampled points:

      - `"original"`: Points are on the scale of the prior space.

      - `"unit_cube"`: Points are on the (0, 1) unit hypercube scale.

  `radial`

  :   Logical. If `TRUE`, returns an additional column `.radial`
      containing the radial coordinate (i.e., the Euclidean norm) for
      each sampled point.

- type:

  Case-sensitive string. The type of plot to create:

  - `"density"`: Shows the posterior density of each parameter.

  - `"trace"`: Shows the distribution of points along estimates of the
    log prior volume.

- vars:

  \<`tidy-select`\> Variables to plot from the run. If `NULL`, all
  variables are plotted.

- plot:

  Logical. If `TRUE`, returns a `ggplot` of the visualisation; if
  `FALSE`, returns a `tibble` of the data used to create the plot.

## Value

A `ggplot` object if `plot = TRUE`, otherwise a `tibble`.

## See also

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) for visualising
evidence estimates from an `ernest_run`.

## Examples

``` r
# Load example run
library(ggdist)
data(example_run)

# Plot posterior distributions of the parameters
visualize(example_run, type = "density")


# Plot the trace of the radial coordinate in unit scale
visualize(
  example_run,
  type = "trace",
  vars = ".radial",
  units = "unit_cube",
  radial = TRUE
)
```
