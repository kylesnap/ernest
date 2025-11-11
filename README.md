
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A Toolkit for Nested Sampling

<!-- badges: start -->

[![R-CMD-check](https://github.com/kylesnap/ernest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kylesnap/ernest/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/kylesnap/ernest/branch/ropensci_submission/graph/badge.svg?token=6HL8L046Y7)](https://codecov.io/gh/kylesnap/ernest)

<!-- badges: end -->

**ernest** is a comprehensive toolkit for [nested
sampling](https://en.wikipedia.org/wiki/Nested_sampling_algorithm) (NS),
an algorithm for estimating a statistical model’s Bayesian evidence and
posterior distribution. It provides S3 objects and methods that make
nested sampling accessible, flexible, and robust within the R
environment.

## Installation

Install the development version of ernest from
[GitHub](https://github.com/kylesnap/ernest) with:

``` r
# install.packages("devtools")
devtools::install_github("kylesnap/ernest")
```

## Why use ernest?

In Bayesian inference, evidence ($`\mathcal{Z}`$, also called the
[marginal
likelihood](https://en.wikipedia.org/wiki/Marginal_likelihood)) is the
probability of observing data $`D`$ under a proposed model $`M`$. This
is obtained by integrating the model’s likelihood over the prior
distribution of $`M`$’s parameters. $`\mathcal{Z}`$ provides a
parameter-independent way to assess the plausibility of $`D`$ given
$`M`$, and is key for Bayesian model comparison through methods such as
[Bayes factors](https://en.wikipedia.org/wiki/Bayes_factor).

Calculating $`\mathcal{Z}`$ is challenging, as it requires evaluating a
high-dimensional integral over the parameter space. Nested sampling
estimates this integral by dividing the space into a series of small
volumes. It starts by drawing points from the prior and ranking them by
likelihood. The least likely points are discarded and replaced with new
samples from more restricted likelihood regions, gradually compressing
the search space. Each round of discarding shrinks the explored volume
in a predictable way, helping to approximate the integral.

This approach to estimating $`Z`$ offers several advantages over methods
like Markov chain Monte Carlo (MCMC):

- **Robustness**: NS handles complex likelihood surfaces that would
  otherwise be difficult to traverse, such as those with multiple modes
  or discontinuities.
- **Posterior inference**: After a run, discarded samples can be
  weighted to approximate the model’s posterior distribution.
- **Natural stopping criterion**: NS can estimate the amount of evidence
  left within the unexplored prior volume, and can stop sampling once
  this amount gets trivially small.
- **Tractable uncertainty estimates**: The shrinkage at each iteration
  follows a uniform order statistic, so uncertainty can be simulated
  using the results from a single run.

ernest’s implementation of NS offers R users several benefits:

- **Native R implementation**: John Skilling’s Skilling (2006) NS
  algorithm is implemented in R, with no Python or Fortran dependencies.
  (C++ is used to implement the included likelihood samplers to improve
  run-time efficiency).
- **Type- and size-safety**: ernest helps ensure that the user provides
  likelihood functions and prior specifications meet the requirements of
  the NS algorithm.
- **Familiar methods**: Sampler specifications and results are stored in
  S3 objects. Start or continue an NS run with `generate()`, review
  results with `summary()`, and simulate estimation error with
  `calculate()`.
- **Powerful visualizations**: Plot evidence estimates and analyse
  posterior distributions using [ggplot2](https://ggplot2.tidyverse.org)
  and [posterior](https://mc-stan.org/posterior/).

## Quick Example

This example demonstrates a basic workflow: define a prior, specify a
likelihood, run nested sampling, and summarise results.

``` r
library(ernest)

# Define a prior (i.i.d. multivariate uniform)
prior <- create_uniform_prior(lower = -10, upper = 10, names = c("x", "y", "z"))

# Define a log-likelihood function (multivariate normal)
mu <- c(0, 0, 0)
Sigma <- diag(1, 3)
Sigma[Sigma == 0] <- 0.95
loglike <- create_likelihood(
  rowwise_fn = LaplacesDemon::dmvn,
  mu = !!mu,
  Sigma = !!Sigma,
  log = TRUE
)

# Set up and run the sampler
sampler <- ernest_sampler(
  log_lik = loglike,
  prior = prior,
  n_points = 500
)
run <- generate(sampler)

# Summarise and visualise results
summary(run)
plot(run)
visualize(run, type = "trace")
```

For advanced usage, including custom priors and hierarchical models, see
the package vignettes.

## Learn More

- **About NS**: `vignette("nested-sampling-with-ernest")`, Skilling
  (2004), Skilling (2006), and Buchner (2023).
- **How to use ernest**: `vignette("more-ernest-runs.Rmd")`.

## Prior Work

NS has been implemented in many languages; some offer R interfaces. This
non-exhaustive list of popular NS implementations is adapted from
Fowlie, Handley, and Su (2021):

| Package | Citation | Language(s) |
|:---|:--:|:--:|
| [nestle](https://github.com/kbarbary/nestle/tree/master) | Barbary (2015) | Python |
| [dynesty](https://github.com/joshspeagle/dynesty) | Speagle (2020) | Python |
| [DIAMONDS](https://github.com/EnricoCorsaro/DIAMONDS/) | Corsaro and Ridder (2014) | C++ |
| [MultiNest](https://github.com/JohannesBuchner/MultiNest) | Feroz, Hobson, and Bridges (2009) | Fortran; interfaces for C++, Python, R, and MatLab |
| [PolyChord](https://github.com/PolyChord/PolyChordLite) | Handley, Hobson, and Lasenby (2015) | Fortran; interfaces for C++ and Python |
| [DNest4](https://github.com/eggplantbren/DNest4) | Brewer and Foreman-Mackey (2018) | C++; interfaces with Python, R, and Julia |

ernest’s design, API, and NS implementation are based on the nestle
package, with further inspiration from dynesty.

The [nestcheck](https://github.com/ejhigson/nestcheck/tree/master)
Python package provides routines for error estimation and diagnostic
plotting with nested sampling runs (Higson et al. 2019). Several of
ernest’s methods are based on this work.

------------------------------------------------------------------------

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-barbary2015" class="csl-entry">

Barbary, Kyle. 2015. *Nestle: Pure Python, MIT-Licensed Implementation
of Nested Sampling Algorithms for Evaluating Bayesian Evidence.*
<https://github.com/kbarbary/nestle>.

</div>

<div id="ref-brewer2018" class="csl-entry">

Brewer, Brendon J., and Daniel Foreman-Mackey. 2018. “DNest4: Diffusive
Nested Sampling in C++ and Python.” *Journal of Statistical Software* 86
(September): 1–33. <https://doi.org/10.18637/jss.v086.i07>.

</div>

<div id="ref-buchner2023" class="csl-entry">

Buchner, Johannes. 2023. “Nested Sampling Methods.” *Statistics Surveys*
17 (none): 169–215. <https://doi.org/10.1214/23-SS144>.

</div>

<div id="ref-corsaro2014" class="csl-entry">

Corsaro, E., and J. De Ridder. 2014. “DIAMONDS: A New Bayesian Nested
Sampling Tool - Application to Peak Bagging of Solar-Like Oscillations.”
*Astronomy & Astrophysics* 571 (November): A71.
<https://doi.org/10.1051/0004-6361/201424181>.

</div>

<div id="ref-feroz2009" class="csl-entry">

Feroz, F., M. P. Hobson, and M. Bridges. 2009. “MULTINEST: An Efficient
and Robust Bayesian Inference Tool for Cosmology and Particle Physics.”
*Monthly Notices of the Royal Astronomical Society* 398 (October):
1601–14. <https://doi.org/10.1111/j.1365-2966.2009.14548.x>.

</div>

<div id="ref-fowlie2021" class="csl-entry">

Fowlie, Andrew, Will Handley, and Liangliang Su. 2021. “Nested Sampling
with Plateaus.” *Monthly Notices of the Royal Astronomical Society* 503
(1): 1199–1205. <https://doi.org/10.1093/mnras/stab590>.

</div>

<div id="ref-handley2015" class="csl-entry">

Handley, W. J., M. P. Hobson, and A. N. Lasenby. 2015. “PolyChord:
Next-Generation Nested Sampling.” *Monthly Notices of the Royal
Astronomical Society* 453 (4): 4384–98.
<https://doi.org/10.1093/mnras/stv1911>.

</div>

<div id="ref-higson2019" class="csl-entry">

Higson, Edward, Will Handley, Michael Hobson, and Anthony Lasenby. 2019.
“Nestcheck: Diagnostic Tests for Nested Sampling Calculations.” *Monthly
Notices of the Royal Astronomical Society* 483 (2): 2044–56.
<https://doi.org/10.1093/mnras/sty3090>.

</div>

<div id="ref-skilling2004" class="csl-entry">

Skilling, John. 2004. “Nested Sampling.” *AIP Conference Proceedings*
735 (1): 395–405. <https://doi.org/10.1063/1.1835238>.

</div>

<div id="ref-skilling2006" class="csl-entry">

———. 2006. “Nested Sampling for General Bayesian Computation.” *Bayesian
Analysis* 1 (4): 833–59. <https://doi.org/10.1214/06-BA127>.

</div>

<div id="ref-speagle2020" class="csl-entry">

Speagle, Joshua S. 2020. “DYNESTY: A Dynamic Nested Sampling Package for
Estimating Bayesian Posteriors and Evidences.” *Monthly Notices of the
Royal Astronomical Society* 493 (April): 3132–58.
<https://doi.org/10.1093/mnras/staa278>.

</div>

</div>
