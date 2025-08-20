
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A Toolkit for Nested Sampling

<!-- badges: start -->

[![R-CMD-check](https://github.com/kylesnap/ernest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kylesnap/ernest/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/kylesnap/ernest/branch/ropensci_submission/graph/badge.svg?token=6HL8L046Y7)](https://codecov.io/gh/kylesnap/ernest)
<!-- badges: end -->

ernest is a comprehensive toolkit for nested sampling (NS) in R,
enabling estimation of Bayesian evidence and posterior distributions for
statistical models. Alongside an original R-based implementation of the
algorithm described by Skilling (2006), ernest provides:

1.  S3 objects and methods for defining model likelihoods and prior
    parameter distributions;
2.  Methods for simulating, reporting, and visualising uncertainty in NS
    estimates;
3.  S3 generics for developers to implement custom likelihood-restricted
    prior samplers.

ernest’s API is inspired by the Python package nestle (Barbary 2015),
which also influenced dynesty (Speagle 2020).

## Installation

Install the development version of ernest from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kylesnap/ernest")
```

## Nested Sampling in a Nutshell

Nested sampling (NS) is a Bayesian algorithm for evaluating model
plausibility, measured as the *Bayesian evidence* or *marginal
likelihood* ($`\mathcal{Z}`$). The evidence normalises the posterior
distribution; by reorganising Bayes’ theorem, we see that
$`\mathcal{Z}`$ is the probability of the observed data across all
possible parameter values $`\theta`$:
``` math

\mathcal{Z} = \int_{\forall \theta} L(\theta) \pi(\theta) d\theta
```
For most models, this integral cannot be solved analytically. NS
addresses this by dividing the prior space $`\pi(\theta)`$ into a nested
sequence of small volumes, sorted by likelihood, to form a
one-dimensional approximation of the evidence integral. NS offers
several advantages over other Bayesian methods such as Markov chain
Monte Carlo (MCMC):

1.  Global exploration of $`\pi(\theta)`$;
2.  Robustness to complex or poorly conditioned distributions (e.g.,
    multimodal or discontinuous);
3.  Simultaneous estimation of the posterior distribution;
4.  Natural stopping criteria—no need for burn-in or manual convergence
    checks.

## Nested Sampling in Ernest

ernest provides a native R and C++11 implementation of nested sampling,
with an API designed for R users.

To begin, specify a likelihood function and a prior space. For example:

``` r
library(ernest)
library(LaplacesDemon)

# Uniform prior over [-10, 10) for three parameters
prior <- create_uniform_prior(
  3,
  lower = -10,
  upper = 10,
  varnames = c("x", "y", "z")
)

# Multivariate normal log-likelihood
mu <- c(0, 0, 0)
C <- diag(1, 3)
C[C == 0] <- 0.95
loglike <- create_likelihood(
  rowwise_fn = dmvn,
  mu = !!mu,
  Sigma = !!C,
  log = TRUE
)

# Set up and run the sampler
sampler <- ernest_sampler(
  log_lik = loglike,
  prior = prior,
  n_points = 500
)
run <- generate(sampler, max_iterations = 2000, seed = 123)

# Summarise and visualise results
summary(run)
plot(run)
visualize(run, type = "density")
```

For advanced usage, including custom priors and hierarchical models, see
the package vignette.

## Prior Art

Below are selected packages that implement NS, in addition to nestle and
dynesty:

| Package | Author | Languages |
|:---|:--:|:--:|
| [polychord](https://github.com/PolyChord/PolyChordLite) | Will Handley, Mike Hobson, & Anthony Lasenby | Fortran, with Python and C/C++ interfaces |
| [MultiNest](https://github.com/JohannesBuchner/MultiNest) | Farhan Feroz & Mike Hobson | Fortran, with R, Python, and C++ interfaces |
| [perfectns](https://github.com/ejhigson/perfectns) | Edward Higson | Python |

The nestcheck Python package provides routines for diagnosing nested
sampling runs (Handley 2019).

### References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-barbary2015" class="csl-entry">

Barbary, Kyle. 2015. “Nestle: Pure Python, MIT-Licensed Implementation
of Nested Sampling Algorithms for Evaluating Bayesian Evidence.”
<https://github.com/kbarbary/nestle>.

</div>

<div id="ref-anesthetic" class="csl-entry">

Handley, Will. 2019. “Anesthetic: Nested Sampling Visualisation.” *The
Journal of Open Source Software* 4 (37): 1414.
<https://doi.org/10.21105/joss.01414>.

</div>

<div id="ref-skilling2006" class="csl-entry">

Skilling, John. 2006. “Nested Sampling for General Bayesian
Computation.” *Bayesian Analysis* 1 (4): 833–59.
<https://doi.org/10.1214/06-BA127>.

</div>

<div id="ref-speagle2020" class="csl-entry">

Speagle, Joshua S. 2020. “DYNESTY: A Dynamic Nested Sampling Package for
Estimating Bayesian Posteriors and Evidences.” *Monthly Notices of the
Royal Astronomical Society* 493 (April): 3132–58.
<https://doi.org/10.1093/mnras/staa278>.

</div>

</div>
