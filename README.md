
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Tools for Nested Sampling

<!-- badges: start -->

[![R-CMD-check](https://github.com/kylesnap/ernest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kylesnap/ernest/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/kylesnap/ernest/branch/ropensci_submission/graph/badge.svg?token=6HL8L046Y7)](https://codecov.io/gh/kylesnap/ernest)
<!-- badges: end -->

The best place to start learning about NS would be this paper, surveying
modern applications and implementations of the algorithm. Much of ernest
was developed based on this paper’s description of NS:

- Buchner, J. (2023). Nested Sampling Methods. Statistics Surveys, 17,
  169–215. <https://doi.org/10.1214/23-SS144>

To read the original papers describing NS’s development, see:

- Skilling, J. (2004). Nested Sampling. *AIP Conference Proceedings*,
  735(1), 395–405. <https://doi.org/10.1063/1.1835238>
- Skilling, J. (2006). Nested Sampling for General Bayesian Computation.
  *Bayesian Analysis*, 1(4), 833–859. <https://doi.org/10.1214/06-BA127>

## Installation

You can install the development version of ernest from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kylesnap/ernest")
```

## Why Use Nested Sampling?

Nested Sampling (NS) is a Bayesian algorithm developed for evaluating
the plausibility of a statistical model. In Bayesian statistics, we
describe our knowledge of a model’s unknown parameters using
probabilities. We use Bayes’ theorem to represent how new data can be
used to update our beliefs about a model’s parameters ($`\theta`$):
``` math

P(\theta) = \frac{L(\theta) \pi(\theta)}{\mathcal{Z}} 
```
Here, the *prior* $`\pi(\theta)`$ reflects what we know about parameters
before seeing the data, the *likelihood* $`l(\theta)`$ encodes the
likelihood of the parameters given the data, and the *posterior*
$`P(\theta)`$ reflects our updated beliefs about $`\theta`$ after
learning from the data.

The quantity $`\mathcal{Z}`$, called the *model evidence* or *marginal
likelihood*, has a special interpretation in Bayesian inference. If we
reorganize Bayes’ theorem, we notice that $`\mathcal{Z}`$ represents the
probability of generating the observed sample for all possible values of
$`\theta`$:
``` math

\mathcal{Z} = \int_{\forall \theta} L(\theta) \pi(\theta) d\theta
```
This allows us to use $`\mathcal{Z}`$ to represent the overall
plausibility of a model given some observed data. This leads to Bayesian
model comparison, where ratios of $`\mathcal{Z}`$ are used to make
nuanced comparisons between different models based on their
plausibility.

Aside from simple models and data, this evidence integral cannot be
solved analytically. To address this, NS was developed as a method to
estimate $`\mathcal{Z}`$ while also producing an estimate of
$`P(\theta)`$. In basic terms, NS works by dividing the prior space into
a nested sequence of very small volumes or shells. By then sorting these
shells according to their worst likelihood value, they can then be used
to form an unidimensional approximation of the evidence integral that
can be solved using numerical methods. In addition to providing
estimates of $`\mathcal{Z}`$ and $`P(\theta)`$, NS has the following
desirable qualities over other Bayesian methods such as Markov-chain
Monte Carlo (MCMC):

1.  NS conducts a global exploration of $`\pi(\theta)`$;
2.  NS can more easily traverse complicated or poorly-conditioned
    distributions, such as those with multiple modes or discontinuities,
    and;
3.  NS defines natural stopping criteria, such that it does not require
    burn-in runs nor does it require supervised testing across
    convergence or termination conditions.

## Why Use Ernest?

Other NS implementations, such as MultiNest, do provide R interfaces to
nested sampling algorithms originally developed in other languages
(e.g., Python, Fortran). The goal of Ernest is to provide a complete,
native R implementation of the nested sampling algorithm, designed
specifically for the resources and packages already used by R
programmers.

### Ernest Helps You Specify Prior Distributions

Nested sampling in ernest works by drawing samples from the unit
hypercube—a $`d`$-dimensional space where each coordinate is in
$`[0, 1)`$. The “hypercube transformation” maps these coordinates to the
actual parameter space using the inverse cumulative distribution
function (CDF) of each marginal prior. This approach ensures efficient
sampling and avoids rejection steps.

Ernest allows you to encode common priors as S3 objects with convenience
functions, like `create_uniform_prior()` and `create_normal_prior()`.
Advanced prior transformations can be provided as functions to
`create_prior`, which will validate a prior specification before a run
is performed.

### Ernest Can Start and Continue Previous Nested Sampling Runs

Ernest allows you to start a new nested sampling run or continue an
existing one using the same sampler object. When you call `generate()`
on a sampler, it performs nested sampling and stores the results in an
`ernest_run` object. If you call `generate()` again on the same sampler,
Ernest will resume the run from where it left off, retaining all
previously collected points and evidence estimates. This makes it easy
to refine your results by increasing the number of iterations or
adjusting stopping criteria without losing previous work.

### Ernest Can Simulate the Uncertainty within Evidence Estimates

The `calculate()` function in ernest is a powerful tool for quantifying
uncertainty in evidence estimates produced by nested sampling. By
default, it computes deterministic estimates of evidence and related
quantities. However, you can also use `calculate()` to simulate the
impact of uncertainty in the log volume estimates. This allows you to
generates multiple realizations of the nested sampling process, allowing
you to assess the variability and robustness of your evidence estimates.
The results are returned as S3 objects with their own `summary()` and
`plot()` methods.

## Prior Art

In addition to the citations provided, Ernest’s API was informed by the
excellent [dynesty](https://dynesty.readthedocs.io/en/v2.1.5/index.html)
python package.

- Speagle, J. (2020). DYNESTY: A Dynamic Nested Sampling Package for
  Estimating Bayesian Posteriors and Evidences. *Monthly Notices of the
  Royal Astronomical Society*, 493(3), 3132–3158.
  <https://doi.org/10.1093/mnras/staa278>.

Beyond dynesty, there exist several other nested sampling
implementations for popular, non-R programming languages. The
implementations listed below are popular, well-maintained, and
well-documented.

| Package | Author | Languages |
|:---|:--:|:--:|
| [polychord](https://github.com/PolyChord/PolyChordLite) | Will Handley, Mike Hobson, & Anthony Lasenby | Fortran, with interfaces for Python and C/C++ |
| [MultiNest](https://github.com/JohannesBuchner/MultiNest) | Farhan Feroz & Mike Hobson | Fortran, with interfaces for R, Python, and C++ |
| [perfectns](https://github.com/ejhigson/perfectns) | Edward Higson | Python |

In addition, the
[nestcheck](https://nestcheck.readthedocs.io/en/latest/) python package
provides methods for creating diagnostic plots for nested sampling runs.

- Higson, E., Handley, W., Hobson, M., & Lasenby, A. (2019). nestcheck:
  Diagnostic Tests for Nested Sampling Calculations. *Monthly Notices of
  the Royal Astronomical Society*, 483(2), 2044–2056.
  <https://doi.org/10.1093/mnras/sty3090>
