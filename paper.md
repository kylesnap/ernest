---
title: 'ernest: An R Toolkit for Nested Sampling'
tags:
  - R
  - Nested sampling
  - Bayesian
  - Model comparison
  - Marginal likelihood
  - Model evidence
authors:
  - name: Kyle Dewsnap
    orcid: 0000-0003-2132-8083
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: University of British Columbia, Canada
   index: 1
   ror: 03rmrcq20
date: 31 August 2026
bibliography: paper.bib
---

# Summary
Marginal likelihood, or model evidence, is the parameter-independent probability of observing data under a given model. It is frequently used in Bayesian analyses to compare competing models for the same data. Directly calculating evidence requires integrating the model's likelihood over the prior parameter space, which is almost always computationally infeasible. Rather than attempting to solve this integral directly, nested sampling approximates a solution by partitioning the prior space into a series of nested likelihood contours and expressing the evidence as the sum of each contour's estimated contribution [@skilling2004]. This approach yields direct estimates of both model evidence and the posterior distribution while preserving several attractive statistical properties [@buchner2023].

# Statement of need
`ernest` is a comprehensive, native R toolkit for learning, performing, and reporting nested sampling analyses. Users provide a model as a log-likelihood function and a description of the prior space, allowing `ernest` to be applied across a wide range of models in different disciplines. Users can select from a variety of sampling methods; internally, these methods are implemented in C++, enabling faster computation without leaving the R environment. `ernest` also represents a nested sampling run as a series of S3 generics and methods, including `generate()`, `summary()`, and `plot()`. This design is helpful for users unfamiliar with nested sampling and promotes consistency with other R packages. Integration with established packages such as `posterior` and `ggplot2` enables researchers to leverage existing visualization and inference tools.

`ernest` is designed for multiple audiences. Students learning Bayesian inference can use it to explore model evidence and posterior inference interactively, with vignettes demonstrating complete workflows from prior specification through visualization. Bayesian statisticians and applied researchers can perform rigorous model comparison directly within their R workflows, without exporting data to external software. Advanced users can implement custom likelihood-restricted prior samplers by following simple S3 conventions, enabling domain-specific optimizations.

# State of the field
Several nested sampling implementations exist across different programming ecosystems. `MultiNest` [@feroz2009] is a well-established Fortran package offering sophisticated ellipsoidal decomposition and stopping criteria, but it lacks native R integration and has a steep learning curve for users unfamiliar with Fortran. `dynesty` [@speagle2020] is a popular Python package with elegant algorithms and excellent documentation, providing an intuitive interface for Python users. `UltraNest` [@buchner2019] emphasizes speed through slice-sampling and clustered ellipsoidal decomposition and includes guidance on using the `reticulate` [@ushey2026] package to run Python from within the R environment.

The main motivation behind `ernest`'s development was the need for a nested sampling implementation that integrates seamlessly within the broader R software ecosystem. In the author's experience, building and debugging log-likelihood and prior functions in R for use in Python introduced significant friction. By performing the entire nested sampling process within the same R environment, runs fail more gracefully within `ernest`, providing descriptive error and warning messages and enabling faster diagnostics. This may impose some performance cost relative to nested sampling performed in Python or a compiled language; however, using C++ implementations (via `cpp11`) for compute-intensive routines prevents these losses from accumulating without degrading `ernest`'s accessible API.

As an additional consequence, `ernest`'s S3 class system and method dispatch provide a modular, extensible design that aligns with R's statistical philosophy rather than imposing a rigid object-oriented hierarchy. This allows advanced users to implement custom likelihood-restricted prior samplers by following simple S3 conventions without forking the codebase.

# Software design
`ernest`'s design was guided by three principles: (a) _flexibility_, so that it remains relevant and useful across multiple research contexts; (b) _composability_, so that users can approach a nested sampling run as a series of smaller, easily understood steps; and (c) _consistency_, meaning that it should align with existing conventions both within R and within related, high-quality R packages. To achieve flexibility, I decided that users should be responsible for providing their model's likelihood function. This adds complexity to the implementation process, but it also aligns with existing practices in other nested sampling implementations and grants `ernest` the ability to work across a variety of modelling scenarios. Composability was emphasized by structuring a run into a series of verbs: a sampler is validated with `compile()`, a run is performed with `generate()`, and the results are viewed with `plot()` or `visualize()`. This reliance on S3 methods also supported consistency, which was further improved by designing the results to be stored with the `vctrs` package and by allowing them to be converted into space-efficient `rvar` variables used by packages in the `RStan` ecosystem.

# Research impact statement
`ernest` addresses an open need for Bayesian analyses in R: a general-purpose nested sampling tool for Bayesian evidence estimation and posterior inference. `ernest`'s usefulness is demonstrated in a worked example in the package vignette, where it is used to resolve a model comparison problem between two Poisson regression models describing epilepsy therapy data.

`ernest` was developed as part of a doctoral dissertation at the University of British Columbia, evaluating applications of nested sampling to statistical models used in the health and social sciences. The package has been peer-reviewed by rOpenSci and was publicly released on CRAN on 2 June 2026.

# AI usage disclosure
During `ernest`'s development, GitHub Copilot in auto mode was used for inline suggestions and to assist periodically with refactoring both source code and documentation. The author certifies that they reviewed, edited, and validated all AI-assisted outputs. Beyond spelling and grammar checking, no AI tool was used while writing this paper.

# References