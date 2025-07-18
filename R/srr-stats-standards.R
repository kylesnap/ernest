#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.2} Life cycle statement found in CONTRIBUTING.md.
#' @srrstats {G1.4} Software uses `roxygen2`.
#' @srrstatsTODO {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files or [`@keywords internal`](https://roxygen2.r-lib.org/reference/tags-index-crossref.html?q=keywords%20internal#null) if documentation is still desired.*
#' @srrstatsTODO {G1.5} Vignette? *Software should include all code necessary to reproduce results which form the basis of performance claims made in associated publications.*
#' @srrstatsTODO {G2.0a} Provide explicit secondary documentation of any expectations on lengths of inputs
#' @srrstatsTODO {G2.1a} *Provide explicit secondary documentation of expectations on data types of all vector inputs.*
#' @srrstatsTODO {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#' @srrstats {G3.0} Ernest avoids making equality comparisons between floating-point numbers.
#' @srrstatsTODO {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstatsTODO {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstatsTODO {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstatsTODO {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstatsTODO {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
#' @srrstatsTODO {G5.3} *For functions which are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the absence of any such values in return objects should be explicitly tested.*
#' @srrstatsTODO {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstatsTODO {G5.4a} *For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.*
#' @srrstatsTODO {G5.4b} *For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.*
#' @srrstatsTODO {G5.4c} *Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available*
#' @srrstatsTODO {G5.5} *Correctness tests should be run with a fixed random seed*
#' @srrstatsTODO {G5.6} **Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.*
#' @srrstatsTODO {G5.6a} *Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstatsTODO {G5.6b} *Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-running, such tests may be part of an extended, rather than regular, test suite; see G5.10-4.12, below).*
#' @srrstatsTODO {G5.7} **Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.*
#' @srrstatsTODO {G5.8} **Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:*
#' @srrstatsTODO {G5.8a} *Zero-length data*
#' @srrstatsTODO {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
#' @srrstatsTODO {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
#' @srrstatsTODO {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*
#' @srrstatsTODO {G5.9} **Noise susceptibility tests** *Packages should test for expected stochastic behaviour, such as through the following conditions:*
#' @srrstatsTODO {G5.9a} *Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results*
#' @srrstatsTODO {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results*
#' @srrstatsTODO {G5.10} *Extended tests should included and run under a common framework with other tests but be switched on by flags such as as a `<MYPKG>_EXTENDED_TESTS="true"` environment variable.* - The extended tests can be then run automatically by GitHub Actions for example by adding the following to the `env` section of the workflow:
#' @srrstatsTODO {G5.11} *Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow.*
#' @srrstatsTODO {G5.11a} *When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with an appropriate diagnostic message.*
#' @srrstatsTODO {G5.12} *Any conditions necessary to run extended tests such as platform requirements, memory, expected runtime, and artefacts produced that may need manual inspection, should be described in developer documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.*
#' @srrstatsTODO {BS1.0} *Bayesian software which uses the term "hyperparameter" should explicitly clarify the meaning of that term in the context of that software.*
#' @srrstatsTODO {BS1.1} *Descriptions of how to enter data, both in textual form and via code examples. Both of these should consider the simplest cases of single objects representing independent and dependent data, and potentially more complicated cases of multiple independent data inputs.*
#' @srrstatsTODO {BS1.3} *Description of all parameters which control the computational process (typically those determining aspects such as numbers and lengths of sampling processes, seeds used to start them, thinning parameters determining post-hoc sampling from simulated values, and convergence criteria). In particular:*
#' @srrstatsTODO {BS1.3a} *Bayesian Software should document, both in text and examples, how to use the output of previous simulations as starting points of subsequent simulations.*
#' @srrstatsTODO {BS1.3b} *Where applicable, Bayesian software should document, both in text and examples, how to use different sampling algorithms for a given model.*
#' @srrstatsTODO {BS1.4} *For Bayesian Software which implements or otherwise enables convergence checkers, documentation should explicitly describe and provide examples of use with and without convergence checkers.*
#' @srrstatsTODO {BS1.5} *For Bayesian Software which implements or otherwise enables multiple convergence checkers, differences between these should be explicitly tested.*
#' @srrstatsTODO {BS2.1a} *The effects of such routines should be tested.*
#' @srrstatsTODO {BS2.6} *Check that values for computational parameters lie within plausible ranges.*
#' @srrstatsTODO {BS2.7} *Enable starting values to be explicitly controlled via one or more input parameters, including multiple values for software which implements or enables multiple computational "chains."*
#' @srrstatsTODO {BS2.8} *Enable results of previous runs to be used as starting points for subsequent runs.*
#' @srrstatsTODO {BS2.11} *Software which accepts starting values as a vector should provide the parameter with a plural name: for example, "starting_values" and not "starting_value".*
#' @srrstatsTODO {BS2.12} *Bayesian Software should implement at least one parameter controlling the verbosity of output, defaulting to verbose output of all appropriate messages, warnings, errors, and progress indicators.*
#' @srrstatsTODO {BS2.13} *Bayesian Software should enable suppression of messages and progress indicators, while retaining verbosity of warnings and errors. This should be tested.*
#' @srrstatsTODO {BS3.1} *Implement pre-processing routines to diagnose perfect collinearity, and provide appropriate diagnostic messages or warnings*
#' @srrstatsTODO {BS3.2} *Provide distinct routines for processing perfectly collinear data, potentially bypassing sampling algorithms*
#' @srrstatsTODO {BS4.0} *Packages should document sampling algorithms (generally via literary citation, or reference to other software)*
#' @srrstatsTODO {BS4.3} *Implement or otherwise offer at least one type of convergence checker, and provide a documented reference for that implementation.*
#' @srrstatsTODO {BS4.4} *Enable computations to be stopped on convergence (although not necessarily by default).*
#' @srrstatsTODO {BS4.5} *Ensure that appropriate mechanisms are provided for models which do not converge.*
#' @srrstatsTODO {BS4.6} *Implement tests to confirm that results with convergence checker are statistically equivalent to results from equivalent fixed number of samples without convergence checking.*
#' @srrstatsTODO {BS4.7} *Where convergence checkers are themselves parametrised, the effects of such parameters should also be tested. For threshold parameters, for example, lower values should result in longer sequence lengths.*
#' @srrstatsTODO {BS5.1} *Return values should include appropriate metadata on types (or classes) and dimensions of input data*
#' @srrstatsTODO {BS5.2} *Bayesian Software should either return the input function or prior distributional specification in the return object; or enable direct access to such via additional functions which accept the return object as single argument.*
#' @srrstatsTODO {BS5.3} *Bayesian Software should return convergence statistics or equivalent*
#' @srrstatsTODO {BS5.4} *Where multiple checkers are enabled, Bayesian Software should return details of convergence checker used*
#' @srrstatsTODO {BS5.5} *Appropriate diagnostic statistics to indicate absence of convergence should either be returned or immediately able to be accessed.*
#' @srrstatsTODO {BS6.0} *Software should implement a default `print` method for return objects*
#' @srrstatsTODO {BS6.1} *Software should implement a default `plot` method for return objects*
#' @srrstatsTODO {BS6.2} *Software should provide and document straightforward abilities to plot sequences of posterior samples, with burn-in periods clearly distinguished*
#' @srrstatsTODO {BS6.3} *Software should provide and document straightforward abilities to plot posterior distributional estimates*
#' @srrstatsTODO {BS6.4} *Software may provide `summary` methods for return objects*
#' @srrstatsTODO {BS6.5} *Software may provide abilities to plot both sequences of posterior samples and distributional estimates together in single graphic*
#' @srrstatsTODO {BS7.0} *Software should demonstrate and confirm recovery of parametric estimates of a prior distribution*
#' @srrstatsTODO {BS7.1} *Software should demonstrate and confirm recovery of a prior distribution in the absence of any additional data or information*
#' @srrstatsTODO {BS7.2} *Software should demonstrate and confirm recovery of a expected posterior distribution given a specified prior and some input data*
#' @srrstatsTODO {BS7.3} *Bayesian software should include tests which demonstrate and confirm the scaling of algorithmic efficiency with sizes of input data.*
#' @srrstatsTODO {BS7.4} *Bayesian software should implement tests which confirm that predicted or fitted values are on (approximately) the same scale as input values.*
#' @srrstatsTODO {BS7.4a} *The implications of any assumptions on scales on input objects should be explicitly tested in this context; for example that the scales of inputs which do not have means of zero will not be able to be recovered.*
#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G1.6} ernest does not make performance claims against other R packages, mostly due to a lack of true R-based implementations.
#' @srrstatsNA {G2.4d, G2.4e, G2.5} Ernest does not allow for factor inputs; users provide model information through likelihood functions.
#' @srrstatsNA {BS2.9, BS2.10} Software does not use parallel chains.
#' @srrstatsNA {G2.7, G2.8, G2.9, G2.10, G2.11, G2.12} Ernest does not allow for tabular input, working instead from user-inputted likelihood functions.
#' @srrstatsNA {G3.1, G3.1a} Ernest doesn't presently call a covariance algorithm.
#' @srrstatsNA {G4.0} Ernest does not write to the user's storage.
#'
#' @noRd
NULL
