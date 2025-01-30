#include <Rcpp.h>

/**
 * Propose a new point uniformly from the unit hypercube and transform it using the prior.
 *
 * @param log_lik A function that computes the log-likelihood of a given parameter vector.
 * @param prior_transform A function that transforms a point from the unit hypercube to the parameter space.
 * @param num_dim The number of dimensions of the parameter space.
 * @param min_lik The minimum log-likelihood value that is considered acceptable.
 * @param max_attempts The maximum number of attempts to find a valid point.
 *
 * @return A list containing:
 *   - "unit": The proposed point in the unit hypercube.
 *   - "parameter": The transformed parameter vector.
 *   - "log_lik": The log-likelihood value of the proposed parameter vector.
 *   - "num_calls": The number of attempts made to find a valid point.
 *
 * @throws Rcpp::exception if a valid point could not be found after max_attempts tries.
 */
// [[Rcpp::export]]
Rcpp::List propose_uniform_(Rcpp::Function log_lik, Rcpp::Function prior_transform,
                            int num_dim, double min_lik, int max_attempts) {
  Rcpp::NumericVector unit(num_dim);
  Rcpp::NumericVector parameter(num_dim);
  double log_lik_value;

  for (int i = 0; i < max_attempts; ++i) {
    for (int j = 0; j < num_dim; ++j) {
      unit[j] = R::runif(0, 1);
    }
    parameter = prior_transform(unit);
    log_lik_value = Rcpp::as<double>(log_lik(parameter));
    if (log_lik_value >= min_lik) {
      return Rcpp::List::create(
        Rcpp::Named("unit") = unit,
        Rcpp::Named("parameter") = parameter,
        Rcpp::Named("log_lik") = log_lik_value,
        Rcpp::Named("num_calls") = i + 1
      );
    }
  }
  Rcpp::stop("Could not find a valid point after max_attempts tries.");
}
