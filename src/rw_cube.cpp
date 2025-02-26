#include <Rcpp.h>

#include "utils.h"

/**
 * Propose new parameters using a random walk within a unit cube.
 *
 * @param log_lik A function that computes the log-likelihood of the parameters.
 * @param prior_transform A function that transforms unit cube values to parameter values.
 * @param start A numeric vector representing the starting point in the unit cube.
 * @param min_lik The minimum log-likelihood threshold for accepting a proposal.
 * @param max_try The maximum number of proposals to try before giving up.
 * @param min_steps The number of steps to perform in the random walk.
 * @param epsilon The step size for the random walk.
 * @return A list containing the final unit cube values, the corresponding parameters,
 *         the log-likelihood of the final parameters, and the number of steps performed.
 */
// [[Rcpp::export]]
Rcpp::List propose_rwcube_(Rcpp::Function log_lik, Rcpp::Function prior_transform,
                           Rcpp::NumericVector original, double min_lik,
                           int max_try, int min_steps, double epsilon) {
  Rcpp::NumericVector cur_unit = Rcpp::clone(original);
  Rcpp::NumericVector cur_param = prior_transform(cur_unit);
  Rcpp::NumericVector new_unit(original.size());
  Rcpp::NumericVector new_param(original.size());
  double cur_loglik = Rcpp::as<double>(log_lik(cur_param));

  int accept = 0;
  int reject = 0;
  int step = 0;
  while (step < min_steps || (accept < 1 && step < max_try)) {
    // Update the step size
    if (accept > reject) {
      epsilon *= std::exp(1.0 / accept);
    } else if (accept < reject) {
      epsilon /= std::exp(1.0 / reject);
    }
    // Add offset point to current unit
    bool fail = Ernest::offset_sphere(new_unit, cur_unit, epsilon);
    if (fail) {
      reject++;
      step++;
      continue;
    }
    new_param = prior_transform(new_unit);
    double new_loglik = Rcpp::as<double>(log_lik(new_param));
    if (new_loglik >= min_lik) {
      cur_unit = Rcpp::clone(new_unit);
      cur_param = new_param;
      cur_loglik = new_loglik;
      accept += 1;
    } else {
      reject += 1;
    }
    step++;
  }

  if (step >= max_try) {
    Rcpp::stop("Maximum number of steps reached.");
  }

  return Rcpp::List::create(
    Rcpp::Named("unit") = cur_unit,
    Rcpp::Named("parameter") = cur_param,
    Rcpp::Named("log_lik") = cur_loglik,
    Rcpp::Named("num_calls") = step
  );
}
