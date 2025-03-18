#include <Rcpp.h>
#include <algorithm> // for std::min

#include "utils.h"

/**
 * Propose new parameters using a random walk within a unit cube.
 *
 * @param log_lik A function that computes the log-likelihood of the parameters.
 * @param prior_transform A function that transforms unit cube values to parameter values.
 * @param original A numeric vector representing the starting point in the unit cube.
 * @param criterion The minimum log-likelihood threshold for accepting a proposal.
 * @param max_try The maximum number of proposals to try before giving up.
 * @param min_steps The number of steps to perform in the random walk.
 * @param epsilon The step size for the random walk.
 * @return A list containing the final unit cube values, the corresponding parameters,
 *         the log-likelihood of the final parameters, and the number of steps performed.
 */
// [[Rcpp::export]]
Rcpp::List lrps_rwcube(Rcpp::Function log_lik,
                       Rcpp::Function prior_transform,
                       Rcpp::NumericVector original,
                       const double criterion,
                       const int steps,
                       const double epsilon) {
  Rcpp::NumericVector cur_unit = Rcpp::clone(original);
  Rcpp::NumericVector cur_param = prior_transform(cur_unit);
  double cur_loglik = Rcpp::as<double>(log_lik(cur_param));

  int accept = 0;

  for (int s = 0; s < steps; s++) {
    Rcpp::NumericVector new_unit = Rcpp::clone(cur_unit);
    bool fail = Ernest::offset_sphere(new_unit, cur_unit, epsilon);
    if (fail) {
      continue;
    }
    Rcpp::NumericVector new_param = prior_transform(new_unit);
    double new_loglik = Rcpp::as<double>(log_lik(new_param));
    if (R_FINITE(new_loglik) && new_loglik >= criterion) {
      cur_unit = std::move(new_unit);
      cur_param = std::move(new_param);
      cur_loglik = new_loglik;
      accept++;
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("unit") = cur_unit,
    Rcpp::Named("parameter") = cur_param,
    Rcpp::Named("log_lik") = cur_loglik,
    Rcpp::Named("num_calls") = steps,
    Rcpp::Named("num_acc") = accept
  );
}
