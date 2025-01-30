#include <Rcpp.h>

/**
 * Propose new parameters using a random walk within a unit cube.
 *
 * @param log_lik A function that computes the log-likelihood of the parameters.
 * @param prior_transform A function that transforms unit cube values to parameter values.
 * @param start A numeric vector representing the starting point in the unit cube.
 * @param min_lik The minimum log-likelihood threshold for accepting a proposal.
 * @param steps The number of steps to perform in the random walk.
 * @param epsilon The step size for the random walk.
 * @return A list containing the final unit cube values, the corresponding parameters,
 *         the log-likelihood of the final parameters, and the number of steps performed.
 */
// [[Rcpp::export]]
Rcpp::List propose_rwcube_(Rcpp::Function log_lik, Rcpp::Function prior_transform,
                            Rcpp::NumericVector start, double min_lik,
                            int steps, double epsilon) {
  Rcpp::NumericVector cur_unit(start), proposed_unit(start.length());
  Rcpp::NumericVector cur_param(prior_transform(cur_unit)), proposed_param(start.length());
  double cur_log_lik = Rcpp::as<double>(log_lik(cur_param));
  int accept = 0;
  int reject = 0;

  for (int stp = 0; stp < steps; ++stp) {
    auto cur_iter = cur_unit.begin();
    for (auto &el : proposed_unit) {
      el = (*cur_iter) + epsilon * R::runif(-1, 1);
      el = (el < 0.0) ? 0.0 : (el > 1.0) ? 1.0 : el;
      ++cur_iter;
    }
    proposed_param = prior_transform(proposed_unit);
    double new_logl = Rcpp::as<double>(log_lik(proposed_param));
    if (new_logl >= min_lik) {
      cur_unit = proposed_unit;
      cur_param = proposed_param;
      cur_log_lik = new_logl;
      ++accept;
    } else {
      ++reject;
    }
    epsilon = accept > reject ? epsilon * exp(1.0 / accept) :
      accept < reject ? epsilon / exp(1.0 / reject) : epsilon;
  }

  return Rcpp::List::create(
    Rcpp::Named("unit") = cur_unit,
    Rcpp::Named("parameter") = cur_param,
    Rcpp::Named("log_lik") = cur_log_lik,
    Rcpp::Named("num_calls") = steps
  );
}
