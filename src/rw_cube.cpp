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
  // Init. the returned list objects with defaults set with the seed
  Rcpp::NumericVector cur_unit = Rcpp::clone(start);
  Rcpp::NumericVector cur_parameter = prior_transform(cur_unit);
  double cur_log_lik = Rcpp::as<double>(log_lik(cur_parameter));
  double orig_log_lik = cur_log_lik;

  // Init. empty proposal objects
  Rcpp::NumericVector nxt_unit(start.length());
  Rcpp::NumericVector nxt_parameter(start.length());

  int accept = 0;
  int reject = 0;
  for (int stp = 0; stp < steps; ++stp) {
    // Nxt Unit = Cur Unit + Epsilon * Runif(-1, 1)
    nxt_unit = Rcpp::clone(cur_unit);
    for (auto &el : nxt_unit) {
      el += epsilon * R::runif(-1, 1);
      el = (el < 0.0) ? 0.0 : (el > 1.0) ? 1.0 : el;
    }
    nxt_parameter = prior_transform(nxt_unit);
    double nxt_log_lik = Rcpp::as<double>(log_lik(nxt_parameter));
    if (nxt_log_lik >= min_lik) {
      cur_unit = std::move(nxt_unit);
      cur_parameter = std::move(nxt_parameter);
      cur_log_lik = nxt_log_lik;
      ++accept;
    } else {
      ++reject;
    }
    epsilon = accept > reject ? epsilon * exp(1.0 / accept) :
      accept < reject ? epsilon / exp(1.0 / reject) : epsilon;
  }

  return Rcpp::List::create(
    Rcpp::Named("unit") = cur_unit,
    Rcpp::Named("parameter") = cur_parameter,
    Rcpp::Named("log_lik") = cur_log_lik,
    Rcpp::Named("num_calls") = steps
  );
}
