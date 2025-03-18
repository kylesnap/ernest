#include "utils.h"

/**
 * Propose a new point uniformly from the unit hypercube and transform it using the prior.
 *
 * @param log_lik A function that computes the log-likelihood of a given parameter vector.
 * @param prior_transform A function that transforms a point from the unit hypercube to the parameter space.
 * @param num_dim The number of dimensions of the parameter space.
 * @param criterion The minimum log-likelihood value that is considered acceptable.
 * @param maxtry The maximum number of attempts to find a valid point.
 */
// [[Rcpp::export]]
Rcpp::List lrps_uniform(Rcpp::Function log_lik_f,
                            Rcpp::Function prior_transform,
                            int num_dim,
                            double criterion,
                            int maxtry) {
  Rcpp::NumericVector unit(num_dim);
  Rcpp::NumericVector parameter(num_dim);
  double log_lik = 0.0;

  for (int i = 0; i < maxtry; ++i) {
    Ernest::runif_cube(unit);
    parameter = prior_transform(unit);
    log_lik = Rcpp::as<double>(log_lik_f(parameter));
    if ((R_FINITE(log_lik) || log_lik == R_NegInf) && log_lik >= criterion) {
      return Rcpp::List::create(
        Rcpp::Named("unit") = unit,
        Rcpp::Named("parameter") = parameter,
        Rcpp::Named("log_lik") = log_lik,
        Rcpp::Named("num_calls") = i + 1
      );
    }
  }
  return(
    Rcpp::List::create(Rcpp::Named("num_calls") = maxtry)
  );
}
