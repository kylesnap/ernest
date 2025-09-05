#include "RandomData.h"
#include "cpp11_blas.h"
#include <iostream>

[[cpp11::register]]
cpp11::list rwmh(cpp11::doubles original, cpp11::function log_lik_fn,
  double criterion, int steps, double epsilon, cpp11::doubles_matrix<> chol_cov) {
  const int n_dim = original.size();
  RandomData::RandomEngine rand_engine;

  // Setups
  cpp11::writable::doubles prev_draw(original);
  cpp11::writable::doubles next_draw(n_dim);
  cpp11::writable::doubles rand_vec(n_dim);
  size_t n_accept = 0;

  for (size_t draw = 0; draw < steps; draw++) {
    cpp11_blas::dcopy(prev_draw, next_draw);
    RandomData::norm_rand_n(rand_vec);
    cpp11_blas::dsymv(epsilon, chol_cov, rand_vec, 1.0, next_draw);
    bool oob = std::any_of(
      next_draw.cbegin(),
      next_draw.cend(),
      [](double i) { return i < 0.0 || i > 1.0; }
    );
    if (oob) {
      RandomData::reflect(next_draw);
    }
    double log_lik = log_lik_fn(next_draw);
    if (log_lik >= criterion) {
      cpp11_blas::dcopy(next_draw, prev_draw);
      n_accept++;
    }
  }
  using namespace cpp11::literals;
  return cpp11::writable::list({
    "unit"_nm = prev_draw,
    "log_lik"_nm = log_lik_fn(prev_draw),
    "n_call"_nm = steps,
    "n_accept"_nm = n_accept
  });
}
