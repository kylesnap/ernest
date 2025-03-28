// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// lrps_uniform
Rcpp::List lrps_uniform(Rcpp::Function log_lik_f, Rcpp::Function prior_transform, int num_dim, double criterion, int maxtry);
RcppExport SEXP _ernest_lrps_uniform(SEXP log_lik_fSEXP, SEXP prior_transformSEXP, SEXP num_dimSEXP, SEXP criterionSEXP, SEXP maxtrySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::Function >::type log_lik_f(log_lik_fSEXP);
    Rcpp::traits::input_parameter< Rcpp::Function >::type prior_transform(prior_transformSEXP);
    Rcpp::traits::input_parameter< int >::type num_dim(num_dimSEXP);
    Rcpp::traits::input_parameter< double >::type criterion(criterionSEXP);
    Rcpp::traits::input_parameter< int >::type maxtry(maxtrySEXP);
    rcpp_result_gen = Rcpp::wrap(lrps_uniform(log_lik_f, prior_transform, num_dim, criterion, maxtry));
    return rcpp_result_gen;
END_RCPP
}
// lrps_rwcube
Rcpp::List lrps_rwcube(Rcpp::Function log_lik, Rcpp::Function prior_transform, Rcpp::NumericVector original, const double criterion, const int steps, const double epsilon);
RcppExport SEXP _ernest_lrps_rwcube(SEXP log_likSEXP, SEXP prior_transformSEXP, SEXP originalSEXP, SEXP criterionSEXP, SEXP stepsSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::Function >::type log_lik(log_likSEXP);
    Rcpp::traits::input_parameter< Rcpp::Function >::type prior_transform(prior_transformSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type original(originalSEXP);
    Rcpp::traits::input_parameter< const double >::type criterion(criterionSEXP);
    Rcpp::traits::input_parameter< const int >::type steps(stepsSEXP);
    Rcpp::traits::input_parameter< const double >::type epsilon(epsilonSEXP);
    rcpp_result_gen = Rcpp::wrap(lrps_rwcube(log_lik, prior_transform, original, criterion, steps, epsilon));
    return rcpp_result_gen;
END_RCPP
}
// logaddexp
double logaddexp(double x, double y);
RcppExport SEXP _ernest_logaddexp(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(logaddexp(x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ernest_lrps_uniform", (DL_FUNC) &_ernest_lrps_uniform, 5},
    {"_ernest_lrps_rwcube", (DL_FUNC) &_ernest_lrps_rwcube, 6},
    {"_ernest_logaddexp", (DL_FUNC) &_ernest_logaddexp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_ernest(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
