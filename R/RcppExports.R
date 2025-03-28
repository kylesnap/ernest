# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

lrps_uniform <- function(log_lik_f, prior_transform, num_dim, criterion, maxtry) {
    .Call(`_ernest_lrps_uniform`, log_lik_f, prior_transform, num_dim, criterion, maxtry)
}

lrps_rwcube <- function(log_lik, prior_transform, original, criterion, steps, epsilon) {
    .Call(`_ernest_lrps_rwcube`, log_lik, prior_transform, original, criterion, steps, epsilon)
}

logaddexp <- function(x, y) {
    .Call(`_ernest_logaddexp`, x, y)
}

