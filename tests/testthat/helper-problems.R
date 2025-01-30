# A completely flat likelihood and prior distribution
make_flat <- function(num_dim) {
  list(
    log_lik = function(x) 0,
    prior_transform = prior_transform(\(x) x, num_dim, LETTERS[1:num_dim])
  )
}
