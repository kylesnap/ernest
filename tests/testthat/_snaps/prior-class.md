# ernest_prior / produces scalar likelihoods

    Code
      pr
    Message
      <custom_prior/ernest_prior> (3 dims.)
      <function(x) {
        stats::qnorm(x, mean = c(-1, 0, 1))
      }>
      v Names: "A", "B", and "C"
      v Interface: "point_fn"

# ernest_prior / produces prior from `vectorized_fn`

    Code
      mat_pr
    Message
      <custom_prior/ernest_prior> (3 dims.)
      <function(x) {
        y <- stats::qnorm(c(x), mean = rep(c(-1, 0, 1), each = nrow(x) %||% 1))
        dim(y) <- dim(x)
        y
      }>
      v Names: "A", "B", and "C"
      v Interface: "vectorized_fn"

# Zero-length prior fails

    Code
      create_prior(prior_fn, names = character())
    Condition
      Error in `new_ernest_prior()`:
      ! `names` must be at least length one, not length 0.

---

    Code
      create_prior(prior_fn, names = LETTERS[1])
    Condition
      Error in `vectorized_prior()`:
      ! Can't convert `y` <double[,0]> to <double[,1]>.
      Non-recyclable dimensions.

