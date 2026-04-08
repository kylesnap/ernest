# ernest_prior / produces scalar likelihoods

    Code
      pr
    Output
      custom prior distribution with 3 dimensions (A, B, and C)

# ernest_prior / produces prior from `vectorized_fn`

    Code
      mat_pr
    Output
      custom prior distribution with 3 dimensions (A, B, and C)

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
      Error:
      ! Can't convert `prior$fn(x)` <double[,0]> to <double[,1]>.
      Non-recyclable dimensions.

