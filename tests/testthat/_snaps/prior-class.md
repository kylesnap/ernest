# new_ernest_prior: Explains dimensionality errors

    Code
      new_ernest_prior(fn, n_dim = 0)
    Condition
      Error:
      ! `n_dim` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      new_ernest_prior(fn, n_dim = 3, lower = c(-Inf, 0))
    Condition
      Error in `vctrs::df_list()`:
      ! Can't recycle `lower` (size 2) to size 3.

---

    Code
      new_ernest_prior(fn, lower = c(-Inf, -Inf, 2), upper = c(Inf, Inf))
    Condition
      Error in `vctrs::df_list()`:
      ! Can't recycle `names` (size 3) to match `upper` (size 2).

---

    Can't recycle `names` (size 4) to size 2.

# new_ernest_prior: explains type errors

    Code
      new_ernest_prior(fn, lower = c("0", "0"))
    Condition
      Error in `new_ernest_prior()`:
      ! Can't convert `lower` <character> to <double>.

---

    Code
      new_ernest_prior(fn, names = c(60, 70))
    Condition
      Error in `new_ernest_prior()`:
      ! Can't convert `names` <double> to <character>.

# new_ernest_prior: explains errors with upper and lower

    Code
      new_ernest_prior(fn, lower = c(Inf, 0))
    Condition
      Error:
      ! `lower` must be strictly smaller than `upper`.

---

    Code
      new_ernest_prior(fn, upper = c(0, -Inf))
    Condition
      Error:
      ! `lower` must be strictly smaller than `upper`.

---

    Code
      new_ernest_prior(fn, lower = 0, upper = 0)
    Condition
      Error:
      ! `lower` must be strictly smaller than `upper`.

# create_prior: creates a custom prior

    Code
      prior
    Output
      custom prior distribution <ernest_prior>
      # A tibble: 3 x 3
        names lower upper
        <chr> <dbl> <dbl>
      1 ...1    -10    10
      2 ...2    -10    10
      3 ...3    -10    10

# create_prior: catchers issues with prior function output length

    Code
      create_prior(fn, .n_dim = 2)
    Condition
      Error in `create_prior()`:
      ! `fn` must return a vector of length 2, not one of length 3.

# create_prior: errors if prior returns non-finite values

    Code
      create_prior(fn, .n_dim = 2)
    Condition
      Error in `create_prior()`:
      ! `fn` failed a sanity check.
      x Input: 0.7736 and 0.0321
      x Output: NaN and NaN
      Caused by error in `create_prior()`:
      ! `fn` must return vectors that only contain finite values.

---

    Code
      create_prior(fn, .n_dim = 2)
    Condition
      Error in `create_prior()`:
      ! `fn` failed a sanity check.
      x Input: 0.0245 and 0.184
      x Output: 0.0245 and NA
      Caused by error in `create_prior()`:
      ! `fn` must return vectors that only contain finite values.

---

    Code
      create_prior(fn, .n_dim = 2)
    Condition
      Error in `create_prior()`:
      ! `fn` failed a sanity check.
      x Input: 0.0123 and 0.9578
      x Output: Inf and 0.9578
      Caused by error in `create_prior()`:
      ! `fn` must return vectors that only contain finite values.

# create_prior: errors if prior returns OOB values

    Code
      create_prior(fn, lower = 0.5, .n_dim = 2)
    Condition
      Error in `create_prior()`:
      ! `fn` failed a sanity check.
      x Input: 0.1615 and 0.1666
      x Output: 0.1615 and 0.1666
      Caused by error in `create_prior()`:
      ! `fn` must respect the `lower` bounds.

---

    Code
      create_prior(fn, upper = 0.5, .n_dim = 2)
    Condition
      Error in `create_prior()`:
      ! `fn` failed a sanity check.
      x Input: 0.0339 and 0.5343
      x Output: 0.0339 and 0.5343
      Caused by error in `create_prior()`:
      ! `fn` must respect the `upper` bounds.

# new_ernest_prior repairs names as specified

    Code
      new_ernest_prior(fn = fn, n_dim = 2, names = c("x", "x"), name_repair = "check_unique")
    Condition
      Error:
      ! Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.

