# new_ernest_prior / Explains dimensionality errors

    Code
      new_ernest_prior(fn)
    Condition
      Error in `new_ernest_prior()`:
      ! argument "names" is missing, with no default

---

    Code
      new_ernest_prior(fn, LETTERS[1:3], lower = c(-Inf, 0))
    Condition
      Error in `new_ernest_prior()`:
      ! Can't recycle `lower` (size 2) to size 3.

---

    Code
      new_ernest_prior(fn, LETTERS[1:3], lower = c(-Inf, -Inf, 2), upper = c(Inf, Inf))
    Condition
      Error in `new_ernest_prior()`:
      ! Can't recycle `upper` (size 2) to size 3.

# new_ernest_prior / explains type errors

    Code
      new_ernest_prior(fn = "mean")
    Condition
      Error in `new_ernest_prior()`:
      ! argument "names" is missing, with no default

---

    Code
      new_ernest_prior(fn, lower = c("0", "0"))
    Condition
      Error in `new_ernest_prior()`:
      ! argument "names" is missing, with no default

# new_ernest_prior / explains errors with explicit upper and lower

    Code
      new_ernest_prior(fn, names = LETTERS[1:2], lower = c(Inf, 0))
    Condition
      Error in `new_ernest_prior()`:
      ! `lower` must be strictly smaller than `upper`.
      x Problem at index 1: `Inf ≮ 1`

---

    Code
      new_ernest_prior(fn, names = LETTERS[1:2], upper = c(0, -Inf))
    Condition
      Error in `new_ernest_prior()`:
      ! `lower` must be strictly smaller than `upper`.
      x Problem at index 1: `0 ≮ 0`

---

    Code
      new_ernest_prior(fn, names = LETTERS[1:2], lower = 0, upper = 0)
    Condition
      Error in `new_ernest_prior()`:
      ! `lower` must be strictly smaller than `upper`.
      x Problem at index 1: `0 ≮ 0`

# c.ernest_prior / Errors when concatenated with non-prior

    Code
      c(unif_p, qnorm)
    Condition
      Error in `c()`:
      ! Can't add <function> objects to an `ernest_prior`.

# create_prior / creates a custom prior

    Code
      prior
    Output
      custom prior distribution <custom_prior/ernest_prior>
      
      Number of Dimensions: 2
      Names: A and B

