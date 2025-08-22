# new_ernest_prior throws informative errors

    `n_dim` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Can't recycle `lower` (size 2) to size 3.

---

    Can't convert `lower` <character> to <double>.

---

    Can't recycle `lower` (size 2) to size 3.

# new_ernest_prior errors if lower >= upper

    `lower` must be strictly smaller than `upper`.
    x Problem at index 2.

---

    `lower` must be strictly smaller than `upper`.
    x Problem at indicies 1 and 2.

# new_ernest_prior repairs names as specified

    Names must be unique.
    x These names are duplicated:
      * "x" at locations 1 and 2.
    i Use argument `name_repair` to specify repair strategy.

# create_prior creates a custom rowwise prior

    Code
      prior
    Output
      Prior distribution <ernest_prior>
      
      Names: "...1", "...2", and "...3"
      Bounds:
      > Lower: -10, -10, and -10
      > Upper: 10, 10, and 10

# create_prior errors if prior function output length is wrong

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! `y` must have size 2, not size 3.

---

    Can't validate `rowwise_fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! `prior(unit)` must return a matrix of equal dim. to `unit`.
    x Expected dim(y) = 10 x 2.
    x Returned dim(y) = 4 and 5.

# create_prior errors if prior returns non-finite values

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! `prior(unit)` must never return `NA` or `NaN` values.

