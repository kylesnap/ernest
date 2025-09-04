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
    x Problem at location 2:
    > `$lower`: 2.
    > `$upper`: 2.

---

    `lower` must be strictly smaller than `upper`.
    x Problem at locations 1 and 2:
    > `$lower`: 2 and 2.
    > `$upper`: 1 and 1.

# new_ernest_prior repairs names as specified

    Names must be unique.
    x These names are duplicated:
      * "x" at locations 1 and 2.
    i Use argument `name_repair` to specify repair strategy.

# create_prior warns about depreciated rowwise_fn

    The `rowwise_fn` argument of `create_prior()` is deprecated as of ernest 1.1.0.
    i Please use the `fn` argument instead.

---

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
    ! `prior$fn(x)` must have size 2, not size 3.

# create_prior errors if prior returns non-finite values

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! Priors must only contain finite values, not NaN and NaN.

