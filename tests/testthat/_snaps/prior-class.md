# check_prior_params throws errors for improper inputs

    `n_dim` must be a whole number larger than or equal to 1, not the number -1.

---

    `n_dim` must be a whole number, not an integer vector.

---

    Can't convert `varnames` <double> to <character>.

---

    Can't recycle `varnames` (size 3) to size 2.

---

    `lower` must be strictly smaller than `upper`.

# create_prior errors if prior function output length is wrong

    `prior(rep(0.5, 2))` must be a double vector of size 2, not 3.

---

    Can't validate `rowwise_fn` as a valid prior.
    Caused by error in `fn()`:
    ! `prior(unit)` must return a matrix of equal dim. to `unit`.
    x Expected dim(y) = 10 x 2.
    x Returned dim(y) = 4 and 5.

# create_prior errors if prior returns non-finite values

    Can't validate `fn` as a valid prior.
    Caused by error in `fn()`:
    ! `prior(unit)` must always return a vector or matrix of doubles.

# create_prior errors with invalid bounds

    `lower` must be strictly smaller than `upper`.

---

    Can't validate `fn` as a valid prior.
    Caused by error:
    ! `prior(matrix(nrow = 10, ncol = 2))` must respect the lower boundary (-Inf and 1).

