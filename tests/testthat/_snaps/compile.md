# Informative error when prior or log. lik. fails completely.

    Can't create live points.
    Caused by error:
    ! Can't calculate the log. likelihood.
    x Bad Likelihood Job!

---

    Can't create live points.
    Caused by error:
    ! Can't calculate the prior transformation.
    x Bad prior job!

# check_live validates live points correctly

    Unit points must be stored as a matrix with dim. `c(5, 4)`.

---

    Unit points must be stored as a matrix.

---

    Unit points must be stored as a matrix with dim. `c(5, 4)`.

---

    Unit points must contain only finite values.

---

    Unit points must contain values within [0, 1].

---

    Couldn't avoid calculating non-finite log-likelihood values.
    i Log-likelihood values can only be finite or `-Inf`.
    x There is 1 non-finite, non-`-Inf` value.

---

    Couldn't generate unique log-likelihood values for each point.
    x Every point had a calculated log-lik. value of -10.
    i This generally indicates an error within a log. lik. function.

