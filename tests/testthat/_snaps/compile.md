# Passing a poorly formed LRPS raises errors

    Error when generating live points:
    Caused by error in `create_live()`:
    ! `prior_transform` produced non-finite values.

---

    Error when evaluating `log_lik`:
    Caused by error in `create_live()`:
    ! `log_lik` produced non-finite values.

---

    Warning when evaluating `log_lik`:
    Caused by warning:
    ! `log_lik` produced -Inf values.

# create_live throws errors as expected

    Error when generating live points:
    Caused by error in `create_live()`:
    ! `prior_transform` produced non-finite values.

---

    Error when evaluating `log_lik`:
    Caused by error in `create_live()`:
    ! `log_lik` must be a vector of length 100.

---

    Error when evaluating `log_lik`:
    Caused by error in `create_live()`:
    ! `log_lik` produced non-finite values.

---

    Error when evaluating `log_lik`:
    Caused by error in `create_live()`:
    ! `log_lik` produced non-finite values.

