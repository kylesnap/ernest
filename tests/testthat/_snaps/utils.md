# check_class works as expected

    Code
      check_class(1, "foo")
    Condition
      Error:
      ! `1` must be an object with class foo, not the number 1.

# check_unique_names works as expected

    Code
      check_unique_names(list(a = 1, a = 2))
    Condition
      Error:
      ! All elements of `list(a = 1, a = 2)` must have unique names.
      x Repeated names: a

