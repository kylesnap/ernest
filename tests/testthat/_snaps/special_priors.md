# create_normal_prior error handling

    All elements of `sd` must be strictly positive and non-missing.

# create_normal_prior and create_uniform_prior: print methods

    Code
      create_normal_prior(mean = 0, sd = 1)
    Output
      custom prior distribution <normal_prior/ernest_prior>
      # A tibble: 1 x 3
        names  lower upper
        <chr>  <dbl> <dbl>
      1 Normal  -Inf   Inf

---

    Code
      create_uniform_prior(lower = 0, upper = 1)
    Output
      custom prior distribution <uniform_prior/ernest_prior>
      # A tibble: 1 x 3
        names   lower upper
        <chr>   <dbl> <dbl>
      1 Uniform     0     1

