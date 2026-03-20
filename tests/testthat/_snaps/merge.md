# merge_sampler / warns and uses default update values when they differ

    Code
      z <- merge_sampler(x, y)
    Condition
      Warning:
      `first_update` values are different between `x` and `y`
      ! Using default `nlive * 2.5`
      Warning:
      `update_interval` values are different between `x` and `y`
      ! Using default `nlive * 1.5`

