# ernest_sampler errors with invalid prior

    `prior` must be an object with class ernest_prior, not a function.

# Fully-verbose output [plain]

    Code
      generate(sampler, max_iterations = 1000)
    Message
      i Created 500 live points.
      v `max_iterations` reached (1000).
      nested sampling results <ernest_run/ernest_sampler>
      * No. Points: 500
      * LRPS Method: rwmh_cube
      --------------------------------------------------------------------------------
      * No. Iterations: 1000
      * No. Calls: ###
      * Log. Evidence: ###

# Fully-verbose output [ansi]

    Code
      generate(sampler, max_iterations = 1000)
    Message
      [36mi[39m Created 500 live points.
      [32mv[39m `max_iterations` reached (1000).
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      [36m*[39m No. Points: 500
      [36m*[39m LRPS Method: rwmh_cube
      --------------------------------------------------------------------------------
      [36m*[39m No. Iterations: 1000
      [36m*[39m No. Calls: ###
      [36m*[39m Log. Evidence: ###

# Fully-verbose output [unicode]

    Code
      generate(sampler, max_iterations = 1000)
    Message
      ℹ Created 500 live points.
      ✔ `max_iterations` reached (1000).
      nested sampling results <ernest_run/ernest_sampler>
      • No. Points: 500
      • LRPS Method: rwmh_cube
      ────────────────────────────────────────────────────────────────────────────────
      • No. Iterations: 1000
      • No. Calls: ###
      • Log. Evidence: ###

# Fully-verbose output [fancy]

    Code
      generate(sampler, max_iterations = 1000)
    Message
      [36mℹ[39m Created 500 live points.
      [32m✔[39m `max_iterations` reached (1000).
      nested sampling results [34m<ernest_run/ernest_sampler>[39m
      [36m•[39m No. Points: 500
      [36m•[39m LRPS Method: rwmh_cube
      ────────────────────────────────────────────────────────────────────────────────
      [36m•[39m No. Iterations: 1000
      [36m•[39m No. Calls: ###
      [36m•[39m Log. Evidence: ###

