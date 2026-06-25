# ernest_run / Stores run record and valid weights

    Code
      glance(example_run)
    Output
      # A tibble: 1 x 8
        nlive  nvar niter  neval log_evidence log_evidence_err information  seed
        <int> <int> <int>  <int>        <dbl>            <dbl>       <dbl> <int>
      1  1000     3  9362 228698        -9.03           0.0827        4.86    42

---

    Code
      example_run
    Message
      Nested sampling run:
      * No. points: #
      * Sampling method: #-step random walk sampling (acceptance target = #%)
      * Prior: <uniform_prior/ernest_prior> (# dims.)
      -- Results ---------------------------------------------------------------------
      * Iterations: #
      * Likelihood evals.: #
      * Log-evidence: -#.# (± #.#)
      * Information: #.#

# summary.ernest_run returns expected structure and values / has the correct meta-info

    Code
      smry
    Message
      Summary of nested sampling run:
      -- Run Information -------------------------------------------------------------
      * No. points: #
      * Iterations: #
      * Likelihood evals.: #
      * Log-evidence: -#.# (± #.#)
      * Information: #.#
      * RNG seed: #
      -- Posterior Summary -----------------------------------------------------------
    Output
      # A tibble: # x #
        variable     mean    sd   median   q#   q#
        <chr>       <dbl> <dbl>    <dbl> <dbl> <dbl>
      # x        -#.#   #.# -#.# -#.#  #.#
      # y         #.#  #.#  #.#  -#.#  #.#
      # z         #.#   #.#  #.# -#.#  #.#
    Message
      -- Maximum Likelihood Estimate (MLE) -------------------------------------------
      * Log-likelihood: -#.#
      * Original parameters: #.#, #.#, and #.#

# summary.ernest_run returns expected structure and values / has the expected posterior

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["variable", "mean", "sd", "median", "q15", "q85"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["draws_summary", "tbl_df", "tbl", "data.frame"]
        },
        "num_args": {
          "type": "list",
          "attributes": {},
          "value": []
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["x", "y", "z"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.00571574, 0.03017027, -0.0037809]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.79226024, 2.80469196, 2.82475901]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.0173789, 0.01649964, 0.01920456]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-1.93985718, -1.90605249, -1.96360447]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.99558637, 2.03935854, 1.96687335]
        }
      ]
    }

