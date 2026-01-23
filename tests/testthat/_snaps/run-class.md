# ernest_run / Stores weights as list

    Code
      example_run
    Message
      Nested sampling run:
      * Live points: 500
      * Sampling method: 25-step random walk sampling (acceptance target = 50.0%)
      * Prior: uniform prior distribution with 2 dimensions (A and B)
      -- Results ---------------------------------------------------------------------
      * Iterations: 1000
      * Likelihood evals.: 11203
      * Log-evidence: -7.1156 (± 1.9058)
      * Information: 6.165

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
          "value": [1, 2]
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
          "value": ["A", "B"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.05652933, -0.04601343]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.28723902, 2.34610747]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.03116833, -0.08325333]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-2.59695916, -2.58772456]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.38923974, 2.58079068]
        }
      ]
    }

---

    Code
      smry
    Message
      Summary of nested sampling run:
      -- Run Information -------------------------------------------------------------
      * Live points: 500
      * Iterations: 1000
      * Likelihood evals.: 11203
      * Log-evidence: -7.1156 (± 1.9058)
      * Information: 6.165
      * RNG seed: 42
      -- Posterior Summary -----------------------------------------------------------
    Output
      # A tibble: 2 x 6
        variable    mean    sd  median   q15   q85
        <chr>      <dbl> <dbl>   <dbl> <dbl> <dbl>
      1 A        -0.0565  2.29 -0.0312 -2.60  2.39
      2 B        -0.0460  2.35 -0.0833 -2.59  2.58
    Message
      -- Maximum Likelihood Estimate (MLE) -------------------------------------------
      * Log-likelihood: -0.0672
      * Original parameters: -1.0367 and -1.0001

