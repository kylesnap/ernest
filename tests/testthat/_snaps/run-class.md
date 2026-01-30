# ernest_run / Stores weights as list

    Code
      example_run
    Message
      Nested sampling run:
      * No. points: #
      * Sampling method: #-step random walk sampling (acceptance target = #.#%)
      * Prior: uniform prior distribution with # dimensions (x, y, and z)
      -- Results ---------------------------------------------------------------------
      * Iterations: #
      * Likelihood evals.: #
      * Log-evidence: -#.# (± #.#)
      * Information: #.#

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

