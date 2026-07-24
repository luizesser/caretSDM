# Train a Stacked Ensemble for SDM

This function builds a meta-model (Layer 2) using the out-of-fold
predictions from models trained in Layer 1.

## Usage

``` r
stack_sdm(m, meta_algo = "glm", ctrl = NULL, ...)
```

## Arguments

- m:

  A `models` or `input_sdm` object.

- meta_algo:

  A character string specifying the algorithm for the meta-learner.

- ctrl:

  A `trainControl` object for the meta-learner. If NULL, a simple CV is
  used.

- ...:

  Additional arguments passed to
  [`caret::train`](https://rdrr.io/pkg/caret/man/train.html).

## Value

A `stacked_models` object.

## See also

[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`algorithms`](https://luizesser.github.io/caretSDM/reference/algorithms.md)` `[`train_sdm`](https://luizesser.github.io/caretSDM/reference/train_sdm.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
if (interactive()) {
  # Create sdm_area object:
  set.seed(1)
  sa <- sdm_area(parana, cell_size = 100000, output_crs = 6933)

  # Include predictors:
  sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))

  # Include scenarios:
  sa <- add_scenarios(sa)

  # Create occurrences:
  oc <- occurrences_sdm(occ, occ_crs = 6933)

  # Create input_sdm:
  i <- input_sdm(oc, sa)

  # Pseudoabsence generation:
  i <- pseudoabsences(i, method = "random", n_set = 2)

  # Custom trainControl:
  ctrl_sdm <- caret::trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 1,
    classProbs = TRUE,
    returnResamp = "all",
    summaryFunction = summary_sdm,
    savePredictions = "all"
  )

  # Train models:
  i <- train_sdm(i, algo = c("naive_bayes", "kknn"), ctrl = ctrl_sdm) |>
    suppressWarnings()

  # Train stacked ensemble:
  i <- stack_sdm(i, meta_algo = "nnet", ctrl = ctrl_sdm)

  # Prediction of stacked models is still under development.
}
```
