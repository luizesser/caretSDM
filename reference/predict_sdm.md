# Predict SDM models in new data

This function projects SDM models to new scenarios

## Usage

``` r
predict_sdm(m,
            scen = NULL,
            metric = "ROC",
            th = 0.9,
            tp = "prob",
            file = NULL,
            add.current = TRUE)

get_predictions(i)

add_predictions(p1, p2)
```

## Arguments

- m:

  A `input_sdm` or a `models` object.

- scen:

  A `scenarios` object or `NULL`. If `NULL` and `m` is a `input_sdm`
  with a scenarios slot, it will be used.

- metric:

  A character containing the metric in which the `th` will be
  calculated/applied. Default is ROC. See
  [`?mean_validation_metrics`](https://luizesser.github.io/caretSDM/reference/train_sdm.md)
  for the metrics available.

- th:

  Thresholds for metrics. Can be numeric or a function.

- tp:

  Type of output to be retrieved. See details.

- file:

  File to sabe predictions.

- add.current:

  If current scenario is not available, predictors will be used as the
  current scenario.

- i:

  A `input_sdm` or a `predictions` object.

- p1:

  A `predictions` object.

- p2:

  A `predictions` object.

## Value

A `input_sdm` or a `predictions` object.

## Details

`tp` is a parameter to be passed on caret to retrieve either the
probabilities of classes (tp="prob") or the raw output (tp="raw"), which
could vary depending on the algorithm used, but usually would be on of
the classes (factor vector with presences and pseudoabsences).

`get_predictions` returns the `list` of all predictions to all
scenarios, all species, all algorithms and all repetitions. Useful for
those who wish to implement their own ensemble methods.

`scenarios_names` returns the scenarios names in a `sdm_area` or
`input_sdm` object.

`get_scenarios_data` returns the data from scenarios in a `sdm_area` or
`input_sdm` object.

## See also

[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)` `[`mean_validation_metrics`](https://luizesser.github.io/caretSDM/reference/train_sdm.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
if (interactive()) {
  # Create sdm_area object:
  set.seed(1)
  sa <- sdm_area(parana, cell_size = 100000, crs = 6933)

  # Include predictors:
  sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))

  # Include scenarios:
  sa <- add_scenarios(sa)

  # Create occurrences:
  oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)

  # Create input_sdm:
  i <- input_sdm(oc, sa)

  # Pseudoabsence generation:
  i <- pseudoabsences(i, method="random", n_set=2)

  # Custom trainControl:
  ctrl_sdm <- caret::trainControl(method = "boot",
                                  number = 1,
                                  repeats = 1,
                                  classProbs = TRUE,
                                  returnResamp = "all",
                                  summaryFunction = summary_sdm,
                                  savePredictions = "all")

  # Train models:
  i <- train_sdm(i, algo = c("naive_bayes"), ctrl=ctrl_sdm) |>
    suppressWarnings()

  # Predict models:
  i  <- predict_sdm(i, th = 0.8)
  i
}
```
