# Prediction Change Analysis

Provides an automate way for the visualization of projections gain,
loss, and stability between different scenarios.

## Usage

``` r
prediction_change_sdm(i, scenario = NULL, ensemble_type = NULL, species = NULL, th = 0.5)
```

## Arguments

- i:

  A `input_sdm` object with projections.

- scenario:

  Character. One of the scenarios that were projected. Can be ensembles
  as well.

- ensemble_type:

  Character. Type of ensemble to be used. Standard is NULL, but will
  return the mean_occ_prob

- species:

  Character. Species to be analyzed. Standard is NULL.

- th:

  Numeric. Threshold to binarize the ensemble.

## Value

A plot with comparison between current and other scenario.

## See also

[`species_names`](https://luizesser.github.io/caretSDM/reference/occurrences_sdm.md)` `[`scenarios_names`](https://luizesser.github.io/caretSDM/reference/add_scenarios.md)

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
  sa <- add_predictors(sa, bioc)

  # Include scenarios:
  sa <- add_scenarios(sa, scen) |> select_predictors(c("bio1", "bio12"))

  # Create occurrences:
  oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)

  # Create input_sdm:
  i <- input_sdm(oc, sa)

  # Pseudoabsence generation:
  i <- pseudoabsences(i, method="random", n_set = 2)

  # Custom trainControl:
  ctrl_sdm <- caret::trainControl(method = "boot",
                                  number = 1,
                                  classProbs = TRUE,
                                  returnResamp = "all",
                                  summaryFunction = summary_sdm,
                                  savePredictions = "all")

  # Train models:
  i <- train_sdm(i,
                 algo = c("naive_bayes"),
                 ctrl=ctrl_sdm,
                 variables_selected = c("bio1", "bio12")) |>
    suppressWarnings()

  # Predict models:
  i  <- predict_sdm(i, th=0.8)

  # Ensemble GCMs:
  i <- gcms_ensembles(i, gcms = c("ca", "mi"))
  i

  # Change Analysis
  prediction_change_sdm(i, scenario = "_ssp585_2090", ensemble_type = "mean_occ_prob")
}
```
