# Ensemble GCMs into one scenario

An ensembling method to group different GCMs into one SSP scenario

## Usage

``` r
gcms_ensembles(i, gcms = NULL)
```

## Arguments

- i:

  A `input_sdm` object.

- gcms:

  GCM codes in `scenarios_names(i)` to group scenarios.

## Value

A `input_sdm` object with grouped GCMs.

## See also

[`GBIF_data`](https://luizesser.github.io/caretSDM/reference/GBIF_data.md)` `[`occurrences_sdm`](https://luizesser.github.io/caretSDM/reference/occurrences_sdm.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)` `[`get_predictor_names`](https://luizesser.github.io/caretSDM/reference/predictor_names.md)

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

  # Ensemble:
  i <- ensemble_sdm(i, method = "average")
  i

  # Ensemble GCMs:
  i <- gcms_ensembles(i, gcms = c("ca", "mi"))
  i
}
```
