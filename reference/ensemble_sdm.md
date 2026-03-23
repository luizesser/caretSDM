# Predict SDM models in new data

This function projects SDM models to new scenarios

## Usage

``` r
ensemble_sdm(m,
            scen = NULL,
            method = "average",
            metric = NULL,
            fun = NULL
            )

get_ensembles(i)

add_ensembles(e1, e2)
```

## Arguments

- m:

  A `input_sdm` or a `models` object.

- scen:

  A `scenarios` object or `NULL`. If `NULL` and `m` is a `input_sdm`
  with a scenarios slot, it will be used.

- method:

  Character or a function. Which ensembles should be calculated? See
  details.

- metric:

  Character. Used with `method = "weighted_average"`: Which metric
  should be used to weight predictions? If NULL

- fun:

  Function. If `method = "committee_average"`, the function will be used
  to binarize the data. It will receive caret's train object and must
  return a numeric value (the threshold, see details).

- i:

  A `input_sdm` or a `predictions` object.

- e1:

  A `ensembles` object.

- e2:

  A `ensembles` object.

## Value

A `input_sdm` or a `predictions` object.

## Details

`ensembles` could be set to three different strategies OR a custom
function. The three implemented strategies are: `average` is the mean
occurrence probability, which is a simple mean of predictions;
`weighted_average` is the same average, but weighted by a metric, which
needs to be set using argument `metric` (see
[mean_validation_metrics](https://luizesser.github.io/caretSDM/reference/train_sdm.md)
for the metrics available). `committee_average` is the committee
average, as known as majority rule, where predictions are binarized and
then a mean is obtained. To binarize predictions, user can set a custom
function in the `fun` argument to calculate a threshold for each model.
Standardly, the committee average uses the
[`caret::thresholder`](https://rdrr.io/pkg/caret/man/thresholder.html)
function to find the threshold that maximizes the sum of sensitivity and
specificity (through `caretSDM:::.MaxSeSp`). The custom function must
use the argument `mod`, which is the model output from caret package
(see
[`get_models`](https://luizesser.github.io/caretSDM/reference/train_sdm.md))
and must return a `numeric` value (see example).

`get_predictions` returns the `list` of all predictions to all
scenarios, all species, all algorithms and all repetitions. Useful for
those who wish to implement their own ensemble methods.

`get_ensembles` returns a `matrix` of `data.frame`s, where each column
is a scenario and each row is a species.

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

  # Ensemble:
  i <- ensemble_sdm(i, method = "average")
  i
}

# Example from a custom function to obtain the threshold that maximizes
# the sensitivity plus specificity:
MaxSeSp <- function(mod) {
   th <- caret::thresholder(mod,
                            threshold = seq(0, 1, by = 0.001),
                            final = TRUE,
                            statistics = c("Sensitivity", "Specificity")
                            )
   th <- th$prob_threshold[which.max(th$Sensitivity + th$Specificity)]
   if (length(th) > 1) mean(th) else th
}
```
