# Predict SDM models in new data

This function projects SDM models to new scenarios

## Usage

``` r
predict_sdm(m,
            scen = NULL,
            metric = "ROC",
            th = 0.9,
            tp = "prob",
            ensembles = TRUE,
            file = NULL,
            add.current = TRUE)

get_predictions(i)

get_ensembles(i)

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

- ensembles:

  Boolean. Should ensembles be calculated? If `TRUE` a series of
  ensembles are obtained. See details.

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

When `ensembles` is set to `TRUE`, three ensembles are currently
implemented. mean_occ_prob is the mean occurrence probability, which is
a simple mean of predictions, wmean_AUC is the same mean_occ_prob, but
weighted by AUC, and committee_avg is the committee average, as known as
majority rule, where predictions are binarized and then a mean is
obtained.

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
# Create sdm_area object:
set.seed(1)
sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include predictors:
sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

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
#> Warning: `repeats` has no meaning for this resampling method.

# Train models:
i <- train_sdm(i, algo = c("naive_bayes"), ctrl=ctrl_sdm) |>
  suppressWarnings()

# Predict models:
i  <- predict_sdm(i, th = 0.8)
#> [1] "Projecting: 1/1"
#> [1] "Ensembling..."
#> [1] "current"
#> [1] "Araucaria angustifolia"
i
#>             caretSDM           
#> ...............................
#> Class                         : input_sdm
#> --------  Occurrences  --------
#> Species Names                 : Araucaria angustifolia 
#> Number of presences           : 420 
#> Pseudoabsence methods         :
#>     Method to obtain PAs      : random 
#>     Number of PA sets         : 2 
#>     Number of PAs in each set : 420 
#> --------  Predictors  ---------
#> Number of Predictors          : 2 
#> Predictors Names              : bio1, bio12 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 1 
#> Scenarios Names               : current 
#> -----------  Models  ----------
#> Algorithms Names              : naive_bayes 
#> Variables Names               : bio1 bio12 
#> Model Validation              :
#>     Method                    : boot 
#>     Number                    : 1 
#>     Metrics                   :
#> $`Araucaria angustifolia`
#>          algo       ROC       TSS Sensitivity Specificity
#> 1 naive_bayes 0.8799581 0.4247848       0.974       0.451
#> 
#> --------  Predictions  --------
#> Ensembles                     :
#>     Scenarios                 : current 
#>     Methods                   : mean_occ_prob wmean_AUC committee_avg 
#> Thresholds                    :
#>     Method                    : threshold 
#>     Criteria                  : 0.8 
```
