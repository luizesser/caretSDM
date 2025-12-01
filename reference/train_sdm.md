# Train SDM models

This function is a wrapper to fit models in caret using caretSDM data.

## Usage

``` r
train_sdm(occ,
          pred = NULL,
          algo,
          ctrl = NULL,
          variables_selected = NULL,
          parallel = FALSE,
          ...)

get_tune_length(i)

algorithms_used(i)

get_models(i)

get_validation_metrics(i)

mean_validation_metrics(i)

add_models(m1, m2)
```

## Arguments

- occ:

  A `occurrences` or a `input_sdm` object.

- pred:

  A `predictors object`. If `occ` is a `input_sdm` object, then `pred`
  is obtained from it.

- algo:

  A `character` vector. Algorithms to be used. For a complete list see
  (https://topepo.github.io/caret/available-models.html) or in
  caretSDM::algorithms.

- ctrl:

  A `trainControl` object to be used to build models. See
  [`?caret::trainControl`](https://rdrr.io/pkg/caret/man/trainControl.html)
  and details.

- variables_selected:

  A `vector` of variables to be used as predictors. If `NULL`,
  predictors names from `pred` will be used. Can also be a selection
  method (e.g. 'vif').

- parallel:

  Should a paralelization method be used (not yet implemented)?

- ...:

  Additional arguments to be passed to
  [`caret::train`](https://rdrr.io/pkg/caret/man/train.html) function.

- i:

  A `models` or a `input_sdm` object.

- m1:

  A `models` object.

- m2:

  A `models` object.

## Value

A `models` or a `input_sdm` object.

## Details

The object
[`algorithms`](https://luizesser.github.io/caretSDM/reference/algorithms.md)
has a table comparing algorithms available. If the function detects that
the necessary packages are not available it will ask for installation.
This will happen just in the first time you use the algorithm.
[`caret::trainControl`](https://rdrr.io/pkg/caret/man/trainControl.html)
holds multiple resources for validation and model tuning. Make sure to
understand its parameters beforehand. As it is a key function in the
modeling process, we also implemented spatial crossvalidation on it. You
can set `methods` to be `cv_spatial` or `cv_cluster` and `train_sdm`
will detect that and apply the method according to `blockCV` package.

`get_tune_length` return the length used in grid-search for tunning.

`algorithms_used` return the names of the algorithms used in the
modeling process.

`get_models` returns a `list` with trained models (class `train`) to
each species.

`get_validation_metrics` return a `list` with a `data.frame` to each
species with complete values for ROC, Sensitivity, Specificity, with
their respectives Standard Deviations (SD) and TSS to each of the
algorithms and pseudoabsence datasets used.

`mean_validation_metrics` return a `list` with a `tibble` to each
species summarizing values for ROC, Sensitivity, Specificity and TSS to
each of the algorithms used.

## See also

[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`algorithms`](https://luizesser.github.io/caretSDM/reference/algorithms.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
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
i <- pseudoabsences(i, method = "random")

# Custom trainControl:
ctrl_sdm <- caret::trainControl(method = "repeatedcv",
                                number = 2,
                                repeats = 1,
                                classProbs = TRUE,
                                returnResamp = "all",
                                summaryFunction = summary_sdm,
                                savePredictions = "all")

# Train models:
i <- train_sdm(i, algo = c("naive_bayes"), ctrl = ctrl_sdm) |>
suppressWarnings()
```
