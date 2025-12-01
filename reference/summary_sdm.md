# Calculates performance across resamples

This function is used in
`caret::trainControl(summaryFunction=summary_sdm)` to calculate
performance metrics across resamples.

## Usage

``` r
summary_sdm(data, lev = NULL, model = NULL, custom_fun=NULL)

summary_sdm_presence_only(data, lev, threshold)

validate_on_independent_data(model, data_independent, obs_col_name)
```

## Arguments

- data:

  A `data.frame` with observed and predicted values.

- lev:

  A `character` vector of factors levels for the response.

- model:

  Models names taken from `train` object.

- custom_fun:

  A custom function to be applied in models (not yet implemented).

- threshold:

  Threshold for presence-only models.

- data_independent:

  independent data.frame to calculate metrics.

- obs_col_name:

  The name of the column with observed values.

## Value

A `input_sdm` or a `predictions` object.

## Details

See
[`?caret::defaultSummary`](https://rdrr.io/pkg/caret/man/postResample.html)
for more details and options to pass on
[`caret::trainControl`](https://rdrr.io/pkg/caret/man/trainControl.html).

## See also

[`train_sdm`](https://luizesser.github.io/caretSDM/reference/train_sdm.md)

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
i <- train_sdm(i, algo = c("naive_bayes"), ctrl=ctrl_sdm) |>
suppressWarnings()
```
