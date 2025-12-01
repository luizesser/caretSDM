# Calculation of variable importance for models

This function retrieves variable importance as a function of ROC curves
to each predictor.

## Usage

``` r
varImp_sdm(m, id = NULL, ...)
```

## Arguments

- m:

  A `models` or `input_sdm` object.

- id:

  Vector of model ids to filter varImp calculation.

- ...:

  Parameters passing to caret::varImp().

## Value

A `data.frame` with variable importance data.

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

# Variable importance:
varImp_sdm(i)
#> $`Araucaria angustifolia`
#>       mean sd
#> bio1   100  0
#> bio12    0  0
#> 
```
