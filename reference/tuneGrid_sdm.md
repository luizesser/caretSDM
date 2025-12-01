# Retrieve tuneGrid from models

This function aims to retrieve the tune grid used to build models.

## Usage

``` r
tuneGrid_sdm(i)
```

## Arguments

- i:

  A `input_sdm` object containing models.

## Value

A `list` with `data.frames` each one representing the table of a given
model.

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

# Retrieve tuneGrid from model:
tuneGrid_sdm(i)
#> $`Araucaria angustifolia`
#> $`Araucaria angustifolia`$m1.1
#>   laplace usekernel adjust
#> 1       0     FALSE      1
#> 2       0      TRUE      1
#> 
#> $`Araucaria angustifolia`$m2.1
#>   laplace usekernel adjust
#> 1       0     FALSE      1
#> 2       0      TRUE      1
#> 
#> 
```
