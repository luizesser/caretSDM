# Correlation between projections

This function aims to unveil the correlation of different algorithms
outputs. For that, it uses the predictions on current scenario, but
other scenarios can be tested.

## Usage

``` r
correlate_sdm(i, scenario = "current")
```

## Arguments

- i:

  A `input_sdm` object containing predictions.

- scenario:

  A `character` containing scenario to be tested. Standard is
  `"current"`. Value must match `scenarios_names(i)`.

## Value

A `data.frame` with pearson correlation between projections.

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
#> Loading required package: ggplot2
#> Loading required package: lattice
#> 
#> Attaching package: ‘caret’
#> The following object is masked from ‘package:caretSDM’:
#> 
#>     predictors

# Predict models:
i  <- predict_sdm(i, th = 0.8)
#> [1] "Projecting: 1/1"
#> [1] "Ensembling..."
#> [1] "current"
#> [1] "Araucaria angustifolia"

# Check correlations:
correlate_sdm(i)
#> $`Araucaria angustifolia`
#>           m1.1      m2.1
#> m1.1 1.0000000 0.5364783
#> m2.1 0.5364783 1.0000000
#> 
```
