# Predictors as PCA-axes

Transform predictors data into PCA-axes.

## Usage

``` r
pca_predictors(i, cumulative_proportion = 0.99)

pca_summary(i)

get_pca_model(i)
```

## Arguments

- i:

  A `input_sdm` object.

- cumulative_proportion:

  A `numeric` with the threshold for cumulative proportion. Standard is
  0.99, meaning that axes returned as predictors sum up more than 99
  variance.

## Value

`input_sdm` object with variables from both `predictors` and `scenarios`
transformed in PCA-axes.

## Details

`pca_predictors` Transform predictors data into PCA-axes. If the user
wants to use PCA-axes as future scenarios, then scenarios should be
added after the PCA transformation (see examples). `pca_summary` Returns
the summary of `prcomp` function. See ?stats::prcomp. `get_pca_model`
Returns the model built to calculate PCA-axes.

## See also

[`vif_predictors`](https://luizesser.github.io/caretSDM/reference/vif_predictors.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`add_scenarios`](https://luizesser.github.io/caretSDM/reference/add_scenarios.md)` `[`add_predictors`](https://luizesser.github.io/caretSDM/reference/add_predictors.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
sa <- sdm_area(parana, cell_size = 50000, crs = 6933)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include predictors:
sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)

# Create input_sdm:
i <- input_sdm(oc, sa)

# PCA transformation:
i <- pca_predictors(i)
```
