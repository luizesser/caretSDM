# Ensemble of Small Models (ESM) in caretSDM

This functions set parameters to run a ESM when running `train_sdm`.

## Usage

``` r
use_esm(x, spp = NULL, n_records = 20)
```

## Arguments

- x:

  A `occurrences` or `input_sdm` object containing occurrences.

- spp:

  A vector of species names containing the species which the ESM must be
  applied. Standard is NULL.

- n_records:

  Numeric. Number of species records to apply the ESM. Standard is 20.

## Value

A `input_sdm` or `occurrences` object with ESM parameters.

## Details

We supply two different ways to apply the ESM. If species names are
provided, then ESM will be applied only in given species. If a number of
species records is provided, then ESM will be applied in every species
with number of records bellow the given threshold. As standard,
`use_esm` will be apply to every species with less then 20 records.

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
sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio4", "bio12"))
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include scenarios:
sa <- add_scenarios(sa)

# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)

# Create input_sdm:
i <- input_sdm(oc, sa)

# Use MEM:
i <- use_esm(i, n_records = 999)
```
