# MacroEcological Models (MEM) in caretSDM

This function sums all species records into one. Should be used before
the data cleaning routine.

## Usage

``` r
use_mem(x, add = TRUE, name = "MEM")
```

## Arguments

- x:

  A `occurrences` or `input_sdm` object containing occurrences.

- add:

  Logical. Should the new MEM records be added to the pool (`TRUE`) of
  species or the output should have only the summed records (`FALSE`)?
  Standard is `TRUE`.

- name:

  How should the new records be named? Standard is "MEM".

## Value

A `input_sdm` or `occurrences` object with MEM data.

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
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
#> Warning: Some records from `occ` do not fall in `pred`.
#> ℹ 2 elements from `occ` were excluded.
#> ℹ If this seems too much, check how `occ` and `pred` intersect.

# Create input_sdm:
i <- input_sdm(oc, sa)

# Use MEM:
i <- use_mem(i)
```
