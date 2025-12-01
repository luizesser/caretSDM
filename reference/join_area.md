# Join Area

Join cell_id data from sdm_area to a occurrences

## Usage

``` r
join_area(occ, pred)
```

## Arguments

- occ:

  A `occurrences` object or `input_sdm`.

- pred:

  A `sdm_area` object to retrieve cell_id from.

## Value

A `occurrences` object with `cell_id` to each record.

## Details

This function is key in this SDM workflow. It attaches cell_id values to
`occ`, deletes records outside `pred` and allows the use of
pseudoabsences. This function also tests if CRS from both `occ` and
`pred` are equal, otherwise the CRS of `pred` is used to convert `occ`.

## See also

[`occurrences_sdm`](https://luizesser.github.io/caretSDM/reference/occurrences_sdm.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)` `[`pseudoabsences`](https://luizesser.github.io/caretSDM/reference/pseudoabsences.md)

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
sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio4", "bio12"))
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include scenarios:
sa <- add_scenarios(sa, scen)
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
```
