# `input_sdm`

This function creates a new `input_sdm` object.

## Usage

``` r
input_sdm(...)

add_input_sdm(i1, i2)
```

## Arguments

- ...:

  Data to be used in SDMs. Can be a `occurrences` and/or a `sdm_area`
  object.

- i1:

  A `input_sdm` object.

- i2:

  A `input_sdm` object.

## Value

A `input_sdm` object containing:

- grid:

  `sf` with POLYGON geometry representing the grid for the study area or
  LINESTRING if `sdm_area` was built with a LINESTRING `sf`.

- bbox:

  Four corners for the bounding box (class `bbox`): minimum value of X,
  minimum value of Y, maximum value of X, maximum value of Y

- cell_size:

  `numeric` information regarding the size of the cell used to rescale
  variables to the study area, representing also the cell size in the
  `grid`.

- epsg:

  `character` information about the EPSG used in all slots from
  `sdm_area`.

- predictors:

  `character` vector with predictors names included in `sdm_area`.

## Details

If `sdm_area` is used, it can include predictors and scenarios. In this
case, `input_sdm` will detect and include as `scenarios` and
`predictors` in the `input_sdm` output. Objects can be included in any
order, since the function will work by detecting their classes. The
returned object is used throughout the whole workflow to apply
functions.

## See also

[`occurrences_sdm`](https://luizesser.github.io/caretSDM/reference/occurrences_sdm.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)

## Author

Luiz Fernando Esser (luizesser@gmail.com)
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

# Create input_sdm:
i <- input_sdm(oc, sa)
```
