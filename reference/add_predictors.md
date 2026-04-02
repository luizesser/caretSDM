# Add predictors to `sdm_area`

This function includes new predictors to the `sdm_area` object.

## Usage

``` r
add_predictors(sa, pred, variables_selected = NULL, gdal = TRUE,
                      lines_as_sdm_area = FALSE)

get_predictors(i)
```

## Arguments

- sa:

  A `sdm_area` object.

- pred:

  `RasterStack`, `SpatRaster`, `stars` or `sf` object with predictors
  data.

- variables_selected:

  `character` vector with variables names in `pred` to be used as
  predictors. If `NULL` adds all variables.

- gdal:

  Boolean. Force the use or not of GDAL when available. See details.

- lines_as_sdm_area:

  Boolean. If `x` is a `sf` with LINESTRING geometry, it can be used to
  model species distribution in lines and not grid cells.

- i:

  `input_sdm` or `sdm_area` object to retrieve data from.

## Value

For `add_predictors` the same input `sdm_area` object is returned
including the `pred` data binded to the previous `grid`.
`get_predictors` retrieves the grid from the `i` object.

## Details

`add_predictors` returns a `sdm_area` object with a grid built upon the
`x` parameter. There are two ways to make the grid and resample the
variables in `sdm_area`: with and without gdal. As standard, if gdal is
available in you machine it will be used (`gdal = TRUE`), otherwise
sf/stars will be used. `lines_as_sdm_area` and `gdal` parameters are
passed to `sdm_area` function, so they will be used in the grid creation
and resampling of predictors. They will be retrieved automatically from
the `sdm_area` object.

## See also

[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`get_predictor_names`](https://luizesser.github.io/caretSDM/reference/predictor_names.md)` `[`bioc`](https://luizesser.github.io/caretSDM/reference/bioc.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com) and Reginaldo Ré.
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include predictors:
sa <- add_predictors(sa, bioc)
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Retrieve predictors data:
get_predictors(sa)
#> Simple feature collection with 373 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5276744 ymin: -3295037 xmax: -4626744 ymax: -2795037
#> Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
#> First 10 features:
#>    cell_id GID0 CODIGOIB1 NOMEUF2 SIGLAUF3     bio1 bio4    bio12
#> 1        6   19        41       0        0 22.39812 1268 262.4270
#> 2        7   19        41       0        0 22.39812 1268 262.4270
#> 3        8   19        41       0        0 22.24774 1243 259.8303
#> 4        9   19        41       0        0 22.13017 1230 260.5331
#> 5       10   19        41       0        0 22.08299 1203 260.9101
#> 6       11   19        41       0        0 22.06016 1192 259.2609
#> 7       12   19        41       0        0 22.24014 1234 258.5582
#> 8       13   19        41       0        0 22.28710 1236 257.0163
#> 9       14   19        41       0        0 22.29775 1247 258.6638
#> 10      31   19        41       0        0 22.15600 1317 267.6054
#>                          geometry
#> 1  POLYGON ((-5151744 -2795037...
#> 2  POLYGON ((-5126744 -2795037...
#> 3  POLYGON ((-5101744 -2795037...
#> 4  POLYGON ((-5076744 -2795037...
#> 5  POLYGON ((-5051744 -2795037...
#> 6  POLYGON ((-5026744 -2795037...
#> 7  POLYGON ((-5001744 -2795037...
#> 8  POLYGON ((-4976744 -2795037...
#> 9  POLYGON ((-4951744 -2795037...
#> 10 POLYGON ((-5176744 -2820037...
```
