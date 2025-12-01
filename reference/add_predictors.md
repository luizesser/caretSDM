# Add predictors to `sdm_area`

This function includes new predictors to the `sdm_area` object.

## Usage

``` r
add_predictors(sa, pred, variables_selected = NULL, gdal = TRUE)

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
sf/stars will be used.

## See also

[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`predictors`](https://luizesser.github.io/caretSDM/reference/predictor_names.md)` `[`bioc`](https://luizesser.github.io/caretSDM/reference/bioc.md)

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
#> 1        6   19        41       0        0 22.45938 1285 261.4752
#> 2        7   19        41       0        0 22.45938 1285 261.4752
#> 3        8   19        41       0        0 22.39415 1243 258.4977
#> 4        9   19        41       0        0 22.25541 1220 258.4987
#> 5       10   19        41       0        0 22.19232 1203 259.3725
#> 6       11   19        41       0        0 22.12438 1189 257.6924
#> 7       12   19        41       0        0 22.38992 1192 257.7096
#> 8       13   19        41       0        0 22.40336 1234 254.7985
#> 9       14   19        41       0        0 22.39525 1247 255.8951
#> 10      31   19        41       0        0 22.45938 1285 261.4752
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
