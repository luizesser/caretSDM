# Create a `sdm_area` object

This function creates a new `sdm_area` object.

## Usage

``` r
sdm_area(x, cell_size = NULL, crs = NULL, variables_selected = NULL,
                gdal = TRUE, crop_by = NULL, lines_as_sdm_area = FALSE)

get_sdm_area(i)

add_sdm_area(sa1, sa2)
```

## Arguments

- x:

  A shape or a raster. Usually a shape from `sf` class, but rasters from
  `stars`, `rasterStack` or `SpatRaster` class are also allowed.

- cell_size:

  `numeric`. The cell size to be used in models.

- crs:

  `numeric`. Indicates which EPSG should the output grid be in. If
  `NULL`, epsg from `x` is used.

- variables_selected:

  A `character` vector with variables in `x` to be used in models. If
  `NULL` (standard), all variables in `x` are used.

- gdal:

  Boolean. Force the use or not of GDAL when available. See details.

- crop_by:

  A shape from `sf` to crop `x`.

- lines_as_sdm_area:

  Boolean. If `x` is a `sf` with LINESTRING geometry, it can be used to
  model species distribution in lines and not grid cells.

- i:

  A `sdm_area` or a `input_sdm` object.

- sa1:

  A `sdm_area` object.

- sa2:

  A `sdm_area` object.

## Value

A `sdm_area` object containing:

- grid:

  `sf` with `POLYGON` geometry representing the grid for the study area.

- cell_size:

  `numeric` information regarding the size of the cell used to rescale
  variables to the study area, representing also the cell size in the
  `grid`.

## Details

The function returns a `sdm_area` object with a grid built upon the `x`
parameter. There are two ways to make the grid and resample the
variables in `sdm_area`: with and without gdal. As standard, if gdal is
available in you machine it will be used (`gdal = TRUE`), otherwise
sf/stars will be used. `get_sdm_area` will return the grid built by
`sdm_area`. `add_sdm_area` will sum two `sdm_area` objects. As
geoprocessing in `caretSDM` is performed using `sf` objects,
`add_sdm_area` simply applies a `rbind` in the two different areas.

## See also

[`WorldClim_data`](https://luizesser.github.io/caretSDM/reference/WorldClim_data.md)` `[`parana`](https://luizesser.github.io/caretSDM/reference/parana.md)` `[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)`, `[`add_predictors`](https://luizesser.github.io/caretSDM/reference/add_predictors.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com) and Reginaldo Ré.
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
sa_area <- sdm_area(parana, cell_size = 50000, crs = 6933)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Create sdm_area using a subset of rivs (lines):
sa_rivers <- sdm_area(rivs[c(1:100),], cell_size = 100000, crs = 6933, lines_as_sdm_area = TRUE)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
```
