# Add scenarios to `sdm_area`

This function includes scenarios in the `sdm_area` object.

## Usage

``` r
add_scenarios(sa, scen = NULL, scenarios_names = NULL, pred_as_scen = TRUE,
                     variables_selected = NULL, stationary = NULL, crop_area = NULL)

set_scenarios_names(i, scenarios_names = NULL)

scenarios_names(i)

get_scenarios_data(i)

select_scenarios(i, scenarios_names = NULL)
```

## Arguments

- sa:

  A `sdm_area` or `input_sdm` object.

- scen:

  `RasterStack`, `SpatRaster` or `stars` object. If `NULL` adds
  predictors as a scenario.

- scenarios_names:

  Character vector with names of scenarios.

- pred_as_scen:

  Logical. If `TRUE` adds the current predictors as a scenario.

- variables_selected:

  Character vector with variables names in `scen` to be used as
  variables. If `NULL` adds all variables.

- stationary:

  Names of variables from `sa` that should be used in scenarios as
  stationary variables.

- crop_area:

  A `sf` object to crop the `scen` object if necessary.

- i:

  A `sdm_area` or `input_sdm` object.

## Value

`add_scenarios` returns the input `sdm_area` or `input_sdm` object with
a new slot called scenarios with `scen` data as a `list`, where each
slot of the `list` holds a scenario and each scenario is a `sf` object.
`set_scenarios_names` sets new names for scenarios in
`sdm_area`/`input_sdm` object. `scenarios_names` returns scenarios'
names. `get_scenarios_data` retrieves scenarios data as a `list` of `sf`
objects. `select_scenarios` selects scenarios from
`sdm_area`/`input_sdm` object.

## Details

The function `add_scenarios` adds scenarios to the `sdm_area` or
`input_sdm` object. If `scen` has variables that are not present as
predictors the function will use only variables present in both objects.
`stationary` variables are those that don't change through the
scenarios. It is useful for hidrological variables in fish habitat
modeling, for example (see examples below). When adding multiple
scenarios in multiple runs, the function will always add a new "current"
scenario. To avoid that, set `pred_as_scen = FALSE`.

## See also

[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)

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
sa <- add_predictors(sa, bioc)
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include scenarios:
sa <- add_scenarios(sa, scen[1:2]) |> select_predictors(c("bio1", "bio12"))
#> Warning: Some variables in `variables_selected` are not present in `scen`.
#> ℹ Using only variables present in `scen`: bio1, bio4, and bio12
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Set scenarios names:
sa <- set_scenarios_names(sa, scenarios_names = c("future_1", "future_2",
                                                  "current"))
scenarios_names(sa)
#> [1] "future_1" "future_2" "current" 

# Get scenarios data:
scenarios_grid <- get_scenarios_data(sa)
scenarios_grid
#> $future_1
#> Simple feature collection with 25 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5371070 ymin: -3380515 xmax: -4571070 ymax: -2680515
#> Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
#> First 10 features:
#>    cell_id     bio1    bio12                       geometry
#> 1       11 27.23990 267.5603 POLYGON ((-5171070 -2780515...
#> 2       12 26.67789 268.4286 POLYGON ((-5071070 -2780515...
#> 3       13 26.77645 270.4923 POLYGON ((-4971070 -2780515...
#> 4       14 25.75506 279.7340 POLYGON ((-4871070 -2780515...
#> 5       18 26.64249 303.4011 POLYGON ((-5271070 -2880515...
#> 6       19 25.78418 288.3141 POLYGON ((-5171070 -2880515...
#> 7       20 24.56363 286.9512 POLYGON ((-5071070 -2880515...
#> 8       21 24.13556 269.6907 POLYGON ((-4971070 -2880515...
#> 9       22 24.81822 280.1432 POLYGON ((-4871070 -2880515...
#> 10      26 25.63413 325.8536 POLYGON ((-5271070 -2980515...
#> 
#> $future_2
#> Simple feature collection with 25 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5371070 ymin: -3380515 xmax: -4571070 ymax: -2680515
#> Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
#> First 10 features:
#>    cell_id     bio1    bio12                       geometry
#> 1       11 32.58110 267.3770 POLYGON ((-5171070 -2780515...
#> 2       12 31.94988 268.9733 POLYGON ((-5071070 -2780515...
#> 3       13 31.98031 270.1299 POLYGON ((-4971070 -2780515...
#> 4       14 30.81193 277.3997 POLYGON ((-4871070 -2780515...
#> 5       18 32.04770 301.0514 POLYGON ((-5271070 -2880515...
#> 6       19 31.13231 286.2296 POLYGON ((-5171070 -2880515...
#> 7       20 29.82842 283.8177 POLYGON ((-5071070 -2880515...
#> 8       21 29.31869 268.7257 POLYGON ((-4971070 -2880515...
#> 9       22 29.82839 279.3565 POLYGON ((-4871070 -2880515...
#> 10      26 30.82495 322.6778 POLYGON ((-5271070 -2980515...
#> 
#> $current
#> Simple feature collection with 31 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5301744 ymin: -3295037 xmax: -4601744 ymax: -2795037
#> Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
#> First 10 features:
#>    cell_id     bio1    bio12                       geometry
#> 1        2 21.98257 270.4756 POLYGON ((-5201744 -2795037...
#> 2        3 21.98257 270.4756 POLYGON ((-5101744 -2795037...
#> 3        4 21.23959 267.7715 POLYGON ((-5001744 -2795037...
#> 4        5 21.46965 264.9304 POLYGON ((-4901744 -2795037...
#> 5        6 21.07145 275.6752 POLYGON ((-4801744 -2795037...
#> 6        8 21.98257 270.4756 POLYGON ((-5301744 -2895037...
#> 7        9 21.98257 270.4756 POLYGON ((-5201744 -2895037...
#> 8       10 21.98257 270.4756 POLYGON ((-5101744 -2895037...
#> 9       11 21.23959 267.7715 POLYGON ((-5001744 -2895037...
#> 10      12 21.46965 264.9304 POLYGON ((-4901744 -2895037...
#> 

# Select scenarios:
sa <- select_scenarios(sa, scenarios_names = c("future_1"))

# Setting stationary variables in scenarios:
sa <- sdm_area(rivs[c(1:200),], cell_size = 100000, crs = 6933, lines_as_sdm_area = TRUE) |>
  add_predictors(bioc) |>
  add_scenarios(scen, stationary = c("LENGTH_KM", "DIST_DN_KM"))
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

```
