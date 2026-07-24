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
sa <- sdm_area(rivs[c(1:200), ], cell_size = 100000, output_crs = 6933, lines_as_sdm_area = TRUE)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

# Include predictors:
sa <- add_predictors(sa, bioc)
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include scenarios:
sa_bioc <- sa |>
  select_predictors(c("bio1", "bio12")) |>
  add_scenarios(scen[1:2])
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# OR to include sationary variables:
sa <- add_scenarios(sa, scen[1:2], stationary = c("LENGTH_KM", "DIST_DN_KM"))
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Set scenarios names:
sa <- set_scenarios_names(sa, scenarios_names = c(
  "future_1", "future_2",
  "current"
))
scenarios_names(sa)
#> [1] "future_1" "future_2" "current" 

# Get scenarios data:
scenarios_grid <- get_scenarios_data(sa)
scenarios_grid
#> $future_1
#> Simple feature collection with 208 features and 6 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -5177092 ymin: -2875359 xmax: -4771046 ymax: -2803134
#> Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
#> First 10 features:
#>    cell_id     bio1     bio4    bio12 LENGTH_KM DIST_DN_KM
#> 1        1 27.20938 273.4452 1149.013      7.31     2189.9
#> 2        2 27.20938 273.4452 1149.013      4.21     2186.2
#> 3        3 27.20938 273.4452 1149.013      2.14     2200.3
#> 4        4 27.20938 273.4452 1149.013      3.45     2186.7
#> 5        5 27.20938 273.4452 1149.013      1.26     2184.9
#> 7        6 27.20938 273.4452 1149.013      2.12     2202.7
#> 8        7 27.20938 273.4452 1149.013      2.54     2183.9
#> 9        8 27.20938 273.4452 1149.013      4.89     2195.6
#> 11       9 27.20938 273.4452 1149.013      2.12     2204.8
#> 12      10 27.20938 273.4452 1149.013      4.66     2179.5
#>                          geometry
#> 1  LINESTRING (-5164171 -28031...
#> 2  LINESTRING (-5118891 -28038...
#> 3  LINESTRING (-5106335 -28066...
#> 4  LINESTRING (-5166639 -28041...
#> 5  LINESTRING (-5121210 -28061...
#> 7  LINESTRING (-5104727 -28076...
#> 8  LINESTRING (-5167443 -28071...
#> 9  LINESTRING (-5108346 -28066...
#> 11 LINESTRING (-5103119 -28086...
#> 12 LINESTRING (-5165433 -28081...
#> 
#> $future_2
#> Simple feature collection with 208 features and 6 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -5177092 ymin: -2875359 xmax: -4771046 ymax: -2803134
#> Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
#> First 10 features:
#>    cell_id     bio1     bio4    bio12 LENGTH_KM DIST_DN_KM
#> 1        1 32.57484 272.2632 1015.714      7.31     2189.9
#> 2        2 32.57484 272.2632 1015.714      4.21     2186.2
#> 3        3 32.57484 272.2632 1015.714      2.14     2200.3
#> 4        4 32.57484 272.2632 1015.714      3.45     2186.7
#> 5        5 32.57484 272.2632 1015.714      1.26     2184.9
#> 7        6 32.57484 272.2632 1015.714      2.12     2202.7
#> 8        7 32.57484 272.2632 1015.714      2.54     2183.9
#> 9        8 32.57484 272.2632 1015.714      4.89     2195.6
#> 11       9 32.57484 272.2632 1015.714      2.12     2204.8
#> 12      10 32.57484 272.2632 1015.714      4.66     2179.5
#>                          geometry
#> 1  LINESTRING (-5164171 -28031...
#> 2  LINESTRING (-5118891 -28038...
#> 3  LINESTRING (-5106335 -28066...
#> 4  LINESTRING (-5166639 -28041...
#> 5  LINESTRING (-5121210 -28061...
#> 7  LINESTRING (-5104727 -28076...
#> 8  LINESTRING (-5167443 -28071...
#> 9  LINESTRING (-5108346 -28066...
#> 11 LINESTRING (-5103119 -28086...
#> 12 LINESTRING (-5165433 -28081...
#> 
#> $current
#> Simple feature collection with 208 features and 6 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -5177092 ymin: -2875359 xmax: -4771046 ymax: -2803134
#> Projected CRS: WGS 84 / NSIDC EASE-Grid 2.0 Global
#> First 10 features:
#>    cell_id LENGTH_KM DIST_DN_KM     bio1     bio4 bio12
#> 1        1      7.31     2189.9 21.95178 271.4204  1343
#> 2        2      4.21     2186.2 21.95178 271.4204  1343
#> 3        3      2.14     2200.3 21.95178 271.4204  1343
#> 4        4      3.45     2186.7 21.95178 271.4204  1343
#> 5        5      1.26     2184.9 21.95178 271.4204  1343
#> 7        6      2.12     2202.7 21.95178 271.4204  1343
#> 8        7      2.54     2183.9 21.95178 271.4204  1343
#> 9        8      4.89     2195.6 21.95178 271.4204  1343
#> 11       9      2.12     2204.8 21.95178 271.4204  1343
#> 12      10      4.66     2179.5 21.95178 271.4204  1343
#>                          geometry
#> 1  LINESTRING (-5164171 -28031...
#> 2  LINESTRING (-5118891 -28038...
#> 3  LINESTRING (-5106335 -28066...
#> 4  LINESTRING (-5166639 -28041...
#> 5  LINESTRING (-5121210 -28061...
#> 7  LINESTRING (-5104727 -28076...
#> 8  LINESTRING (-5167443 -28071...
#> 9  LINESTRING (-5108346 -28066...
#> 11 LINESTRING (-5103119 -28086...
#> 12 LINESTRING (-5165433 -28081...
#> 

# Select scenarios:
sa <- select_scenarios(sa, scenarios_names = c("future_1"))
```
