# `is_class` functions to check caretSDM data classes.

This functions returns a boolean to check caretSDM object classes.

## Usage

``` r
is_input_sdm(x)

is_sdm_area(x)

is_occurrences(x)

is_models(x)

is_predictions(x)
```

## Arguments

- x:

  Object to be tested.

## Value

Boolean.

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
sa <- sdm_area(parana, cell_size = 25000, crs = 6933)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

is_sdm_area(sa)
#> [1] TRUE

is_input_sdm(sa)
#> [1] FALSE
```
