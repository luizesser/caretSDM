# Predictors Names Managing

This function manage predictors names in `sdm_area` objects.

## Usage

``` r
predictors(x)

# S3 method for class 'sdm_area'
predictors(x)

# S3 method for class 'input_sdm'
predictors(x)

set_predictor_names(x, new_names)

# S3 method for class 'input_sdm'
set_predictor_names(x, new_names)

# S3 method for class 'sdm_area'
set_predictor_names(x, new_names)

get_predictor_names(x)

# S3 method for class 'sdm_area'
get_predictor_names(x)

# S3 method for class 'input_sdm'
get_predictor_names(x)

test_variables_names(sa, scen)

set_variables_names(s1 = NULL, s2 = NULL, new_names = NULL)
```

## Arguments

- x:

  A `sdm_area` or `input_sdm` object to get/set predictors names.

- new_names:

  A `character` vector from size `length(get_predictor_names(x))`

- sa:

  A `sdm_area` object.

- scen:

  A `stars` object with scenarios.

- s1:

  A `stars` object with scenarios.

- s2:

  A `stars` object with scenarios or a `sdm_area` object.

## Value

`predictors` and `get_predictor_names` return a `character` vector with
predictors names. `test_variables_names` returns a logical informing if
all variables are equal in both objects (TRUE) or not (FALSE).
`set_variables_names` returns the `s1` object with new names provided by
`s2` or `new_names`.

## Details

This functions is available so users can modify predictors names to
better represent them. Use carefully to avoid giving wrong names to the
predictors. Useful to make sure the predictors names are equal the names
in scenarios. `test_variables_names` Tests if variables in a `stars`
object (`scen` argument) matches the given `sdm_area` object (`sa`
argument). `set_variables_names` will set `s1` object variables names as
the `s2` object variables names OR assign new names to it.

## See also

[`parana`](https://luizesser.github.io/caretSDM/reference/parana.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)

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
sa <- add_predictors(sa, bioc)
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Check predictors' names:
get_predictor_names(sa)
#> [1] "GID0"      "CODIGOIB1" "NOMEUF2"   "SIGLAUF3"  "bio1"      "bio4"     
#> [7] "bio12"    
```
