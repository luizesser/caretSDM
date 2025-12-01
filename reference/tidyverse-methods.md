# Tidyverse methods for caretSDM objects

Set of functions to facilitate the use of caretSDM through tidyverse
grammatics.

## Usage

``` r
select_predictors(x, ...)

# S3 method for class 'sdm_area'
select(.data, ...)

# S3 method for class 'input_sdm'
select(.data, ...)

# S3 method for class 'sdm_area'
mutate(.data, ...)

# S3 method for class 'input_sdm'
mutate(.data, ...)

# S3 method for class 'sdm_area'
filter(.data, ..., .by, .preserve)

# S3 method for class 'input_sdm'
filter(.data, ..., .by, .preserve)

# S3 method for class 'occurrences'
filter(.data, ..., .by, .preserve)

filter_species(x, spp = NULL, ...)
```

## Arguments

- x:

  `sdm_area` or `input_sdm` object.

- ...:

  `character` arguments to pass to the given function.

- .data:

  Data to pass to tidyr function.

- .by:

  See ?dplyr::filter.

- .preserve:

  See ?dplyr::filter.

- spp:

  Species to be filtered.

## Value

The transformed `sdm_area`/`input_sdm` object.

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
```
