# Obtain Background data

This function obtains background data given a set of predictors.

## Usage

``` r
background(occ,
           pred = NULL,
           n = 10000,
           n_set = 1,
           proportion = NULL)

n_background(i)

background_data(i)
```

## Arguments

- occ:

  A `occurrences_sdm` or `input_sdm` object.

- pred:

  A `sdm_area` object. If `NULL` and `occ` is a `input_sdm`, `pred` will
  be retrieved from `occ`.

- n:

  `numeric`. Number of background records to be generated in each
  dataset created. If `NULL` then the function prevents imbalance by
  using the same number of presence records (`n_records(occ)`). If you
  want to address different sizes to each species, you must provide a
  named vector (as in `n_records(occ)`).

- n_set:

  `numeric`. Number of datasets of background data to create.

- proportion:

  `numeric`. A number between 0 and 1 representing a proportion of the
  area to be mapped as background. E.g.: if the whole area has 5,000
  cells and proportion is 0.1, then `n` is set to 500. Standard is NULL.
  This argument overwrites `n`.

- i:

  A `input_sdm` object.

## Value

A `occurrences_sdm` or `input_sdm` object with background data.

## Details

`background` is used in the SDM workflow to obtain background data, a
step necessary for MaxEnt algorithm to run. This function helps avoid
the use of pseudoabsence data in background algorithms and the use of
background data in pseudoabsence algorithms, a very common mistake.

`n_background` returns the number of background records obtained per
species.

`background_data` returns a `list` of species names. Each species name
will have a `list`s with background data from class `sf`.

## See also

`link{input_sdm} `[`pseudoabsences`](https://luizesser.github.io/caretSDM/reference/pseudoabsences.md)` `[`occurrences_sdm`](https://luizesser.github.io/caretSDM/reference/occurrences_sdm.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

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

# Include scenarios:
sa <- add_scenarios(sa)

# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#> Warning: Some records from `occ` do not fall in `pred`.
#> ℹ 2 elements from `occ` were excluded.
#> ℹ If this seems too much, check how `occ` and `pred` intersect.

# Create input_sdm:
i <- input_sdm(oc, sa)

# Pseudoabsence generation:
i <- background(i, proportion = 1) # All available data is obtained as background data.
#> → Proportion is 1 Setting all species to have 373 background records.
#> → Background number is higher than the total data available.
#> Setting the number of background data to be 373.
```
