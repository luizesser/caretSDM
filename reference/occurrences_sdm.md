# Occurrences Managing

This function creates and manage `occurrences` objects.

## Usage

``` r
occurrences_sdm(x,
                independent_test = NULL,
                p = 0.1,
                crs = NULL,
                independent_test_crs = NULL,
                ...)

n_records(i)

species_names(i)

get_coords(i)

occurrences_as_df(i)

add_occurrences(oc1, oc2)
```

## Arguments

- x:

  A `data.frame`, `tibble` or `sf` with species records.

- independent_test:

  Boolean. If `independet_test` is `TRUE`, a fraction of the data is
  kept for independent testing. Otherwise, the whole dataset `x` is
  used. It can also be a `data.frame` or a `sf`, with species records to
  be used as independent test. Structure and names should be identical
  to those in `x`.

- p:

  Numeric. Fraction of data to be used as independent test. Standard is
  0.1.

- crs:

  Numeric. CRS of `x`.

- independent_test_crs:

  Numeric. CRS of `independent_test` if it is a `data.frame`.

- ...:

  A vector with column names addressing the columns with species names,
  longitude and latitude, respectively, in `x`.

- i:

  `input_sdm` or `occurrences` object.

- oc1:

  A `occurrences` object to be summed with.

- oc2:

  A `occurrences` object to be summed with.

## Value

A `occurrences` object.

## Details

`x` must have three columns: species, decimalLongitude and
decimalLatitude. When `sf` it is only necessary a species column.
`n_records` return the number of presence records to each species.
`species_names` return the species names. `get_coords` return a
`data.frame` with coordinates of species records. `add_occurrences`
return a `occurrences`. This function sums two `occurrences` objects. It
can also sum a `occurrences` object with a `data.frame` object.
`occurrences_as_df` returns a `data.frame` with species names and
coordinates.

## See also

[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)` `[`GBIF_data`](https://luizesser.github.io/caretSDM/reference/GBIF_data.md)` `[`occ`](https://luizesser.github.io/caretSDM/reference/occ.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933)
```
