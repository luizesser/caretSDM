# Create buffer around occurrences

Create buffer around records in `occ_data` to be used as study area

## Usage

``` r
buffer_sdm(occ_data, size = NULL, crs = NULL)
```

## Arguments

- occ_data:

  A `data.frame` object with species, decimalLongitude and
  decimalLatitude columns. Usually the output from `GBIF_data`.

- size:

  `numeric`. The distance between the record and the margin of the
  buffer (i.e. buffer radius).

- crs:

  `numeric`. Indicates which EPSG it the `occ_data` in.

## Value

A `sf` buffer around `occ_data` records.

## See also

[`GBIF_data`](https://luizesser.github.io/caretSDM/reference/GBIF_data.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
study_area <- buffer_sdm(occ, size=50000, crs=6933)
plot(study_area)

```
