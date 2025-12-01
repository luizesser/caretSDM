# Retrieve Species data from GBIF

This function is a wrapper to get records from GBIF using `rgbif` and
return a `data.frame` ready to be used in caretSDM.

## Usage

``` r
GBIF_data(s, file = NULL, as_df = FALSE, ...)
```

## Arguments

- s:

  `character` vector of species names.

- file:

  `character` with file to save the output. If not informed, data will
  not be saved on folder.

- as_df:

  Should the output be a `dataframe`? Default is `FALSE`, returning a
  `occurrences` object.

- ...:

  Arguments to pass on
  [`rgbif::occ_data()`](https://docs.ropensci.org/rgbif/reference/occ_data.html).

## Value

A `data.frame` with species occurrences data, or an `occurrences` object
if `as_df = FALSE`.

## References

https://www.gbif.org

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# \donttest{
# Select species names:
s <- c("Araucaria angustifolia", "Salminus brasiliensis")

# Run function:
oc <- GBIF_data(s)
# }
```
