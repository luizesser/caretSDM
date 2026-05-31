# Find species, longitude and latitudecolumns in a data.frame.

Find species, longitude and latitudecolumns in a data.frame.

## Usage

``` r
.find_columns(df, col_names = NULL, spp = TRUE)
```

## Arguments

- df:

  data.frame to search for.

- col_names:

  Names of the columns so it doesn't need to look for.

- spp:

  Boolean. Search for species column?

## Value

Vector of column names that correspond to the species, longitude and
latitude columns.
