# Presence data cleaning routine

Data cleaning wrapper using CoordinateCleaner package.

## Usage

``` r
data_clean(occ, pred = NULL,
           species = NA, lon = NA, lat = NA,
           capitals = TRUE,
           centroids = TRUE,
           duplicated = TRUE,
           identical = TRUE,
           institutions = TRUE,
           invalid = TRUE,
           terrestrial = TRUE,
           independent_test = TRUE)
```

## Arguments

- occ:

  A `occurrences_sdm` object or `input_sdm`.

- pred:

  A `sdm_area` object. If `occ` is a `input_sdm` object with predictors
  data, than `pred` is obtained from it.

- species:

  A `character` stating the name of the column with species names in
  `occ` (see details).

- lon:

  A `character` stating the name of the column with longitude in `occ`
  (see details).

- lat:

  A `character` stating the name of the column with latitude in `occ`
  (see details).

- capitals:

  Boolean to turn on/off the exclusion from countries capitals
  coordinates (see `?cc_cap`)

- centroids:

  Boolean to turn on/off the exclusion from countries centroids
  coordinates (see `?cc_cen`)

- duplicated:

  Boolean to turn on/off the exclusion from duplicated records (see
  `?cc_dupl`)

- identical:

  Boolean to turn on/off the exclusion from records with identical
  lat/long values (see `?cc_equ`)

- institutions:

  Boolean to turn on/off the exclusion from biodiversity institutions
  coordinates (see `?cc_inst`)

- invalid:

  Boolean to turn on/off the exclusion from invalid coordinates (see
  `?cc_val`)

- terrestrial:

  Boolean to turn on/off the exclusion from coordinates falling on sea
  (see `?cc_sea`)

- independent_test:

  Boolean. If `occ` has independent test data, the data cleaning routine
  is also applied on it.

## Value

A `occurrences_sdm` object or `input_sdm` with cleaned presence data.

## Details

If the user does not used `GBIF_data` function to obtain species
records, the function may have problems to find which column from the
presences table has species, longitude and latitude information. In this
regard, we implemented the parameters `species`, `lon` and `lat` so the
use can explicitly inform which columns should be used. If they remain
as NA (standard) the function will try to guess which columns are the
correct one.

## See also

[`GBIF_data`](https://luizesser.github.io/caretSDM/reference/GBIF_data.md)` `[`occurrences_sdm`](https://luizesser.github.io/caretSDM/reference/occurrences_sdm.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)` `[`predictors`](https://luizesser.github.io/caretSDM/reference/predictor_names.md)

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
sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)

# Create input_sdm:
i <- input_sdm(oc, sa)

# Clean coordinates (terrestrial is set to false to make the run quicker):
i <- data_clean(i, terrestrial = FALSE)
#> Cell_ids identified, removing duplicated cell_id.
#> Testing country capitals
#> Removed 0 records.
#> Testing country centroids
#> Removed 0 records.
#> Testing duplicates
#> Removed 0 records.
#> Testing equal lat/lon
#> Removed 0 records.
#> Testing biodiversity institutions
#> Removed 0 records.
#> Testing coordinate validity
#> Removed 0 records.
#> Predictors identified, procceding with grid filter (removing NA and duplicated data).
```
