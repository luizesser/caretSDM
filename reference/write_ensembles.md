# Write caretSDM data

This function exports caretSDM data.

## Usage

``` r
write_ensembles(x, path = NULL, ext = ".tif", centroid = FALSE)

write_predictions(x, path = NULL, ext = ".tif", centroid = FALSE)

write_predictors(x, path = NULL, ext = ".tif", centroid = FALSE)

write_models(x, path = NULL)

write_gpkg(x, file_path, file_name)

# S3 method for class 'sdm_area'
write_gpkg(x, file_path, file_name)

write_occurrences(x, path = NULL, grid = FALSE, ...)

write_pseudoabsences(x, path = NULL, ext = ".csv", centroid = FALSE)

write_grid(x, path = NULL, centroid = FALSE)

write_validation_metrics(x, path = NULL)
```

## Arguments

- x:

  Object to be written. Can be of class `input_sdm`, `occurrences`,
  `predictions` or `models`.

- path:

  A path with filename and the proper extension (see details) or the
  directory to save files in.

- ext:

  How it should be saved?

- centroid:

  Should coordinates for the centroids of each cell be included?
  Standard is FALSE.

- file_path:

  A path to save the `sdm_area` GeoPackage file.

- file_name:

  The name of the `sdm_area` GeoPackage file to be saved without
  extension.

- grid:

  Boolean. Return a grid.

- ...:

  Arguments to pass to
  [`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html)
  or `write.csv`.

## Value

No return value, called for side effects.

## Details

`ext` can be set accordingly to the desired output. Possible values are
.tif and .asc for rasters, .csv for for a spreadsheet, but also one of:
c("bna", "csv", "e00", "gdb", "geojson", "gml", "gmt", "gpkg", "gps",
"gtm", "gxt", "jml", "map", "mdb", "nc", "ods", "osm", "pbf", "shp",
"sqlite", "vdv", "xls", "xlsx"). `path` ideally should only provide the
folder. We recommend using: `results/what_are_you_writting`. So for
writting ensembles users are advised to run:
`path = "results/ensembles"`

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com
