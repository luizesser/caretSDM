# Obtain Pseudoabsences

This function obtains pseudoabsences given a set of predictors.

## Usage

``` r
pseudoabsences(occ,
               pred = NULL,
               method = "random",
               n_set = 10,
               n_pa = NULL,
               variables_selected = NULL,
               th = 0,
               size = 1,
               crs = 4326)

n_pseudoabsences(i)

pseudoabsence_method(i)

pseudoabsence_data(i)
```

## Arguments

- occ:

  A `occurrences_sdm` or `input_sdm` object.

- pred:

  A `sdm_area` object. If `NULL` and `occ` is a `input_sdm`, `pred` will
  be retrieved from `occ`.

- method:

  Method to create pseudoabsences. One of: "random", "bioclim",
  "mahal.dist" or "buffer_sdm". User can also provide a custom function
  (see details).

- n_set:

  `numeric`. Number of datasets of pseudoabsence to create.

- n_pa:

  `numeric`. Number of pseudoabsences to be generated in each dataset
  created. If `NULL` then the function prevents imbalance by using the
  same number of presence records (`n_records(occ)`). If you want to
  address different sizes to each species, you must provide a named
  vector (as in `n_records(occ)`).

- variables_selected:

  A vector with variables names to be used while building
  pseudoabsences. Only used when method is not "random".

- th:

  `numeric` Threshold to be applied in bioclim/mahal.dist projections.
  See details.

- size:

  `numeric` The distance between the record and the margin of the buffer
  (i.e. buffer radius).

- crs:

  `numeric` Indicates which EPSG it the size in.

- i:

  A `input_sdm` object.

## Value

A `occurrences_sdm` or `input_sdm` object with pseudoabsence data.

## Details

`pseudoabsences` is used in the SDM workflow to obtain pseudoabsences, a
step necessary for most of the algorithms to run. We implemented three
methods so far: `"random"`, which is self-explanatory, `"bioclim"`,
`"mahal.dist"` and `"buffer_sdm"`. The two last are built with the idea
that pseudoabsences should be environmentally different from presences.
Thus, we implemented two presence-only methods to infer the distribution
of the species. `"bioclim"` uses an envelope approach (bioclimatic
envelope), while `"mahal.dist"` uses a distance approach (mahalanobis
distance). `th` parameter enters here as a threshold to binarize those
results. Pseudoabsences are retrieved outside the projected distribution
of the species. If user provides a custom function, it must have the
arguments `env_sf` and `occ_sf`, which will consist of two `"sf"`s. The
first has the predictor values for the whole study area, while the
second has the presence records for the species. The function must
return a vector with cell_ids of the pseudoabsences.

`n_pseudoabsences` returns the number of pseudoabsences obtained per
species.

`pseudoabsence_method` returns the method used to obtain pseudoabsences.

`pseudoabsence_data` returns a `list` of species names. Each species
name will have a `list`s with pseudoabsences data from class `sf`.

## See also

`link{input_sdm} `[`background`](https://luizesser.github.io/caretSDM/reference/background.md)` `[`occurrences_sdm`](https://luizesser.github.io/caretSDM/reference/occurrences_sdm.md)` `[`get_occurrences`](https://luizesser.github.io/caretSDM/reference/occurrences_sdm.md)` `[`get_predictors`](https://luizesser.github.io/caretSDM/reference/add_predictors.md)` `

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
oc <- occurrences_sdm(occ[1:50,], crs = 6933)

# Create input_sdm:
i <- input_sdm(oc, sa)

# Pseudoabsence generation:
i <- pseudoabsences(i, method="random")

# Custom method example:
env_distance_pa <- function(env_sf, occ_sf, n_pa=n_pa) {

  # Transform the environmental data to a data frame and remove the cell_id and geometry columns
  df <- as.data.frame(env_sf)[, -which(names(env_sf) %in% c("cell_id", "geometry"))]

  # Define the center of the environmental space based on the mean of the predictors
  center <- colMeans(df)

  # Calculate the distance of each cell in the environmental space to the center
  d <- sqrt(rowSums((df - center)^2))

  # Convert distances to probabilities (the closer to the center, the higher the probability)
  p <- d / sum(d)

  # Sample pseudo-absences based on the calculated probabilities
  sample(env_sf$cell_id, size = n_pa, prob = p)

}

i <- pseudoabsences(i, method=env_distance_pa)
#> → Previous pseudoabsence element on Occurrences object will be overwritten.
```
