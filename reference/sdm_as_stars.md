# `sdm_as_X` functions to transform `caretSDM` data into other classes.

This functions transform data from a `caretSDM` object to be used in
other packages.

## Usage

``` r
sdm_as_stars(x,
             what = NULL,
             spp = NULL,
             scen = NULL,
             id = NULL,
             ens = NULL)

sdm_as_raster(x, what = NULL, spp = NULL, scen = NULL, id = NULL, ens = NULL)

sdm_as_terra(x, what = NULL, spp = NULL, scen = NULL, id = NULL, ens = NULL)
```

## Arguments

- x:

  A `caretSDM` object.

- what:

  Sometimes multiple data inside `x` could be transformed. This
  parameter allows users to specify what needs to be converted.It can be
  one of: "predictors", "scenarios", "predictions" or "ensembles".

- spp:

  `character`. Which species should be converted?

- scen:

  `character`. Which scenario should be converted?

- id:

  `character`. Which id should be converted?

- ens:

  `character`. Which ensemble should be converted?

## Value

The output is the desired class.

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
sa <- sdm_area(parana, cell_size = 100000, crs = 6933)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include predictors:
sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include scenarios:
sa <- add_scenarios(sa)

# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)

# Create input_sdm:
i <- input_sdm(oc, sa)

# Pseudoabsence generation:
i <- pseudoabsences(i, method = "random", n_set = 2)

# Custom trainControl:
ctrl_sdm <- caret::trainControl(method = "boot",
                                number = 1,
                                classProbs = TRUE,
                                returnResamp = "all",
                                summaryFunction = summary_sdm,
                                savePredictions = "all")

# Train models:
i <- train_sdm(i, algo = c("naive_bayes"), ctrl = ctrl_sdm) |>
  suppressWarnings()

# Predict models:
i  <- predict_sdm(i, th = 0.8)
#> [1] "Projecting: 1/1"
#> [1] "Ensembling..."
#> [1] "current"
#> [1] "Araucaria angustifolia"

# Transform in stars:
sdm_as_stars(i)
#> stars object with 1 dimensions and 2 attributes
#> attribute(s):
#>                       Min.    1st Qu.     Median       Mean    3rd Qu.
#> cell_id        2.000000000 10.5000000 19.0000000 18.6774194 26.5000000
#> mean_occ_prob  0.002866807  0.2308136  0.5716561  0.5970531  0.9804122
#>                     Max.
#> cell_id        35.000000
#> mean_occ_prob   0.999509
#> dimension(s):
#>          from to                       refsys point
#> geometry    1 31 WGS 84 / NSIDC EASE-Grid ... FALSE
#>                                                                 values
#> geometry POLYGON ((-5201744 -27950...,...,POLYGON ((-4701744 -31950...
```
