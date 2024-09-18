# add_predictors - rasterStack não usando sdm_area

    Code
      sa_pred <- add_predictors(pr_raster, pr_raster)
    Condition
      Error in `add_predictors()`:
      x The sa argument must be an instance of class sdm_area.

# add_predictors - lista de variáveis invalida

    Code
      sa_pred <- add_predictors(sa, pr_raster, list("foo"))
    Condition
      Warning:
      ! Some selected variables not found!
      i Variables: foo.
    Message
      ! Making grid over the study area is an expensive task. Please, be patient!
      i Using GDAL to make the grid and resample the variables.

