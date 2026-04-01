# Multicollinearity Analysis

Apply multicollinearity calculation on predictors.

## Usage

``` r
multicollinearity_sdm(pred,
                             method = NULL,
                             variables_selected = NULL,
                             cumulative_proportion = 0.99,
                             th = 0.5,
                             ...)

selected_variables(i)
```

## Arguments

- pred:

  A `input_sdm` or `predictors` object.

- method:

  Which method should be used to detect multicollinearity. Can be a
  `character` or a custom `function`.

- variables_selected:

  A vector with pre-selected variables names to filter variables.

- cumulative_proportion:

  A `numeric` with the threshold for cumulative proportion in PCA.
  Standard is 0.99, meaning that axes returned as predictors sum up more
  than 99 environmental variance.

- th:

  Threshold to be applied in VIF routine. See ?usdm::vifcor.

- ...:

  Further arguments to be passed to the applied method.

- i:

  A `input_sdm` object.

## Value

A `input_sdm` or `predictors` object with VIF data.

## Details

multicollinearity_sdm is a wrapper function to run usdm::vifcor,
usdm::vifstep or a pca in caretSDM, but also provides a way to implement
custom functions to reduce multicollinearity. If user provides a custom
function, it must have the arguments `env_sf` and `occ_sf`, which will
consist of two `sf`s. The first has the predictor values for the whole
study area, while the second has the presence records for the species.
The function must return a vector with selected variables.

## See also

[`vif_predictors`](https://luizesser.github.io/caretSDM/reference/vif_predictors.md)` `[`pca_predictors`](https://luizesser.github.io/caretSDM/reference/pca_predictors.md)` `[`get_predictor_names`](https://luizesser.github.io/caretSDM/reference/predictor_names.md)

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
sa <- add_scenarios(sa, scen)
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Create occurrences:
oc <- occurrences_sdm(occ, crs = 6933)

# Create input_sdm:
i <- input_sdm(oc, sa)
#> Warning: Some records from `occ` do not fall in `pred`.
#> ℹ 2 elements from `occ` were excluded.
#> ℹ If this seems too much, check how `occ` and `pred` intersect.

# VIF calculation:
i <- multicollinearity_sdm(i, method = "vifcor", th = 0.5)
i
#>             caretSDM           
#> ...............................
#> Class                         : input_sdm
#> --------  Occurrences  --------
#> Species Names                 : Araucaria angustifolia 
#> Number of presences           : 417 
#> --------  Predictors  ---------
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Variable Selection            : vif 
#> Selected Variables            : bio1, bio12 
#> ---------  Scenarios  ---------
#> Number of Scenarios           : 5 
#> Scenarios Names               : ca_ssp245_2090 ca_ssp585_2090 mi_ssp245_2090 mi_ssp585_2090 current 

# Retrieve information about vif:
vif_summary(i)
#> 1 variables from the 3 input variables have collinearity problem: 
#>  
#> bio4 
#> 
#> After excluding the collinear variables, the linear correlation coefficients ranges between: 
#> min correlation ( bio12 ~ bio1 ):  -0.3350609 
#> max correlation ( bio12 ~ bio1 ):  -0.3350609 
#> 
#> ---------- VIFs of the remained variables -------- 
#>   Variables      VIF
#> 1      bio1 1.126463
#> 2     bio12 1.126463
selected_variables(i)
#> [1] "bio1"  "bio12"

# Example of custom function:
custom_function <- function(env_sf, occ_sf) {
  env_df <- dplyr::select(sf::st_drop_geometry(env_sf), -"cell_id")
  correlations <- cor(env_df)
  col <- caret::findCorrelation(correlations, cutoff = 0.7)
  selected <- colnames(correlations)[-col]
  return(selected)
}
```
