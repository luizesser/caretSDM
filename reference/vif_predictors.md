# Calculate VIF

Apply Variance Inflation Factor (VIF) calculation.

## Usage

``` r
vif_predictors(pred, area = "all", th = 0.5, maxobservations = 5000, variables_selected =
NULL)

vif_summary(i)

selected_variables(i)
```

## Arguments

- pred:

  A `input_sdm` or `predictors` object.

- area:

  Character. Which area should be used in vif selection? Standard is
  `"all"`.

- th:

  Threshold to be applied in VIF routine. See ?usdm::vifcor.

- maxobservations:

  Max observations to use to calculate the VIF.

- variables_selected:

  If there is a subset of predictors that should be used in this
  function, it can be informed using this parameter. If set to `NULL`
  (standard) all variables are used.

- i:

  A `input_sdm` to retrieve information from.

## Value

A `input_sdm` or `predictors` object with VIF data.

## Details

vif_predictors is a wrapper function to run usdm::vifcor in caretSDM.

## See also

[`get_predictor_names`](https://luizesser.github.io/caretSDM/reference/predictor_names.md)

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
oc <- occurrences_sdm(occ, crs = 6933) |> join_area(sa)
#> Warning: Some records from `occ` do not fall in `pred`.
#> ℹ 2 elements from `occ` were excluded.
#> ℹ If this seems too much, check how `occ` and `pred` intersect.

# Create input_sdm:
i <- input_sdm(oc, sa)

# VIF calculation:
i <- vif_predictors(i)
i
#>             caretSDM           
#> ...............................
#> Class                         : input_sdm
#> --------  Occurrences  --------
#> Species Names                 : Araucaria angustifolia 
#> Number of presences           : 418 
#> --------  Predictors  ---------
#> Number of Predictors          : 3 
#> Predictors Names              : bio1, bio4, bio12 
#> Area (VIF)                    : all
#> Threshold                     : 0.5
#> Selected Variables (VIF)      : bio1, bio12 
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
```
