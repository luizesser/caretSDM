# Calculate VIF

Apply Variance Inflation Factor (VIF) calculation.

## Usage

``` r
vif_predictors(pred, area = "all", th = 0.5, maxobservations = 5000, variables_selected =
NULL)

vif_summary(i)
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
sa <- sdm_area(parana, cell_size = 25000, output_crs = 6933)
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
oc <- occurrences_sdm(occ, occ_crs = 6933)

# Create input_sdm:
i <- input_sdm(oc, sa)
#> Warning: Some records from `occ` do not fall in `pred`.
#> ℹ 2 elements from `occ` were excluded.
#> ℹ If this seems too much, check how `occ` and `pred` intersect.

# VIF calculation:
i <- vif_predictors(i)
i
#>              caretSDM           
#> ................................
#> Class                          : input_sdm
#> 
#> =========== Overview ===========
#> Focal Taxon                    : Araucaria angustifolia 
#> Spatial extent                 : -5276744.44724281, -3295036.62222337, -4626744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
#> Temporal extent (inferred)     : 2090 - 2090 
#> Observation type               : Presence-only 
#> Predictor names                : bio1, bio4, bio12 
#> Software                       : caretSDM v1.9.6, R version 4.6.0 (2026-04-24)
#> 
#> ============= Data =============
#> -- Biodiversity data --
#> Taxon names                    : Araucaria angustifolia 
#> Sample size                    : 417 
#> -- Predictor variables --
#> Number of predictors           : 3 
#> Predictor names                : bio1, bio4, bio12 
#> Spatial extent                 : -5276744.44724281, -3295036.62222337, -4626744.44724281, -2795036.62222337  (xmin,xmax,ymin,ymax)
#> Spatial resolution             : (25000, 25000) 
#> Coordinate reference system    : WGS 84 / NSIDC EASE- ( EPSG: 6933 ) 
#> -- Transfer data --
#> Number of scenarios            : 5 
#> Scenario names                 : ca_ssp245_2090, ca_ssp585_2090, mi_ssp245_2090, mi_ssp585_2090, current 
#> Temporal extent (inferred)     : 2090 - 2090 
#> 
#> ============= Model ============
#> -- Multicollinearity --
#> Variable selection method      : vif 
#> Selected variables             : bio1, bio4 
#> 
#> ========== Assessment ==========

# Retrieve information about vif:
vif_summary(i)
#> 1 variables from the 3 input variables have collinearity problem: 
#>  
#> bio12 
#> 
#> After excluding the collinear variables, the linear correlation coefficients ranges between: 
#> min correlation ( bio4 ~ bio1 ):  -0.3182326 
#> max correlation ( bio4 ~ bio1 ):  -0.3182326 
#> 
#> ---------- VIFs of the remained variables -------- 
#>   Variables      VIF
#> 1      bio1 1.112684
#> 2      bio4 1.112684
selected_variables(i)
#> [1] "bio1" "bio4"
```
