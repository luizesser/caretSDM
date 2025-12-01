# S3 Methods for plot and mapview

This function creates different plots depending on the input.

## Usage

``` r
plot_occurrences(i, spp_name = NULL, pa = TRUE, pa_id = 1)

plot_grid(i)

plot_predictors(i, variables_selected = NULL)

plot_scenarios(i, variables_selected = NULL, scenario = NULL)

plot_predictions(
  i,
  spp_name = NULL,
  scenario = NULL,
  id = NULL,
  ensemble = TRUE,
  ensemble_type = "mean_occ_prob"
)

mapview_grid(i)

mapview_occurrences(i, spp_name = NULL, pa = TRUE)

mapview_predictors(i, variables_selected = NULL)

mapview_scenarios(i, variables_selected = NULL, scenario = NULL)

mapview_predictions(
  i,
  spp_name = NULL,
  scenario = NULL,
  id = NULL,
  ensemble = TRUE,
  ensemble_type = "mean_occ_prob"
)

plot_background(i, variables_selected = NULL)

plot_niche(
  i,
  spp_name = NULL,
  variables_selected = NULL,
  scenario = NULL,
  id = NULL,
  ensemble = TRUE,
  ensemble_type = "mean_occ_prob",
  raster = FALSE
)
```

## Arguments

- i:

  Object to be plotted. Can be a `input_sdm`, but also `occurrences` or
  `sdm_area`.

- spp_name:

  A character with species to be plotted. If NULL, the first species is
  plotted.

- pa:

  Boolean. Should pseudoabsences be plotted together? (not implemented
  yet.)

- pa_id:

  The id of pseudoabsences to be plotted (only used when `pa = TRUE`).
  Possible values are numeric values from 1 to number of PA sets.

- variables_selected:

  A character vector with names of variables to be plotted.

- scenario:

  description

- id:

  The id of models to be plotted (only used when `ensemble = FALSE`).
  Possible values are row names of get_validation_metrics(i).

- ensemble:

  Boolean. Should the ensemble be plotted (TRUE)? Otherwise a prediction
  will be plotted

- ensemble_type:

  Character of the type of ensemble to be plotted. One of:
  "mean_occ_prob", "wmean_AUC" or "committee_avg"

- raster:

  Should the niche be extrapolated to a raster covering all possibe
  values in the environmental space?

## Value

The plot or mapview desired.

## Details

We implemented a bestiary of plots to help visualizing the process and
results. If you are not familiar with mapview, consider using it to
better visualize maps.

## See also

[`WorldClim_data`](https://luizesser.github.io/caretSDM/reference/WorldClim_data.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com
