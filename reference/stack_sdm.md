# Train a Stacked Ensemble for SDM

This function builds a meta-model (Layer 2) using the out-of-fold
predictions from models trained in Layer 1.

## Usage

``` r
stack_sdm(m, meta_algo = "glm", ctrl = NULL, ...)
```

## Arguments

- m:

  A `models` or `input_sdm` object.

- meta_algo:

  A character string specifying the algorithm for the meta-learner.

- ctrl:

  A `trainControl` object for the meta-learner. If NULL, a simple CV is
  used.

- ...:

  Additional arguments passed to
  [`caret::train`](https://rdrr.io/pkg/caret/man/train.html).

## Value

A `stacked_models` object.

## See also

[`input_sdm`](https://luizesser.github.io/caretSDM/reference/input_sdm.md)` `[`sdm_area`](https://luizesser.github.io/caretSDM/reference/sdm_area.md)` `[`algorithms`](https://luizesser.github.io/caretSDM/reference/algorithms.md)` `[`train_sdm`](https://luizesser.github.io/caretSDM/reference/train_sdm.md)

## Author

Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com

## Examples

``` r
# Create sdm_area object:
sa <- sdm_area(parana, cell_size = 100000, output_crs = 6933)
#> ! Making grid over study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include predictors:
sa <- add_predictors(sa, bioc) |> select_predictors(c("bio1", "bio12"))
#> ! Making grid over the study area is an expensive task. Please, be patient!
#> ℹ Using GDAL to make the grid and resample the variables.

# Include scenarios:
sa <- add_scenarios(sa)

# Create occurrences:
oc <- occurrences_sdm(occ, occ_crs = 6933)

# Create input_sdm:
i <- input_sdm(oc, sa)

# Pseudoabsence generation:
i <- pseudoabsences(i, method = "random")

# Custom trainControl:
ctrl_sdm <- caret::trainControl(method = "repeatedcv",
                                number = 2,
                                repeats = 1,
                                classProbs = TRUE,
                                returnResamp = "all",
                                summaryFunction = summary_sdm,
                                savePredictions = "all")

# Train models:
i <- train_sdm(i, algo = c("naive_bayes", "kknn"), ctrl = ctrl_sdm) |>
suppressWarnings()

# Train stacked ensemble:
i <- stack_sdm(i, meta_algo = "nnet", ctrl = ctrl_sdm)
#> Training meta-learner for: Araucaria angustifolia
#> # weights:  23
#> initial  value 108.600079 
#> iter  10 value 55.045355
#> final  value 55.039924 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  67
#> initial  value 103.758247 
#> iter  10 value 38.833769
#> iter  20 value 31.293827
#> iter  30 value 29.338925
#> iter  40 value 27.052904
#> iter  50 value 26.580397
#> iter  60 value 26.252377
#> iter  70 value 26.222419
#> iter  80 value 26.135457
#> iter  90 value 26.008467
#> iter 100 value 25.764117
#> final  value 25.764117 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 135.897037 
#> final  value 55.039699 
#> converged
#> # weights:  23
#> initial  value 258.560283 
#> iter  10 value 57.099883
#> iter  20 value 46.856021
#> iter  30 value 44.722845
#> iter  40 value 43.725217
#> iter  50 value 43.477366
#> final  value 43.477364 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  67
#> initial  value 95.448489 
#> iter  10 value 47.398741
#> iter  20 value 43.227570
#> iter  30 value 42.315806
#> iter  40 value 42.304203
#> iter  50 value 42.287337
#> final  value 42.287035 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 80.317922 
#> iter  10 value 45.974185
#> iter  20 value 42.579221
#> iter  30 value 42.256052
#> iter  40 value 42.222074
#> iter  50 value 42.168230
#> iter  60 value 42.117418
#> iter  70 value 42.087948
#> iter  80 value 42.073391
#> iter  90 value 42.072704
#> iter 100 value 42.072628
#> final  value 42.072628 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  23
#> initial  value 140.835811 
#> final  value 55.042684 
#> converged
#> # weights:  67
#> initial  value 125.265962 
#> iter  10 value 55.129110
#> iter  20 value 55.048698
#> iter  30 value 54.998468
#> iter  40 value 54.825528
#> iter  50 value 54.806023
#> iter  60 value 54.794748
#> iter  70 value 54.781229
#> iter  80 value 54.717750
#> iter  90 value 54.592264
#> iter 100 value 36.117110
#> final  value 36.117110 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 234.989744 
#> iter  10 value 55.752340
#> iter  20 value 55.033089
#> iter  30 value 38.874448
#> iter  40 value 32.078827
#> iter  50 value 27.314119
#> iter  60 value 22.560794
#> iter  70 value 21.180326
#> iter  80 value 20.401282
#> iter  90 value 19.930483
#> iter 100 value 19.646656
#> final  value 19.646656 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  23
#> initial  value 108.591033 
#> iter  10 value 57.784692
#> iter  20 value 57.394921
#> iter  30 value 52.381264
#> iter  40 value 52.187220
#> iter  50 value 31.325745
#> iter  60 value 30.940868
#> iter  70 value 30.926010
#> iter  80 value 30.923787
#> final  value 30.923779 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  67
#> initial  value 126.467987 
#> iter  10 value 57.787707
#> iter  20 value 57.786981
#> final  value 57.786909 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 267.692038 
#> iter  10 value 57.833302
#> iter  20 value 57.787367
#> final  value 57.786908 
#> converged
#> # weights:  23
#> initial  value 143.797519 
#> iter  10 value 42.232616
#> iter  20 value 40.811571
#> final  value 40.811522 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  67
#> initial  value 116.955445 
#> iter  10 value 40.437078
#> iter  20 value 38.727866
#> iter  30 value 38.425141
#> iter  40 value 38.384121
#> iter  50 value 38.362436
#> iter  60 value 38.360708
#> final  value 38.360707 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 115.607771 
#> iter  10 value 41.606041
#> iter  20 value 38.505495
#> iter  30 value 38.201256
#> iter  40 value 38.132549
#> iter  50 value 38.091982
#> iter  60 value 38.087735
#> final  value 38.087695 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  23
#> initial  value 185.094579 
#> final  value 57.793863 
#> converged
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  67
#> initial  value 206.103482 
#> iter  10 value 55.825787
#> iter  20 value 31.165728
#> iter  30 value 28.428164
#> iter  40 value 27.640815
#> iter  50 value 27.616391
#> iter  60 value 27.605841
#> iter  70 value 27.592002
#> iter  80 value 27.582631
#> iter  90 value 27.513011
#> iter 100 value 26.769151
#> final  value 26.769151 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> # weights:  111
#> initial  value 163.378546 
#> iter  10 value 38.980465
#> iter  20 value 27.121334
#> iter  30 value 22.905095
#> iter  40 value 22.237791
#> iter  50 value 20.578573
#> iter  60 value 15.927004
#> iter  70 value 14.763404
#> iter  80 value 14.126781
#> iter  90 value 13.278670
#> iter 100 value 11.773852
#> final  value 11.773852 
#> stopped after 100 iterations
#> Warning: Partial AUC correction not defined for ROC curves below the diagonal.
#> Warning: There were missing values in resampled performance measures.
#> # weights:  23
#> initial  value 402.840494 
#> iter  10 value 113.346280
#> iter  20 value 82.466967
#> iter  30 value 81.207313
#> final  value 81.202395 
#> converged
```
